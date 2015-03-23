////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              Definição de classes para a versão 1.03                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type
  TCTe = class;
  TInfCTe = class;
  TIde = class;
  TToma03 = class;
  TToma4 = class;
  TEnderToma = class;
  TCompl = class;
  TFluxo = class;
  TPassCollection = class;
  TPassCollectionItem = class;
  TEntrega = class;
  TSemData = class;
  TComData = class;
  TNoPeriodo = class;
  TSemHora = class;
  TComHora = class;
  TNoInter = class;
  TObsContCollection = class;
  TObsContCollectionItem = class;
  TObsFiscoCollection = class;
  TObsFiscoCollectionItem = class;
  TEmit = class;
  TEnderEmit = class;
  TRem = class;
  TEnderReme = class;
  TInfNFCollection = class;
  TInfNFCollectionItem = class;
  TLocRet = class;
  TInfNFECollection = class;
  TInfNFECollectionItem = class;
  TInfOutrosCollection  = class;
  TInfOutrosCollectionItem = class;
  TExped = class;
  TEnderExped = class;
  TReceb = class;
  TEnderReceb = class;
  TDest = class;
  TEnderDest = class;
  TLocEnt = class;
  TvPrest = class;
  TCompCollection = class;
  TCompCollectionItem = class;
  TImp = class;
  TICMS = class;
  TCST00 = class;
  TCST20 = class;
  TCST45 = class;
  TCST60 = class;
  TCST80 = class;
  TCST81 = class;
  TCST90 = class;
  TICMSOutraUF = class;
  TICMSSN = class;

  TInfCTeNorm = class;
  TInfCarga = class;
  TInfQCollection = class;
  TInfQCollectionItem = class;
  TInfcontQtCollection = class;
  TInfcontQtCollectionItem = class;
  TlacContQtCollection = class;
  TlacContQtCollectionItem = class;
  TemiDocAntCollection = class;
  TemiDocAntCollectionItem = class;
  TidDocAntCollection = class;
  TidDocAntCollectionItem = class;
  TidDocAntPapCollection = class;
  TidDocAntPapCollectionItem = class;
  TidDocAntEleCollection = class;
  TidDocAntEleCollectionItem = class;
  TInfSegCollection = class;
  TInfSegCollectionItem = class;

  TRodo = class; // Informações do modal Rodoviário
  TCTRB = class;
  TOccCollection = class;
  TOccCollectionItem = class;
  TEmiOCC = class;
  TValePed = class;
  TDispCollection = class;
  TDispCollectionItem = class;
  TVeicCollection = class;
  TVeicCollectionItem = class;
  Tprop = class;
  TLacresCollection = class;
  TLacresCollectionItem = class;
  TMotoCollection = class;
  TMotoCollectionItem = class;

  Taereo = class; // Informações do modal Aéreo
  Ttarifa = class;
  TnatCarga = class;

  Taquav = class; // Informações do modal Aquaviário
  TbalsaCollection = class;
  TbalsaCollectionItem = class;
  TLacreCollection = class;
  TLacreCollectionItem = class;

  Tferrov = class; // Informações do modal Ferroviário
  TtrafMut = class;
  TferroSub = class;
  TEnderFerro = class;
  TDCLCollection = class;
  TDCLCollectionItem = class;

  TdetVagDCLCollection = class;
  TdetVagDCLCollectionItem = class;
  TlacDetVagDCLCollection = class;
  TlacDetVagDCLCollectionItem = class;
  TcontDCLCollection = class;
  TcontDCLCollectionItem = class;

  TdetVagCollection = class;
  TdetVagCollectionItem = class;
  TlacDetVagCollection = class;
  TlacDetVagCollectionItem = class;
  TcontVagCollection = class;
  TcontVagCollectionItem = class;

  Tduto = class; // Informações do modal Dutoviário

  TperiCollection = class; // Informações de produtos classificados pela ONU como perigosos
  TperiCollectionItem = class;

  TveicNovosCollection = class; // Informações dos veículos transportados
  TveicNovosCollectionItem = class;

  TCobr = class; // Informações dos Dados da cobrança do CT-e
  TFat = class;
  TDupCollection = class;
  TDupCollectionItem = class;

  TrefNF = class;
  TtomaICMS = class;
  TtomaNaoICMS = class;
  TinfCTeSub = class; // Informações do CT-e de substituição
  TinfCTeCompCollection = class;
  TinfCTeCompCollectionItem = class;
  TvPresComp = class;
  TcompCompCollection = class;
  TcompCompCollectionItem = class;
  TImpComp = class;
  TICMSComp = class;
  TInfCTeAnuEnt = class;

  TCTe = class(TPersistent)
  private
    FSchema     : TpcnSchema;
    FinfCTe     : TinfCTe;
    FIde        : TIde;
    FCompl      : TCompl;
    FEmit       : TEmit;
    FRem        : TRem;
    FExped      : TExped;
    FReceb      : TReceb;
    FDest       : TDest;
    FvPrest     : TvPrest;
    FImp        : TImp;
    FInfCTeNorm : TInfCteNorm;
    FInfCarga   : TInfCarga;
    FInfSeg     : TInfSegCollection;

    FRodo       : TRodo;   // Informações do modal Rodoviário
    Faereo      : Taereo;  // Informações do modal Aéreo
    Faquav      : Taquav;  // Informações do modal Aquaviário
    Fferrov     : Tferrov; // Informações do modal Ferroviário
    Fduto       : Tduto;   // Informações do modal Dutoviário

    Fperi       : TperiCollection;
    FveicNovos  : TveicNovosCollection;
    FCobr       : TCobr;
    FinfCTeSub  : TinfCTeSub;
    FinfCTeComp : TinfCTeCompCollection;

    FInfCTeAnuEnt : TInfCTeAnuEnt;
    FProcCTe: TProcCTe;
    FSignature: TSignature;
    procedure Setperi(Value: TperiCollection);
    procedure SetveicNovos(Value: TveicNovosCollection);
    procedure SetInfSeg(Value: TInfSegCollection);
    procedure SetInfCTeComp(Value: TInfCTeCompCollection);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property schema: TpcnSchema read Fschema write Fschema;
    property infCTe: TinfCTe read FinfCTe write FinfCTe;
    property Ide: TIde read FIde write FIde;
    property Compl: TCompl read FCompl write FCompl;
    property Emit: TEmit read FEmit write FEmit;
    property Rem: TRem read FRem write FRem;
    property Exped: TExped read FExped write FExped;
    property Receb: TReceb read FReceb write FReceb;
    property Dest: TDest read FDest write FDest;
    property vPrest: TvPrest read FvPrest write FvPrest;
    property Imp: TImp read FImp write FImp;
    property infCTeNorm: TInfCTeNorm read FInfCteNorm write FInfCTeNorm;
    property infCarga: TInfCarga read FInfCarga write FInfCarga;
    property infSeg: TInfSegCollection read FInfSeg write SetInfSeg;

    property Rodo: TRodo read FRodo write FRodo;
    property Aereo: Taereo read Faereo write Faereo;
    property Aquav: Taquav read Faquav write Faquav;
    property Ferrov: Tferrov read Fferrov write Fferrov;
    property duto: Tduto read Fduto write Fduto;

    property peri: TperiCollection read Fperi write Setperi;
    property veicNovos: TveicNovosCollection read FveicNovos write SetveicNovos;
    property cobr: TCobr read FCobr write FCobr;
    property infCTeSub: TinfCTeSub read FinfCTeSub write FinfCTeSub;
    property InfCTeComp: TInfCTeCompCollection read FInfCTeComp write SetInfCTeComp;
    property InfCTeAnuEnt: TInfCTeAnuEnt read FInfCTeAnuEnt write FInfCTeAnuEnt;
    property procCTe: TProcCTe read FProcCTe write FProcCTe;
    property signature: Tsignature read Fsignature write Fsignature;
  end;

  TinfCTe = class(TPersistent)
  private
    FID: string;
  published
    property ID: string read FID write FID;
  end;

  TIde = class(TPersistent)
  private
    FcUF: integer;
    FcCT: integer;
    FCFOP: integer;
    FnatOp : String;
    FforPag : TpCTeFormaPagamento;
    Fmod: String;
    Fserie: integer;
    FnCT: integer;
    FdhEmi: TDateTime;
    FtpImp: TpcnTipoImpressao;
    FtpEmis: TpcnTipoEmissao;
    FcDV: integer;
    FtpAmb: TpcnTipoAmbiente;
    FtpCTe: TpcteTipoCTe;
    FprocEmi: TpcnProcessoEmissao;
    FverProc: String;
    FrefCTe: String;
    FcMunEmi: integer;
    FxMunEmi: String;
    FUFEmi: String;
    Fmodal: TpcteModal;
    FtpServ: TpcteTipoServico;
    FcMunIni: integer;
    FxMunIni: String;
    FUFIni: String;
    FcMunFim: integer;
    FxMunFim: String;
    FUFFim: String;
    Fretira: TpcteRetira;
    Fxdetretira: String;
    FToma03: TToma03;
    FToma4: TToma4;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property cUF: integer read FcUF write FcUF;
    property cCT: integer read FcCT write FcCT;
    property CFOP: integer read FCFOP write FCFOP;
    property natOp : String read FnatOp write FnatOp;
    property forPag : TpcteFormaPagamento read FforPag write FforPag;
    property modelo: String read Fmod write Fmod;
    property serie: integer read Fserie write Fserie;
    property nCT: integer read FnCT write FnCT;
    property dhEmi: TDateTime read FdhEmi write FdhEmi;
    property tpImp: TpcnTipoImpressao read FtpImp write FtpImp;
    property tpEmis: TpcnTipoEmissao read FtpEmis write FtpEmis;
    property cDV: integer read FcDV write FcDV;
    property tpAmb: TpcnTipoAmbiente read FtpAmb write FtpAmb;
    property tpCTe: TpcteTipoCTe read FtpCTe write FtpCTe;
    property procEmi: TpcnProcessoEmissao read FprocEmi write FprocEmi;
    property verProc: String read FverProc write FverProc;
    property refCTe: String read FrefCTe write FrefCTe;
    property cMunEmi: integer read FcMunEmi write FcMunEmi;
    property xMunEmi: String read FxMunEmi write FxMunEmi;
    property UFEmi: String read FUFEmi write FUFEmi;
    property modal: TpcteModal read Fmodal write Fmodal;
    property tpServ: TpcteTipoServico read FtpServ write FtpServ;
    property cMunIni: integer read FcMunIni write FcMunIni;
    property xMunIni: String read FxMunIni write FxMunIni;
    property UFIni: String read FUFIni write FUFIni;
    property cMunFim: integer read FcMunFim write FcMunFim;
    property xMunFim: String read FxMunFim write FxMunFim;
    property UFFim: String read FUFFim write FUFFim;
    property retira: TpcteRetira read Fretira write Fretira;
    property xDetRetira: String read Fxdetretira write Fxdetretira;
    property toma03: TToma03 read FToma03 write FToma03;
    property toma4: TToma4 read FToma4 write FToma4;
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
    FEnderToma : TEnderToma;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property toma: TpcteTomador read Ftoma write Ftoma;
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property IE: String read FIE write FIE;
    property xNome: String read FxNome write FxNome;
    property xFant: String read FxFant write FxFant;
    property fone : String read Ffone write Ffone;
    property enderToma: TEnderToma read FEnderToma write FEnderToma;
  end;

  TEnderToma = class(TPersistent)
  private
    FxLgr    : string;
    Fnro     : string;
    FxCpl    : string;
    FxBairro : string;
    FcMun    : integer;
    FxMun    : string;
    FCEP     : integer;
    FUF      : string;
    FcPais   : integer;
    FxPais   : string;
  published
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property CEP: integer read FCEP write FCEP;
    property UF: string read FUF write FUF;
    property cPais: integer read FcPais write FcPais;
    property xPais: string read FxPais write FxPais;
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
    FObsCont   : TObsContCollection;
    FObsFisco  : TObsFiscoCollection;
    procedure SetObsCont(Value: TObsContCollection);
    procedure SetObsFisco(Value: TObsFiscoCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property xCaracAd : String read FxCaracAd write FxCaracAd;
    property xCaracSer : String read FxCaracSer write FxCaracSer;
    property xEmi : String read FxEmi write FxEmi;
    property fluxo : TFluxo read Ffluxo write Ffluxo;
    property Entrega : TEntrega read FEntrega write FEntrega;
    property origCalc : String read ForigCalc write ForigCalc;
    property destCalc : String read FdestCalc write FdestCalc;
    property xObs : String read FxObs write FxObs;
    property ObsCont: TObsContCollection read FObsCont write SetObsCont;
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
    property xOrig : String read FxOrig write FxOrig;
    property pass: TPassCollection read Fpass write SetPass;
    property xDest : String read FxDest write FxDest;
    property xRota : String read FxRota write FxRota;
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
    property xPass: string read FxPass write FxPass;
  end;

  TEntrega = class(TPersistent)
  private
    FTipoData  : TpcteTipoDataPeriodo;
    FTipoHora  : TpcteTipoHorarioIntervalo;

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
    property TipoData : TpcteTipoDataPeriodo read FTipoData write FTipoData;
    property TipoHora : TpcteTipoHorarioIntervalo read FTipoHora write FTipoHora;

    property semData : TSemData read FsemData write FsemData;
    property comData : TComData read FcomData write FcomData;
    property noPeriodo : TNoPeriodo read FnoPeriodo write FnoPeriodo;
    property semHora : TSemHora read FsemHora write FsemHora;
    property comHora : TComHora read FcomHora write FcomHora;
    property noInter : TNoInter read FnoInter write FnoInter;
  end;

  TSemData = class(TPersistent)
  private
   FtpPer : TpcteTipoDataPeriodo;
  published
    property tpPer : TpcteTipoDataPeriodo read FtpPer write FtpPer;
  end;

  TComData = class(TPersistent)
  private
   FtpPer : TpcteTipoDataPeriodo;
   FdProg : TDateTime;
  published
    property tpPer : TpcteTipoDataPeriodo read FtpPer write FtpPer;
    property dProg : TDateTime read FdProg write FdProg;
  end;

  TNoPeriodo = class(TPersistent)
  private
   FtpPer : TpcteTipoDataPeriodo;
   FdIni  : TDateTime;
   FdFim  : TDateTime;
  published
    property tpPer : TpcteTipoDataPeriodo read FtpPer write FtpPer;
    property dIni : TDateTime read FdIni write FdIni;
    property dFim : TDateTime read FdFim write FdFim;
  end;

  TSemHora = class(TPersistent)
  private
   FtpHor : TpcteTipoHorarioIntervalo;
  published
    property tpHor : TpcteTipoHorarioIntervalo read FtpHor write FtpHor;
  end;

  TComHora = class(TPersistent)
  private
   FtpHor : TpcteTipoHorarioIntervalo;
   FhProg : TDateTime;
  published
    property tpHor : TpcteTipoHorarioIntervalo read FtpHor write FtpHor;
    property hProg : TDateTime read FhProg write FhProg;
  end;

  TNoInter = class(TPersistent)
  private
   FtpHor : TpcteTipoHorarioIntervalo;
   FhIni  : TDateTime;
   FhFim  : TDateTime;
  published
    property tpHor : TpcteTipoHorarioIntervalo read FtpHor write FtpHor;
    property hIni : TDateTime read FhIni write FhIni;
    property hFim : TDateTime read FhFim write FhFim;
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
    property xCampo: string read FxCampo write FxCampo;
    property xTexto: string read FxTexto write FxTexto;
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
    property xCampo: string read FxCampo write FxCampo;
    property xTexto: string read FxTexto write FxTexto;
  end;

  TEmit = class(TPersistent)
  private
    FCNPJ  : String;
    FIE    : String;
    FxNome : String;
    FxFant : String;
    FEnderEmit : TEnderEmit;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property CNPJ : String read FCNPJ write FCNPJ;
    property IE : String read FIE write FIE;
    property xNome : String read FxNome write FxNome;
    property xFant : String read FxFant write FxFant;
    property enderEmit: TEnderEmit read FEnderEmit write FEnderEmit;
  end;

  TEnderEmit = class(TPersistent)
  private
    FxLgr    : string;
    Fnro     : string;
    FxCpl    : string;
    FxBairro : string;
    FcMun    : integer;
    FxMun    : string;
    FCEP     : integer;
    FUF      : string;
    FcPais   : integer;
    FxPais   : string;
    Ffone    : String;
  published
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property CEP: integer read FCEP write FCEP;
    property UF: string read FUF write FUF;
    property cPais: integer read FcPais write FcPais;
    property xPais: string read FxPais write FxPais;
    property fone: String read Ffone write Ffone;
  end;

  TRem = class(TPersistent)
  private
    FCNPJCPF   : String;
    FIE        : String;
    FxNome     : String;
    FxFant     : String;
    Ffone      : String;
    FEnderReme : TEnderReme;
    FInfNF     : TInfNFCollection;
    FInfNFE    : TInfNFECollection;
    FInfOutros : TInfOutrosCollection;
    procedure SetInfNF(Value: TInfNFCollection);
    procedure SetInfNFE(Value: TInfNFECollection);
    procedure SetInfOutros(Value: TInfOutrosCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property IE: String read FIE write FIE;
    property xNome: String read FxNome write FxNome;
    property xFant: String read FxFant write FxFant;
    property fone: String read Ffone write Ffone;
    property enderReme: TEnderReme read FEnderReme write FEnderReme;
    property infNF: TInfNFCollection read FInfNF write SetInfNF;
    property infNFe: TInfNFECollection read FInfNFE write SetInfNFE;
    property infOutros: TInfOutrosCollection read FInfOutros write SetInfOutros;
  end;

  TEnderReme = class(TPersistent)
  private
    FxLgr    : string;
    Fnro     : string;
    FxCpl    : string;
    FxBairro : string;
    FcMun    : integer;
    FxMun    : string;
    FCEP     : integer;
    FUF      : string;
    FcPais   : integer;
    FxPais   : string;
  published
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property CEP: integer read FCEP write FCEP;
    property UF: string read FUF write FUF;
    property cPais: integer read FcPais write FcPais;
    property xPais: string read FxPais write FxPais;
  end;

  TInfNFCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfNFCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfNFCollectionItem);
  public
    constructor Create(AOwner: TRem);
    function Add: TInfNFCollectionItem;
    property Items[Index: Integer]: TInfNFCollectionItem read GetItem write SetItem; default;
  end;

  TInfNFCollectionItem = class(TCollectionItem)
  private
    FnRoma  : String;
    FnPed   : String;
    Fserie  : String;
    FnDoc   : String;
    FdEmi   : TDateTime;
    FvBC    : Currency;
    FvICMS  : Currency;
    FvBCST  : Currency;
    FvST    : Currency;
    FvProd  : Currency;
    FvNF    : Currency;
    FnCFOP  : integer;
    FnPeso  : Currency;
    FPIN    : String;
    FlocRet : TLocRet;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nRoma: string read FnRoma write FnRoma;
    property nPed: string read FnPed write FnPed;
    property serie: string read Fserie write Fserie;
    property nDoc: string read FnDoc write FnDoc;
    property dEmi: TDateTime read FdEmi write FdEmi;
    property vBC: Currency read FvBC write FvBC;
    property vICMS: Currency read FvICMS write FvICMS;
    property vBCST: Currency read FvBCST write FvBCST;
    property vST: Currency read FvST write FvST;
    property vProd: Currency read FvProd write FvProd;
    property vNF: Currency read FvNF write FvNF;
    property nCFOP: integer read FnCFOP write FnCFOP;
    property nPeso: Currency read FnPeso write FnPeso;
    property PIN: string read FPIN write FPIN;
    property locRet: TLocRet read FlocRet write FlocRet;
  end;

  TLocRet = class(TPersistent)
  private
    FCNPJCPF : String;
    FxNome   : String;
    FxLgr    : string;
    Fnro     : string;
    FxCpl    : string;
    FxBairro : string;
    FcMun    : integer;
    FxMun    : string;
    FUF      : string;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property xNome: string read FxNome write FxNome;
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property UF: string read FUF write FUF;
  end;

  TInfNFECollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfNFECollectionItem;
    procedure SetItem(Index: Integer; Value: TInfNFECollectionItem);
  public
    constructor Create(AOwner: TRem);
    function Add: TInfNFECollectionItem;
    property Items[Index: Integer]: TInfNFECollectionItem read GetItem write SetItem; default;
  end;

  TInfNFECollectionItem = class(TCollectionItem)
  private
    Fchave : String;
    FPIN   : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property chave: string read Fchave write Fchave;
    property PIN: string read FPIN write FPIN;
  end;

  TInfOutrosCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfOutrosCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfOutrosCollectionItem);
  public
    constructor Create(AOwner: TRem);
    function Add: TInfOutrosCollectionItem;
    property Items[Index: Integer]: TInfOutrosCollectionItem read GetItem write SetItem; default;
  end;

  TInfOutrosCollectionItem = class(TCollectionItem)
  private
    FtpDoc      : TpcteTipoDocumento;
    FdescOutros : string;
    FnDoc       : string;
    FdEmi       : TdateTime;
    FvDocFisc   : Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property tpDoc: TpcteTipoDocumento read FtpDoc write FtpDoc;
    property descOutros: string read FdescOutros write FdescOutros;
    property nDoc: string read FnDoc write FnDoc;
    property dEmi: TdateTime read FdEmi write FdEmi;
    property vDocFisc: Currency read FvDocFisc write FvDocFisc;
  end;

  TExped = class(TPersistent)
  private
    FCNPJCPF    : String;
    FIE         : String;
    FxNome      : String;
    Ffone       : String;
    FEnderExped : TEnderExped;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property CNPJCPF : String read FCNPJCPF write FCNPJCPF;
    property IE : String read FIE write FIE;
    property xNome : String read FxNome write FxNome;
    property fone : String read Ffone write Ffone;
    property enderExped: TEnderExped read FEnderExped write FEnderExped;
  end;

  TEnderExped = class(TPersistent)
  private
    FxLgr    : string;
    Fnro     : string;
    FxCpl    : string;
    FxBairro : string;
    FcMun    : integer;
    FxMun    : string;
    FCEP     : integer;
    FUF      : string;
    FcPais   : integer;
    FxPais   : string;
  published
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property CEP: integer read FCEP write FCEP;
    property UF: string read FUF write FUF;
    property cPais: integer read FcPais write FcPais;
    property xPais: string read FxPais write FxPais;
  end;

  TReceb = class(TPersistent)
  private
    FCNPJCPF    : String;
    FIE         : String;
    FxNome      : String;
    Ffone       : String;
    FEnderReceb : TEnderReceb;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property CNPJCPF : String read FCNPJCPF write FCNPJCPF;
    property IE : String read FIE write FIE;
    property xNome : String read FxNome write FxNome;
    property fone : String read Ffone write Ffone;
    property enderReceb: TEnderReceb read FEnderReceb write FEnderReceb;
  end;

  TEnderReceb = class(TPersistent)
  private
    FxLgr    : string;
    Fnro     : string;
    FxCpl    : string;
    FxBairro : string;
    FcMun    : integer;
    FxMun    : string;
    FCEP     : integer;
    FUF      : string;
    FcPais   : integer;
    FxPais   : string;
  published
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property CEP: integer read FCEP write FCEP;
    property UF: string read FUF write FUF;
    property cPais: integer read FcPais write FcPais;
    property xPais: string read FxPais write FxPais;
  end;

  TDest = class(TPersistent)
  private
    FCNPJCPF   : String;
    FIE        : String;
    FxNome     : String;
    Ffone      : String;
    FISUF      : String;
    FEnderDest : TEnderDest;
    FlocEnt    : TLocEnt;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property CNPJCPF : String read FCNPJCPF write FCNPJCPF;
    property IE : String read FIE write FIE;
    property xNome : String read FxNome write FxNome;
    property fone : String read Ffone write Ffone;
    property ISUF : String read FISUF write FISUF;
    property enderDest: TEnderDest read FEnderDest write FEnderDest;
    property locEnt: TLocEnt read FlocEnt write FlocEnt;
  end;

  TEnderDest = class(TPersistent)
  private
    FxLgr    : string;
    Fnro     : string;
    FxCpl    : string;
    FxBairro : string;
    FcMun    : integer;
    FxMun    : string;
    FCEP     : integer;
    FUF      : string;
    FcPais   : integer;
    FxPais   : string;
  published
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property CEP: integer read FCEP write FCEP;
    property UF: string read FUF write FUF;
    property cPais: integer read FcPais write FcPais;
    property xPais: string read FxPais write FxPais;
  end;

  TLocEnt = class(TPersistent)
  private
    FCNPJCPF : String;
    FxNome   : String;
    FxLgr    : string;
    Fnro     : string;
    FxCpl    : string;
    FxBairro : string;
    FcMun    : integer;
    FxMun    : string;
    FUF      : string;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property xNome: string read FxNome write FxNome;
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property UF: string read FUF write FUF;
  end;

  TvPrest = class(TPersistent)
  private
    FvTPrest : Currency;
    FvRec    : Currency;
    Fcomp    : TCompCollection;
    procedure SetCompItem(const Value: TCompCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property vTPrest : Currency read FvTPrest write FvTPrest;
    property vRec : Currency read FvRec write FvRec;
    property comp: TCompCollection read Fcomp write SetCompItem;
  end;

  TCompCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TCompCollectionItem;
    procedure SetItem(Index: Integer; Value: TCompCollectionItem);
  public
    constructor Create(AOwner: TCTe);
    function Add: TCompCollectionItem;
    property Items[Index: Integer]: TCompCollectionItem read GetItem write SetItem; default;
  end;

  TCompCollectionItem = class(TCollectionItem)
  private
    FxNome : string;
    FvComp : Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property xNome: string read FxNome write FxNome;
    property vComp: Currency read FvComp write FvComp;
  end;

  TImp = class(TPersistent)
  private
    FICMS       : TICMS;
    FInfAdFisco : String;
    FvTotTrib: Currency;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property ICMS: TICMS read FICMS write FICMS;
    property infAdFisco : String read FInfAdFisco write FInfAdFisco;
    property vTotTrib: Currency read FvTotTrib write FvTotTrib;
  end;

  TICMS = class(TPersistent)
  private
    FSituTrib    : TpcnCSTIcms;
    FCST00       : TCST00;
    FCST20       : TCST20;
    FCST45       : TCST45;
    FCST60       : TCST60;
    FCST80       : TCST80;
    FCST81       : TCST81;
    FCST90       : TCST90;
    FICMSOutraUF : TICMSOutraUF;
    FICMSSN      : TICMSSN;
  public
    constructor Create(AOwner: TImp);
    destructor Destroy; override;
  published
    property SituTrib: TpcnCSTIcms read FSituTrib write FSituTrib;
    property CST00 : TCST00 read FCST00 write FCST00;
    property CST20 : TCST20 read FCST20 write FCST20;
    property CST45 : TCST45 read FCST45 write FCST45;
    property CST80 : TCST80 read FCST80 write FCST80;
    property CST81 : TCST81 read FCST81 write FCST81;
    property CST90 : TCST90 read FCST90 write FCST90;
  end;

  TCST00 = class(TPersistent)
  private
    FCST   : TpcnCSTIcms;
    FvBC   : Currency;
    FpICMS : Currency;
    FvICMS : Currency;
  published
    property CST: TpcnCSTIcms read FCST write FCST default cst00;
    property vBC: Currency read FvBC write FvBC;
    property pICMS: Currency read FpICMS write FpICMS;
    property vICMS: Currency read FvICMS write FvICMS;
  end;

  TCST20 = class(TPersistent)
  private
    FCST    : TpcnCSTIcms;
    FpRedBC : Currency;
    FvBC    : Currency;
    FpICMS  : Currency;
    FvICMS  : Currency;
  published
    property CST: TpcnCSTIcms read FCST write FCST default cst20;
    property pRedBC: Currency read FpRedBC write FpRedBC;
    property vBC: Currency read FvBC write FvBC;
    property pICMS: Currency read FpICMS write FpICMS;
    property vICMS: Currency read FvICMS write FvICMS;
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
    property CST: TpcnCSTIcms read FCST write FCST default cst60;
    property vBCSTRet: Currency read FvBCSTRet write FvBCSTRet;
    property vICMSSTRet: Currency read FvICMSSTRet write FvICMSSTRet;
    property pICMSSTRet: Currency read FpICMSSTRet write FpICMSSTRet;
    property vCred: Currency read FvCred write FvCred;
  end;

  TCST80 = class(TPersistent)
  private
    FCST   : TpcnCSTIcms;
    FvBC   : Currency;
    FpICMS : Currency;
    FvICMS : Currency;
    FvCred : Currency;
  published
    property CST: TpcnCSTIcms read FCST write FCST default cst80;
    property vBC: Currency read FvBC write FvBC;
    property pICMS: Currency read FpICMS write FpICMS;
    property vICMS: Currency read FvICMS write FvICMS;
    property vCred: Currency read FvCred write FvCred;
  end;

  TCST81 = class(TPersistent)
  private
    FCST    : TpcnCSTIcms;
    FpRedBC : Currency;
    FvBC    : Currency;
    FpICMS  : Currency;
    FvICMS  : Currency;
  published
    property CST: TpcnCSTIcms read FCST write FCST default cst81;
    property pRedBC: Currency read FpRedBC write FpRedBC;
    property vBC: Currency read FvBC write FvBC;
    property pICMS: Currency read FpICMS write FpICMS;
    property vICMS: Currency read FvICMS write FvICMS;
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
    property CST: TpcnCSTIcms read FCST write FCST default cst90;
    property pRedBC: Currency read FpRedBC write FpRedBC;
    property vBC: Currency read FvBC write FvBC;
    property pICMS: Currency read FpICMS write FpICMS;
    property vICMS: Currency read FvICMS write FvICMS;
    property vCred: Currency read FvCred write FvCred;
  end;

  TICMSOutraUF = class(TPersistent)
  private
    FCST           : TpcnCSTIcms;
    FpRedBCOutraUF : Currency;
    FvBCOutraUF    : Currency;
    FpICMSOutraUF  : Currency;
    FvICMSOutraUF  : Currency;
  published
    property CST: TpcnCSTIcms read FCST write FCST default cst90;
    property pRedBCOutraUF: Currency read FpRedBCOutraUF write FpRedBCOutraUF;
    property vBCOutraUF: Currency read FvBCOutraUF write FvBCOutraUF;
    property pICMSOutraUF: Currency read FpICMSOutraUF write FpICMSOutraUF;
    property vICMSOutraUF: Currency read FvICMSOutraUF write FvICMSOutraUF;
  end;

  TICMSSN = class(TPersistent)
  private
    FindSN : Integer;
  published
    property indSN: Integer read FindSN write FindSN;
  end;

  TInfCarga = class(TPersistent)
  private
    FvMerc    : Currency;
    FproPred  : string;
    FxOutCat  : string;
    FInfQ     : TInfQCollection;
    procedure SetInfQ(const Value: TInfQCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property vMerc : Currency read FvMerc write FvMerc;
    property proPred : String read FproPred write FproPred;
    property xOutCat : String read FxOutCat write FxOutCat;
    property infQ: TInfQCollection read FInfQ write SetInfQ;
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
    FcUnid  : UnidMed;
    FtpMed  : string;
    FqCarga : currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property cUnid: UnidMed read FcUnid write FcUnid;
    property tpMed: string read FtpMed write FtpMed;
    property qCarga: currency read FqCarga write FqCarga;
  end;

  TInfCTeNorm = class(TPersistent)
  private
    FcontQt    : TInfcontQtCollection;
    FemiDocAnt : TemiDocAntCollection;
    procedure SetcontQt(const Value: TInfcontQtCollection);
    procedure SetemiDocAnt(const Value: TemiDocAntCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property contQt: TInfcontQtCollection read FcontQt write SetcontQt;
    property emiDocAnt: TemiDocAntCollection read FemiDocAnt write SetemiDocAnt;
  end;

  TInfcontQtCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfcontQtCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfcontQtCollectionItem);
  public
    constructor Create(AOwner: TInfCTeNorm);
    function Add: TInfcontQtCollectionItem;
    property Items[Index: Integer]: TInfcontQtCollectionItem read GetItem write SetItem; default;
  end;

  TInfcontQtCollectionItem = class(TCollectionItem)
  private
    // Alterado de Integer para String por Italo em 31/01/2012
    FnCont     : String;
    FlacContQt : TlacContQtCollection;
    FdPrev     : TDateTime;
    procedure SetlacContQt(const Value: TlacContQtCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    // Alterado de Integer para String por Italo em 31/01/2012
    property nCont: String read FnCont write FnCont;
    property lacContQt: TlacContQtCollection read FlacContQt write SetlacContQt;
    property dPrev: TDateTime read FdPrev write FdPrev;
  end;

  TlacContQtCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TlacContQtCollectionItem;
    procedure SetItem(Index: Integer; Value: TlacContQtCollectionItem);
  public
    constructor Create(AOwner: TInfcontQtCollectionItem);
    function Add: TlacContQtCollectionItem;
    property Items[Index: Integer]: TlacContQtCollectionItem read GetItem write SetItem; default;
  end;

  TlacContQtCollectionItem = class(TCollectionItem)
  private
    FnLacre : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nLacre: String read FnLacre write FnLacre;
  end;

  TemiDocAntCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TemiDocAntCollectionItem;
    procedure SetItem(Index: Integer; Value: TemiDocAntCollectionItem);
  public
    constructor Create(AOwner: TInfCTeNorm);
    function Add: TemiDocAntCollectionItem;
    property Items[Index: Integer]: TemiDocAntCollectionItem read GetItem write SetItem; default;
  end;

  TemiDocAntCollectionItem = class(TCollectionItem)
  private
    FCNPJCPF  : String;
    FIE       : String;
    FUF       : String;
    FxNome    : String;
    FidDocAnt : TidDocAntCollection;
    procedure SetidDocAnt(const Value: TidDocAntCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property IE: String read FIE write FIE;
    property UF: String read FUF write FUF;
    property xNome: String read FxNome write FxNome;
    property idDocAnt: TidDocAntCollection read FidDocAnt write SetidDocAnt;
  end;

  TidDocAntCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TidDocAntCollectionItem;
    procedure SetItem(Index: Integer; Value: TidDocAntCollectionItem);
  public
    constructor Create(AOwner: TemiDocAntCollectionItem);
    function Add: TidDocAntCollectionItem;
    property Items[Index: Integer]: TidDocAntCollectionItem read GetItem write SetItem; default;
  end;

  TidDocAntCollectionItem = class(TCollectionItem)
  private
    FidDocAntPap : TidDocAntPapCollection;
    FidDocAntEle : TidDocAntEleCollection;
    procedure SetidDocAntPap(const Value: TidDocAntPapCollection);
    procedure SetidDocAntEle(const Value: TidDocAntEleCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property idDocAntPap: TidDocAntPapCollection read FidDocAntPap write SetidDocAntPap;
    property idDocAntEle: TidDocAntEleCollection read FidDocAntEle write SetidDocAntEle;
  end;

  TidDocAntPapCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TidDocAntPapCollectionItem;
    procedure SetItem(Index: Integer; Value: TidDocAntPapCollectionItem);
  public
    constructor Create(AOwner: TidDocAntCollectionItem);
    function Add: TidDocAntPapCollectionItem;
    property Items[Index: Integer]: TidDocAntPapCollectionItem read GetItem write SetItem; default;
  end;

  TidDocAntPapCollectionItem = class(TCollectionItem)
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
    property tpDoc: TpcteTipoDocumentoAnterior read FtpDoc write FtpDoc;
    property serie: String read Fserie write Fserie;
    property subser: String read Fsubser write Fsubser;
    property nDoc: Integer read FnDoc write FnDoc;
    property dEmi: TDateTime read FdEmi write FdEmi;
  end;

  TidDocAntEleCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TidDocAntEleCollectionItem;
    procedure SetItem(Index: Integer; Value: TidDocAntEleCollectionItem);
  public
    constructor Create(AOwner: TidDocAntCollectionItem);
    function Add: TidDocAntEleCollectionItem;
    property Items[Index: Integer]: TidDocAntEleCollectionItem read GetItem write SetItem; default;
  end;

  TidDocAntEleCollectionItem = class(TCollectionItem)
  private
    Fchave : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property chave: String read Fchave write Fchave;
  end;

  TInfSegCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfSegCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfSegCollectionItem);
  public
    constructor Create(AOwner: TCTe);
    function Add: TInfSegCollectionItem;
    property Items[Index: Integer]: TInfSegCollectionItem read GetItem write SetItem; default;
  end;

  TInfSegCollectionItem = class(TCollectionItem)
  private
    FrespSeg : TpcteRspSeg;
    FxSeg    : String;
    FnApol   : String;
    FnAver   : String;
    FvMerc    : Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property respSeg: TpcteRspSeg read FrespSeg write FrespSeg;
    property xSeg: string read FxSeg write FxSeg;
    property nApol: string read FnApol write FnApol;
    property nAver: string read FnAver write FnAver;
    property vMerc: Currency read FvMerc write FvMerc;
  end;

  TRodo = class(TPersistent)
  private
    FRNTRC   : String;
    FdPrev   : tDateTime;
    FLota    : TpcteLotacao;
    FCTRB    : TCTRB;
    FOcc     : TOccCollection;
    FvalePed : TValePed;
    Fveic    : TVeicCollection;
    FLacres  : TLacresCollection;
    Fmoto    : TMotoCollection;
    procedure SetOcc(const Value: TOccCollection);
    procedure SetVeic(const Value: TVeicCollection);
    procedure SetLacres(const Value: TLacresCollection);
    procedure SetMoto(const Value: TMotoCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property RNTRC: String read FRNTRC write FRNTRC;
    property dPrev: TDateTime read FdPrev write FdPrev;
    property Lota: TpcteLotacao read FLota write FLota;
    property CTRB: TCTRB read FCTRB write FCTRB;
    property Occ: TOccCollection read FOcc write SetOcc;
    property valePed: TValePed read FvalePed write FvalePed;
    property veic: TVeicCollection read Fveic write SetVeic;
    property Lacres: TLacresCollection read FLacres write SetLacres;
    property moto: TMotoCollection read Fmoto write SetMoto;
  end;

  TCTRB = class(TPersistent)
  private
    Fserie : Integer;
    FnCTRB : Integer;
  public
    property serie: Integer read Fserie write Fserie;
    property nCTRB: Integer read FnCTRB write FnCTRB;
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
    FEmiOCC : TEmiOCC;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property serie: String read Fserie write Fserie;
    property nOcc: Integer read FnOcc write FnOcc;
    property dEmi: TDateTime read FdEmi write FdEmi;
    property EmiOCC: TEmiOCC read FEmiOCC write FEmiOCC;
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
    property IE: String read FIE write FIE;
    property UF: String read FUF write FUF;
    property fone: String read Ffone write Ffone;
  end;

  TValePed = class(TPersistent)
  private
    FnroRE     : String;
    FvTValePed : Currency;
    FrespPg    : TpcteRspPagPedagio;
    Fdisp      : TDispCollection;
    procedure SetDisp(const Value: TDispCollection);
  public
    constructor Create(AOwner: TRodo);
    destructor Destroy; override;
  published
    property nroRE: String read FnroRE write FnroRE;
    property vTValePed: Currency read FvTValePed write FvTValePed;
    property respPg: TpcteRspPagPedagio read FrespPg write FrespPg;
    property disp: TDispCollection read Fdisp write SetDisp;
  end;

  TDispCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDispCollectionItem;
    procedure SetItem(Index: Integer; Value: TDispCollectionItem);
  public
    constructor Create(AOwner: TValePed);
    function Add: TDispCollectionItem;
    property Items[Index: Integer]: TDispCollectionItem read GetItem write SetItem; default;
  end;

  TDispCollectionItem = class(TCollectionItem)
  private
    FtpDisp : TpcteTipoDispositivo;
    FxEmp   : String;
    FdVig   : TDateTime;
    FnDisp  : String;
    FnCompC : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property tpDisp: TpcteTipoDispositivo read FtpDisp write FtpDisp;
    property xEmp: String read FxEmp write FxEmp;
    property dVig: TDateTime read FdVig write FdVig;
    property nDisp: String read FnDisp write FnDisp;
    property nCompC: String read FnCompC write FnCompC;
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
    FProp    : Tprop;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property cInt: String read FcInt write FcInt;
    property RENAVAM: String read FRENAVAM write FRENAVAM;
    property placa: String read Fplaca write Fplaca;
    property tara: Integer read Ftara write Ftara;
    property capKG: Integer read FcapKG write FcapKG;
    property capM3: Integer read FcapM3 write FcapM3;
    property tpProp: TpcteTipoPropriedade read FtpProp write FtpProp;
    property tpVeic: TpcteTipoVeiculo read FtpVeic write FtpVeic;
    property tpRod: TpcteTipoRodado read FtpRod write FtpRod;
    property tpCar: TpcteTipoCarroceria read FtpCar write FtpCar;
    property UF: String read FUF write FUF;
    property Prop: Tprop read FProp write FProp;
  end;

  Tprop = class(TPersistent)
  private
    FCNPJCPF : String;
    FRNTRC   : String;
    FxNome   : String;
    FIE      : String;
    FUF      : String;
    FtpProp  : TpcteProp;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property RNTRC: String read FRNTRC write FRNTRC;
    property xNome: String read FxNome write FxNome;
    property IE: String read FIE write FIE;
    property UF: String read FUF write FUF;
    property tpProp: TpcteProp read FtpProp write FtpProp;
  end;

  TLacresCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TLacresCollectionItem;
    procedure SetItem(Index: Integer; Value: TLacresCollectionItem);
  public
    constructor Create(AOwner: TRodo);
    function Add: TLacresCollectionItem;
    property Items[Index: Integer]: TLacresCollectionItem read GetItem write SetItem; default;
  end;

  TLacresCollectionItem = class(TCollectionItem)
  private
    FnLacre : string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nLacre: string read FnLacre write FnLacre;
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
    FxNome : string;
    FCPF   : string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property xNome: string read FxNome write FxNome;
    property CPF: string read FCPF write FCPF;
  end;

  Taereo = class(TPersistent)
  private
    FnMinu   : Integer;
    FnOCA    : String;
    FdPrev   : tDateTime;
    FxLAgEmi : String;
    FcIATA   : String;
    Ftarifa  : Ttarifa;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property nMinu: Integer read FnMinu write Fnminu;
    property nOCA: String read FnOCA write FnOCA;
    property dPrev: TDateTime read FdPrev write FdPrev;
    property xLAgEmi: String read FxLAgEmi write FxLAgEmi;
    property cIATA: String read FcIATA write FcIATA;
    property tarifa: Ttarifa read Ftarifa write Ftarifa;
  end;

  Ttarifa = class(TPersistent)
  private
    Ftrecho : String;
    FCL    : String;
    FcTar  : String;
    FvTar  : Currency;
  public
    property trecho: String read Ftrecho write Ftrecho;
    property CL: String read FCL write FCL;
    property cTar: String read FcTar write FcTar;
    property vTar: Currency read FvTar write FvTar;
  end;

  TnatCarga = class(TPersistent)
  private
    FxDime    : String;
    FcinfManu : Integer; // Alterar para ser uma lista
    FcIMP     : String;  // Alterar para ser uma lista
  public
    property xDime: String read FxDime write FxDime;
    property cinfManu: Integer read FcinfManu write FcinfManu;
    property cIMP: String read FcIMP write FcIMP;
  end;

  Taquav = class(TPersistent) // Definir o Grupo detCont
  private
    FvPrest   : Currency;
    FvAFRMM   : Currency;
    FnBooking : String;
    FnCtrl    : String;
    FxNavio   : String;
    FnViag    : String;
    Fdirec    : TpcteDirecao;
    FprtEmb   : String;
    FprtTrans : String;
    FprtDest  : String;
    FtpNav    : TpcteTipoNavegacao;
    Firin     : String;
    Flacre    : TLacreCollection;

    procedure SetLacre(const Value: TLacreCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property vPrest: Currency read FvPrest write FvPrest;
    property vAFRMM: Currency read FvAFRMM write FvAFRMM;
    property nBooking: String read FnBooking write FnBooking;
    property nCtrl: String read FnCtrl write FnCtrl;
    property xNavio: String read FxNavio write FxNavio;
    property nViag: String read FnViag write FnViag;
    property direc: TpcteDirecao read Fdirec write Fdirec;
    property prtEmb: String read FprtEmb write FprtEmb;
    property prtTrans: String read FprtTrans write FprtTrans;
    property prtDest: String read FprtDest write FprtDest;
    property tpNav: TpcteTipoNavegacao read FtpNav write FtpNav;
    property irin: String read Firin write Firin;
    property Lacre: TLacreCollection read FLacre write SetLacre;
  end;

  TbalsaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TbalsaCollectionItem;
    procedure SetItem(Index: Integer; Value: TbalsaCollectionItem);
  public
    constructor Create(AOwner: Taquav);
    function Add: TbalsaCollectionItem;
    property Items[Index: Integer]: TbalsaCollectionItem read GetItem write SetItem; default;
  end;

  TbalsaCollectionItem = class(TCollectionItem)
  private
    FxBalsa : string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property xBalsa: string read FxBalsa write FxBalsa;
  end;

  TLacreCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TLacreCollectionItem;
    procedure SetItem(Index: Integer; Value: TLacreCollectionItem);
  public
    constructor Create(AOwner: Taquav);
    function Add: TLacreCollectionItem;
    property Items[Index: Integer]: TLacreCollectionItem read GetItem write SetItem; default;
  end;

  TLacreCollectionItem = class(TCollectionItem)
  private
    FnLacre : string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nLacre: string read FnLacre write FnLacre;
  end;

  // Informações do modal Ferroviário
  Tferrov = class(TPersistent)
  private
    FtpTraf   : TpcteTipoTrafego;
    Ffluxo    : String;
    FidTrem   : String;
    FvFrete   : Currency;
    FferroSub : TferroSub;
    FDCL      : TDCLCollection;
    FdetVag   : TdetVagCollection;
    procedure SetDCL(const Value: TDCLCollection);
    procedure SetdetVag(const Value: TdetVagCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property tpTraf: TpcteTipoTrafego read FtpTraf write FtpTraf;
    property fluxo: String read Ffluxo write Ffluxo;
    property idTrem: String read FidTrem write FidTrem;
    property vFrete: Currency read FvFrete write FvFrete;
    property ferroSub: TferroSub read FferroSub write FferroSub;
    property DCL: TDCLCollection read FDCL write SetDCL;
    property detVag: TdetVagCollection read FdetVag write SetdetVag;
  end;

  TferroSub = class(TPersistent)
  private
    FCNPJ       : String;
    FcInt       : String;
    FIE         : String;
    FxNome      : String;
    FEnderFerro : TEnderFerro;
  public
    constructor Create(AOwner: Tferrov);
    destructor Destroy; override;
  published
    property CNPJ : String read FCNPJ write FCNPJ;
    property cInt : String read FcInt write FcInt;
    property IE : String read FIE write FIE;
    property xNome : String read FxNome write FxNome;
    property EnderFerro: TEnderFerro read FEnderFerro write FEnderFerro;
  end;

  TtrafMut = class(TPersistent)
  private
    FrespFat : TpcteTrafegoMutuo;
    FferrEmi : TpcteTrafegoMutuo;
  published
    property respFat : TpcteTrafegoMutuo read FrespFat write FrespFat;
    property ferrEmi : TpcteTrafegoMutuo read FferrEmi write FferrEmi;
  end;

  TEnderFerro = class(TPersistent)
  private
    FxLgr    : string;
    Fnro     : string;
    FxCpl    : string;
    FxBairro : string;
    FcMun    : integer;
    FxMun    : string;
    FCEP     : integer;
    FUF      : string;
  published
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property CEP: integer read FCEP write FCEP;
    property UF: string read FUF write FUF;
  end;

  TDCLCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDCLCollectionItem;
    procedure SetItem(Index: Integer; Value: TDCLCollectionItem);
  public
    constructor Create(AOwner: Tferrov);
    function Add: TDCLCollectionItem;
    property Items[Index: Integer]: TDCLCollectionItem read GetItem write SetItem; default;
  end;

  TDCLCollectionItem = class(TCollectionItem)
  private
    Fserie     : string;
    FnDCL      : string;
    FdEmi      : TDateTime;
    FqVag      : integer;
    FpCalc     : Currency;
    FvTar      : Currency;
    FvFrete    : Currency;
    FvSAcess   : Currency;
    FvTServ    : Currency;
    FidTrem    : string;
    FdetVagDCL : TdetVagDCLCollection;
    procedure SetdetVagDCL(const Value: TdetVagDCLCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property serie: string read Fserie write Fserie;
    property nDCL: string read FnDCL write FnDCL;
    property dEmi: TDateTime read FdEmi write FdEmi;
    property qVag: integer read FqVag write FqVag;
    property pCalc: Currency read FpCalc write FpCalc;
    property vTar: Currency read FvTar write FvTar;
    property vFrete: Currency read FvFrete write FvFrete;
    property vSAcess: Currency read FvSAcess write FvSAcess;
    property vTServ: Currency read FvTServ write FvTServ;
    property idTrem: string read FidTrem write FidTrem;
    property detVagDCL: TdetVagDCLCollection read FdetVagDCL write SetdetVagDCL;
  end;

  TdetVagDCLCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TdetVagDCLCollectionItem;
    procedure SetItem(Index: Integer; Value: TdetVagDCLCollectionItem);
  public
    constructor Create(AOwner: TDCLCollectionItem);
    function Add: TdetVagDCLCollectionItem;
    property Items[Index: Integer]: TdetVagDCLCollectionItem read GetItem write SetItem; default;
  end;

  TdetVagDCLCollectionItem = class(TCollectionItem)
  private
    FnVag         : integer;
    Fcap          : Currency;
    FtpVag        : string;
    FpesoR        : Currency;
    FpesoBC       : Currency;
    FlacDetVagDCL : TlacDetVagDCLCollection;
    FcontDCL      : TcontDCLCollection;
    procedure SetlacDetVagDCL(const Value: TlacDetVagDCLCollection);
    procedure SetcontDCL(const Value: TcontDCLCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nVag: integer read FnVag write FnVag;
    property cap: Currency read Fcap write Fcap;
    property tpVag: string read FtpVag write FtpVag;
    property pesoR: Currency read FpesoR write FpesoR;
    property pesoBC: Currency read FpesoBC write FpesoBC;
   property lacDetVagDCL: TlacDetVagDCLCollection read FlacDetVagDCL write SetlacDetVagDCL;
   property contDCL: TcontDCLCollection read FcontDCL write SetcontDCL;
  end;

  TlacDetVagDCLCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TlacDetVagDCLCollectionItem;
    procedure SetItem(Index: Integer; Value: TlacDetVagDCLCollectionItem);
  public
    constructor Create(AOwner: TdetVagDCLCollectionItem);
    function Add: TlacDetVagDCLCollectionItem;
    property Items[Index: Integer]: TlacDetVagDCLCollectionItem read GetItem write SetItem; default;
  end;

  TlacDetVagDCLCollectionItem = class(TCollectionItem)
  private
    FnLacre : string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nLacre: string read FnLacre write FnLacre;
  end;

  TcontDCLCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TcontDCLCollectionItem;
    procedure SetItem(Index: Integer; Value: TcontDCLCollectionItem);
  public
    constructor Create(AOwner: TdetVagDCLCollectionItem);
    function Add: TcontDCLCollectionItem;
    property Items[Index: Integer]: TcontDCLCollectionItem read GetItem write SetItem; default;
  end;

  TcontDCLCollectionItem = class(TCollectionItem)
  private
    FnCont : string;
    FdPrev : TDateTime;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nCont: string read FnCont write FnCont;
    property dPrev: TDateTime read FdPrev write FdPrev;
  end;

  TdetVagCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TdetVagCollectionItem;
    procedure SetItem(Index: Integer; Value: TdetVagCollectionItem);
  public
    constructor Create(AOwner: Tferrov);
    function Add: TdetVagCollectionItem;
    property Items[Index: Integer]: TdetVagCollectionItem read GetItem write SetItem; default;
  end;

  TdetVagCollectionItem = class(TCollectionItem)
  private
    FnVag   : integer;
    Fcap    : Currency;
    FtpVag  : string;
    FpesoR  : Currency;
    FpesoBC : Currency;
    FlacDetVag : TlacDetVagCollection;
    FcontVag : TcontVagCollection;
    procedure SetlacDetVag(const Value: TlacDetVagCollection);
    procedure SetcontVag(const Value: TcontVagCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nVag: integer read FnVag write FnVag;
    property cap: Currency read Fcap write Fcap;
    property tpVag: string read FtpVag write FtpVag;
    property pesoR: Currency read FpesoR write FpesoR;
    property pesoBC: Currency read FpesoBC write FpesoBC;
    property lacDetVag: TlacDetVagCollection read FlacDetVag write SetlacDetVag;
    property contVag: TcontVagCollection read FcontVag write SetcontVag;
  end;

  TlacDetVagCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TlacDetVagCollectionItem;
    procedure SetItem(Index: Integer; Value: TlacDetVagCollectionItem);
  public
    constructor Create(AOwner: TdetVagCollectionItem);
    function Add: TlacDetVagCollectionItem;
    property Items[Index: Integer]: TlacDetVagCollectionItem read GetItem write SetItem; default;
  end;

  TlacDetVagCollectionItem = class(TCollectionItem)
  private
    FnLacre : string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nLacre: string read FnLacre write FnLacre;
  end;

  TcontVagCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TcontVagCollectionItem;
    procedure SetItem(Index: Integer; Value: TcontVagCollectionItem);
  public
    constructor Create(AOwner: TdetVagCollectionItem);
    function Add: TcontVagCollectionItem;
    property Items[Index: Integer]: TcontVagCollectionItem read GetItem write SetItem; default;
  end;

  TcontVagCollectionItem = class(TCollectionItem)
  private
    FnCont : string;
    FdPrev : TDateTime;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nCont: string read FnCont write FnCont;
    property dPrev: TDateTime read FdPrev write FdPrev;
  end;

  Tduto = class(TPersistent)
  private
    FvTar : Currency;
  published
    property vTar: Currency read FvTar write FvTar;
  end;

  TperiCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TperiCollectionItem;
    procedure SetItem(Index: Integer; Value: TperiCollectionItem);
  public
    constructor Create(AOwner: TCTe);
    function Add: TperiCollectionItem;
    property Items[Index: Integer]: TperiCollectionItem read GetItem write SetItem; default;
  end;

  TperiCollectionItem = class(TCollectionItem)
  private
    FnONU        : String;
    FxNomeAE     : String;
    FxClaRisco   : String;
    FgrEmb       : String;
    FqTotProd    : String;
    FqVolTipo    : String;
    FpontoFulgor : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nONU: string read FnONU write FnONU;
    property xNomeAE: string read FxNomeAE write FxNomeAE;
    property xClaRisco: string read FxClaRisco write FxClaRisco;
    property grEmb: string read FgrEmb write FgrEmb;
    property qTotProd: string read FqTotProd write FqTotProd;
    property qVolTipo: string read FqVolTipo write FqVolTipo;
    property pontoFulgor: string read FpontoFulgor write FpontoFulgor;
  end;

  TveicNovosCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TveicNovosCollectionItem;
    procedure SetItem(Index: Integer; Value: TveicNovosCollectionItem);
  public
    constructor Create(AOwner: TCTe);
    function Add: TveicNovosCollectionItem;
    property Items[Index: Integer]: TveicNovosCollectionItem read GetItem write SetItem; default;
  end;

  TveicNovosCollectionItem = class(TCollectionItem)
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
    property chassi: string read Fchassi write Fchassi;
    property cCor: string read FcCor write FcCor;
    property xCor: string read FxCor write FxCor;
    property cMod: string read FcMod write FcMod;
    property vUnit: Currency read FvUnit write FvUnit;
    property vFrete: Currency read FvFrete write FvFrete;
  end;

  TCobr = class(TPersistent)
  private
    FFat: TFat;
    FDup: TDupCollection;
    procedure SetDup(Value: TDupCollection);
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property Fat: TFat read FFat write FFat;
    property Dup: TDupCollection read FDup write SetDup;
  end;

  TFat = class(TPersistent)
  private
    FnFat: string;
    FvOrig: currency;
    FvDesc: currency;
    FvLiq: currency;
  published
    property nFat: string read FnFat write FnFat;
    property vOrig: currency read FvOrig write FvOrig;
    property vDesc: currency read FvDesc write FvDesc;
    property vLiq: currency read FvLiq write FvLiq;
  end;

  TDupCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDupCollectionItem;
    procedure SetItem(Index: Integer; Value: TDupCollectionItem);
  public
    constructor Create(AOwner: TCobr);
    destructor Destroy; override;

    function Add: TDupCollectionItem;
    property Items[Index: Integer]: TDupCollectionItem read GetItem write SetItem; default;
  end;

  TDupCollectionItem = class(TCollectionItem)
  private
    FnDup: string;
    FdVenc: TDateTime;
    FvDup: currency;
  published
    property nDup: string read FnDup write FnDup;
    property dVenc: TDateTime read FdVenc write FdVenc;
    property vDup: currency read FvDup write FvDup;
  end;

  TrefNF = class(TPersistent)
  private
    FCNPJ     : String;
    Fmod      : String;
    Fserie    : Integer;
    Fsubserie : Integer;
    Fnro      : Integer;
    Fvalor    : Currency;
    FdEmi     : TDateTime;
  published
    property CNPJ: String read FCNPJ write FCNPJ;
    property modelo: String read Fmod write Fmod;
    property serie: Integer read Fserie write Fserie;
    property subserie: Integer read Fsubserie write Fsubserie;
    property nro: Integer read Fnro write Fnro;
    property valor: Currency read Fvalor write Fvalor;
    property dEmi: TDateTime read FdEmi write FdEmi;
  end;

  TtomaICMS = class(TPersistent)
  private
    FrefNFe : String;
    FrefNF  : TrefNF;
    FrefCte : String;
  public
    constructor Create(AOwner: TinfCTeSub);
    destructor Destroy; override;
  published
    property refNFe: String read FrefNFe write FrefNFe;
    property refNF: TrefNF read FrefNF write FrefNF;
    property refCte: String read FrefCte write FrefCte;
  end;

  TtomaNaoICMS = class(TPersistent)
  private
    FrefCteAnu : String;
  published
    property refCteAnu: String read FrefCteAnu write FrefCteAnu;
  end;

  TInfCTeSub = class(TPersistent)
  private
    FchCte       : String;
    FtomaICMS    : TtomaICMS;
    FtomaNaoICMS : TtomaNaoICMS;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property chCte: String read FchCte write FchCte;
    property tomaICMS: TtomaICMS read FtomaICMS write FtomaICMS;
    property tomaNaoICMS: TtomaNaoICMS read FtomaNaoICMS write FtomaNaoICMS;
  end;

  TinfCTeCompCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfCTeCompCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfCTeCompCollectionItem);
  public
    constructor Create(AOwner: TCTe);
    function Add: TinfCTeCompCollectionItem;
    property Items[Index: Integer]: TinfCTeCompCollectionItem read GetItem write SetItem; default;
  end;

  TinfCTeCompCollectionItem = class(TCollectionItem)
  private
    FChave     : String;
    FvPresComp : TvPresComp;
    FimpComp   : TimpComp;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property Chave : String read FChave write FChave;
    property vPresComp: TvPresComp read FvPresComp write FvPresComp;
    property impComp: TimpComp read FimpComp write FimpComp;
  end;

  TvPresComp = class(TPersistent)
  private
    FvTPrest  : Currency;
    FcompComp : TcompCompCollection;
    procedure SetcompCompItem(const Value: TcompCompCollection);
  public
    constructor Create(AOwner: TinfCTeCompCollectionItem);
    destructor Destroy; override;
  published
    property vTPrest: Currency read FvTPrest write FvTPrest;
    property compComp: TcompCompCollection read FcompComp write SetcompCompItem;
  end;

  TcompCompCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TcompCompCollectionItem;
    procedure SetItem(Index: Integer; Value: TcompCompCollectionItem);
  public
    constructor Create(AOwner: TvPresComp);
    function Add: TcompCompCollectionItem;
    property Items[Index: Integer]: TcompCompCollectionItem read GetItem write SetItem; default;
  end;

  TcompCompCollectionItem = class(TCollectionItem)
  private
    FxNome : string;
    FvComp : Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property xNome: string read FxNome write FxNome;
    property vComp: Currency read FvComp write FvComp;
  end;

  TImpComp = class(TPersistent)
  private
    FICMSComp   : TICMSComp;
    FInfAdFisco : String;
    FvTotTrib: Currency;
  public
    constructor Create(AOwner: TinfCTeCompCollectionItem);
    destructor Destroy; override;
  published
    property ICMSComp: TICMSComp read FICMSComp write FICMSComp;
    property InfAdFisco : String read FInfAdFisco write FInfAdFisco;
    property vTotTrib: Currency read FvTotTrib write FvTotTrib;
  end;

  TICMSComp = class(TPersistent)
  private
    FSituTrib    : TpcnCSTIcms;
    FCST00       : TCST00;
    FCST20       : TCST20;
    FCST45       : TCST45;
    FCST60       : TCST60;
    FCST80       : TCST80;
    FCST81       : TCST81;
    FCST90       : TCST90;
    FICMSOutraUF : TICMSOutraUF;
    FICMSSN      : TICMSSN;
  public
    constructor Create(AOwner: TImpComp);
    destructor Destroy; override;
  published
    property SituTrib: TpcnCSTIcms read FSituTrib write FSituTrib;
    property CST00 : TCST00 read FCST00 write FCST00;
    property CST20 : TCST20 read FCST20 write FCST20;
    property CST45 : TCST45 read FCST45 write FCST45;
    property CST80 : TCST80 read FCST80 write FCST80;
    property CST81 : TCST81 read FCST81 write FCST81;
    property CST90 : TCST90 read FCST90 write FCST90;
  end;

  TInfCTeAnuEnt = class(TPersistent)
  private
    FchCTe : String;
    FdEmi  : TDateTime;
  public
    constructor Create(AOwner: TCTe);
    destructor Destroy; override;
  published
    property chCTe : String read FchCTe write FchCTe;
    property dEmi : TDateTime read FdEmi write FdEmi;
  end;

const
  CMUN_EXTERIOR: integer = 9999999;
  XMUN_EXTERIOR: string = 'EXTERIOR';
  UF_EXTERIOR: string = 'EX';

implementation

{ TCTe }
constructor TCTe.Create;
begin
  FinfCTe       := TInfCTe.Create;
  FIde          := TIde.Create(Self);
  FCompl        := TCompl.Create(Self);
  FEmit         := TEmit.Create(Self);
  FRem          := TRem.Create(Self);;
  FExped        := TExped.Create(Self);
  FReceb        := TReceb.Create(Self);
  FDest         := TDest.Create(Self);
  FvPrest       := TvPrest.Create(Self);
  FImp          := TImp.Create(Self);
  FInfCTeNorm   := TInfCTeNorm.Create(Self);
  FInfCarga     := TInfCarga.Create(Self);
  FInfSeg       := TInfSegCollection.Create(Self);

  FRodo         := TRodo.Create(Self);
  FAereo        := Taereo.Create(Self);
  FAquav        := Taquav.Create(Self);
  FFerrov       := Tferrov.Create(Self);
  Fduto         := Tduto.Create;

  Fperi         := TperiCollection.Create(Self);
  FveicNovos    := TveicNovosCollection.Create(Self);
  FCobr         := TCobr.Create(Self);
  FinfCTeSub    := TinfCTeSub.Create(Self);
  FinfCTeComp   := TinfCTeCompCollection.Create(Self);
  FInfCTeAnuEnt := TInfCTeAnuEnt.Create(Self);
  FProcCTe      := TProcCTe.create;
  Fsignature    := Tsignature.create;
end;

destructor TCTe.Destroy;
begin
  FinfCTe.Free;
  FIde.Free;
  FCompl.Free;
  FEmit.Free;
  FRem.Free;
  FExped.Free;
  FReceb.Free;
  FDest.Free;
  FvPrest.Free;
  FImp.Free;
  FInfCTeNorm.Free;
  FInfCarga.Free;
  FInfSeg.Free;

  FRodo.Free;
  FAereo.Free;
  FAquav.Free;
  FFerrov.Free;
  Fduto.Free;

  Fperi.Free;
  FveicNovos.Free;
  FCobr.Free;
  FinfCTeSub.Free;
  FInfCTeComp.Free;
  FInfCTeAnuEnt.Free;
  FProcCTe.Free;
  Fsignature.Free;
  inherited Destroy;
end;

{Ide}

constructor TIde.Create(AOwner: TCTe);
begin
  inherited Create;
  FToma03 := TToma03.Create;
  FToma4  := TToma4.Create( AOwner );
end;

destructor TIde.Destroy;
begin
  FToma03.Free;
  FToma4.Free;
  inherited;
end;

procedure TCTe.Setperi(Value: TperiCollection);
begin
  Fperi.Assign(Value);
end;

procedure TCTe.SetveicNovos(Value: TveicNovosCollection);
begin
  FveicNovos.Assign(Value);
end;

procedure TCTe.SetInfSeg(Value: TInfSegCollection);
begin
  FInfSeg.Assign(Value);
end;

procedure TCTe.SetInfCTeComp(Value: TInfCTeCompCollection);
begin
  FInfCTeComp.Assign(Value);
end;

{TToma4}

constructor TToma4.Create(AOwner: TCTe);
begin
  inherited Create;
  FEnderToma := TEnderToma.Create;
end;

destructor TToma4.Destroy;
begin
  FEnderToma.Free;
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

{TFluxo}

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

function TObsContCollection.GetItem(Index: Integer): TObsContCollectionItem;
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

function TObsFiscoCollection.GetItem(Index: Integer): TObsFiscoCollectionItem;
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
  FEnderReme := TEnderReme.Create;
  FInfNF     := TInfNFCollection.Create(Self);
  FInfNFe    := TInfNFeCollection.Create(Self);
  FInfOutros := TInfOutrosCollection.Create(Self);
end;

destructor TRem.Destroy;
begin
  FEnderReme.Free;
  FInfNF.Free;
  FInfNFe.Free;
  FInfOutros.Free;
  inherited;
end;

procedure TRem.SetInfNF(Value: TInfNFCollection);
begin
  FInfNF.Assign(Value);
end;

procedure TRem.SetInfNFE(Value: TInfNFECollection);
begin
  FInfNFE.Assign(Value);
end;

procedure TRem.SetInfOutros(Value: TInfOutrosCollection);
begin
  FInfOutros.Assign(Value);
end;

{ TInfNFCollection }

function TInfNFCollection.Add: TInfNFCollectionItem;
begin
  Result := TInfNFCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfNFCollection.Create(AOwner: TRem);
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
  FlocRet := TLocRet.Create;
end;

destructor TInfNFCollectionItem.Destroy;
begin
  FlocRet.Free;
  inherited;
end;

{ TInfNFECollection }

function TInfNFECollection.Add: TInfNFECollectionItem;
begin
  Result := TInfNFECollectionItem(inherited Add);
  Result.create;
end;

constructor TInfNFECollection.Create(AOwner: TRem);
begin
  inherited Create(TInfNFECollectionItem);
end;

function TInfNFECollection.GetItem(Index: Integer): TInfNFECollectionItem;
begin
  Result := TInfNFECollectionItem(inherited GetItem(Index));
end;

procedure TInfNFECollection.SetItem(Index: Integer;
  Value: TInfNFECollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfNFECollectionItem }

constructor TInfNFECollectionItem.Create;
begin

end;

destructor TInfNFECollectionItem.Destroy;
begin

  inherited;
end;

{ TInfOutrosCollection }

function TInfOutrosCollection.Add: TInfOutrosCollectionItem;
begin
  Result := TInfOutrosCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfOutrosCollection.Create(AOwner: TRem);
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

end;

destructor TInfOutrosCollectionItem.Destroy;
begin

  inherited;
end;

{ TExped }

constructor TExped.Create(AOwner: TCTe);
begin
  inherited Create;
  FEnderExped := TEnderExped.Create;
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
  FEnderReceb := TEnderReceb.Create;
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
  FEnderDest := TEnderDest.Create;
  FlocEnt    := TLocEnt.Create;
end;

destructor TDest.Destroy;
begin
  FEnderDest.Free;
  FlocEnt.Free;
  inherited;
end;

{ TvPrest }

constructor TvPrest.Create(AOwner: TCTe);
begin
  inherited Create;
  FComp := TCompCollection.Create(AOwner);
end;

destructor TvPrest.Destroy;
begin
  FComp.Free;
  inherited;
end;

procedure TvPrest.SetCompItem(const Value: TCompCollection);
begin
  Fcomp.Assign(Value);
end;

{ TCompCollection }

function TCompCollection.Add: TCompCollectionItem;
begin
  Result := TCompCollectionItem(inherited Add);
  Result.create;
end;

constructor TCompCollection.Create(AOwner: TCTe);
begin
  inherited Create(TCompCollectionItem);
end;

function TCompCollection.GetItem(Index: Integer): TCompCollectionItem;
begin
  Result := TCompCollectionItem(inherited GetItem(Index));
end;

procedure TCompCollection.SetItem(Index: Integer; Value: TCompCollectionItem);
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
end;

destructor TImp.Destroy;
begin
  FICMS.free;
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
  FCST80       := TCST80.create;
  FCST81       := TCST81.create;
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
  FCST80.Free;
  FCST81.Free;
  FCST90.Free;
  FICMSOutraUF.Free;
  FICMSSN.Free;
  inherited;
end;

{ TInfCTeNorm }

constructor TInfCTeNorm.Create(AOwner: TCTe);
begin
  FcontQt    := TInfcontQtCollection.Create(Self);
  FemiDocAnt := TemiDocAntCollection.Create(Self);
end;

destructor TInfCTeNorm.Destroy;
begin
  FcontQt.Free;
  FemiDocAnt.Free;
  inherited;
end;

procedure TInfCTeNorm.SetcontQt(const Value: TInfcontQtCollection);
begin
  FcontQt.Assign(Value);
end;

procedure TInfCTeNorm.SetemiDocAnt(const Value: TemiDocAntCollection);
begin
 FemiDocAnt.Assign(Value);
end;

{ TInfCarga }
constructor TInfCarga.Create(AOwner: TCTe);
begin
  FInfQ := TInfQCollection.Create(self);
end;

destructor TInfCarga.Destroy;
begin
  FInfQ.Free;
  inherited;
end;

procedure TInfCarga.SetInfQ(const Value: TInfQCollection);
begin
  FInfQ.Assign(Value);
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

{ TInfcontQtCollection }

function TInfcontQtCollection.Add: TInfcontQtCollectionItem;
begin
  Result := TInfcontQtCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfcontQtCollection.Create(AOwner: TInfCTeNorm);
begin
  inherited Create(TInfcontQtCollectionItem);
end;

function TInfcontQtCollection.GetItem(
  Index: Integer): TInfcontQtCollectionItem;
begin
  Result := TInfcontQtCollectionItem(inherited GetItem(Index));
end;

procedure TInfcontQtCollection.SetItem(Index: Integer;
  Value: TInfcontQtCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfcontQtCollectionItem }

constructor TInfcontQtCollectionItem.Create;
begin
  FlacContQt := TlacContQtCollection.Create(Self);
end;

destructor TInfcontQtCollectionItem.Destroy;
begin
  FlacContQt.Free;
  inherited;
end;

procedure TInfcontQtCollectionItem.SetlacContQt(
  const Value: TlacContQtCollection);
begin
  FlacContQt.Assign(Value);
end;

{ TInflacContQtCollection }

function TlacContQtCollection.Add: TlacContQtCollectionItem;
begin
  Result := TlacContQtCollectionItem(inherited Add);
  Result.create;
end;

constructor TlacContQtCollection.Create(AOwner: TInfcontQtCollectionItem);
begin
  inherited Create(TlacContQtCollectionItem);
end;

function TlacContQtCollection.GetItem(
  Index: Integer): TlacContQtCollectionItem;
begin
  Result := TlacContQtCollectionItem(inherited GetItem(Index));
end;

procedure TlacContQtCollection.SetItem(Index: Integer;
  Value: TlacContQtCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInflacContQtCollectionItem }

constructor TlacContQtCollectionItem.Create;
begin

end;

destructor TlacContQtCollectionItem.Destroy;
begin

  inherited;
end;

{ TemiDocAntCollection }

function TemiDocAntCollection.Add: TemiDocAntCollectionItem;
begin
  Result := TemiDocAntCollectionItem(inherited Add);
  Result.create;
end;

constructor TemiDocAntCollection.Create(AOwner: TInfCTeNorm);
begin
  inherited Create(TemiDocAntCollectionItem);
end;

function TemiDocAntCollection.GetItem(
  Index: Integer): TemiDocAntCollectionItem;
begin
  Result := TemiDocAntCollectionItem(inherited GetItem(Index));
end;

procedure TemiDocAntCollection.SetItem(Index: Integer;
  Value: TemiDocAntCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TemiDocAntCollectionItem }

procedure TemiDocAntCollectionItem.SetidDocAnt(
  const Value: TidDocAntCollection);
begin
  FidDocAnt.Assign(Value);
end;

constructor TemiDocAntCollectionItem.Create;
begin
 FidDocAnt := TidDocAntCollection.Create(Self);
end;

destructor TemiDocAntCollectionItem.Destroy;
begin
  FidDocAnt.Free;
  inherited;
end;

{ TidDocAntCollection }

function TidDocAntCollection.Add: TidDocAntCollectionItem;
begin
  Result := TidDocAntCollectionItem(inherited Add);
  Result.create;
end;

constructor TidDocAntCollection.Create(AOwner: TemiDocAntCollectionItem);
begin
  inherited Create(TidDocAntCollectionItem);
end;

function TidDocAntCollection.GetItem(
  Index: Integer): TidDocAntCollectionItem;
begin
  Result := TidDocAntCollectionItem(inherited GetItem(Index));
end;

procedure TidDocAntCollection.SetItem(Index: Integer;
  Value: TidDocAntCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TidDocAntCollectionItem }

constructor TidDocAntCollectionItem.Create;
begin
 FidDocAntPap := TidDocAntPapCollection.Create(Self);
 FidDocAntEle := TidDocAntEleCollection.Create(Self);
end;

destructor TidDocAntCollectionItem.Destroy;
begin
  FidDocAntPap.Free;
  FidDocAntEle.Free;
  inherited;
end;

procedure TidDocAntCollectionItem.SetidDocAntPap(
  const Value: TidDocAntPapCollection);
begin
 FidDocAntPap.Assign(Value);
end;

procedure TidDocAntCollectionItem.SetidDocAntEle(
  const Value: TidDocAntEleCollection);
begin
 FidDocAntEle.Assign(Value);
end;

{ TidDocAntPapCollection }

function TidDocAntPapCollection.Add: TidDocAntPapCollectionItem;
begin
  Result := TidDocAntPapCollectionItem(inherited Add);
  Result.create;
end;

constructor TidDocAntPapCollection.Create(AOwner: TidDocAntCollectionItem);
begin
  inherited Create(TidDocAntPapCollectionItem);
end;

function TidDocAntPapCollection.GetItem(
  Index: Integer): TidDocAntPapCollectionItem;
begin
  Result := TidDocAntPapCollectionItem(inherited GetItem(Index));
end;

procedure TidDocAntPapCollection.SetItem(Index: Integer;
  Value: TidDocAntPapCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TidDocAntPapCollectionItem }

constructor TidDocAntPapCollectionItem.Create;
begin

end;

destructor TidDocAntPapCollectionItem.Destroy;
begin

  inherited;
end;

{ TidDocAntEleCollection }

function TidDocAntEleCollection.Add: TidDocAntEleCollectionItem;
begin
  Result := TidDocAntEleCollectionItem(inherited Add);
  Result.create;
end;

constructor TidDocAntEleCollection.Create(AOwner: TidDocAntCollectionItem);
begin
  inherited Create(TidDocAntEleCollectionItem);
end;

function TidDocAntEleCollection.GetItem(
  Index: Integer): TidDocAntEleCollectionItem;
begin
  Result := TidDocAntEleCollectionItem(inherited GetItem(Index));
end;

procedure TidDocAntEleCollection.SetItem(Index: Integer;
  Value: TidDocAntEleCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TidDocAntEleCollectionItem }

constructor TidDocAntEleCollectionItem.Create;
begin

end;

destructor TidDocAntEleCollectionItem.Destroy;
begin

  inherited;
end;

{ TInfSegCollection }

function TInfSegCollection.Add: TInfSegCollectionItem;
begin
  Result := TInfSegCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfSegCollection.Create(AOwner: TCTe);
begin
  inherited Create(TInfSegCollectionItem);
end;

function TInfSegCollection.GetItem(Index: Integer): TInfSegCollectionItem;
begin
  Result := TInfSegCollectionItem(inherited GetItem(Index));
end;

procedure TInfSegCollection.SetItem(Index: Integer; Value: TInfSegCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfSegCollectionItem }

constructor TInfSegCollectionItem.Create;
begin

end;

destructor TInfSegCollectionItem.Destroy;
begin

  inherited;
end;

{ TRodo }

constructor TRodo.Create(AOwner: TCTe);
begin
  FCTRB    := TCTRB.Create;
  FOcc     := TOccCollection.Create(Self);
  FvalePed := TValePed.Create(Self);
  Fveic    := TVeicCollection.Create(Self);
  FLacres  := TLacresCollection.Create(Self);
  Fmoto    := TMotoCollection.Create(Self);
end;

destructor TRodo.Destroy;
begin
  FCTRB.Free;
  FOcc.Free;
  FvalePed.Free;
  Fveic.Free;
  FLacres.Free;
  Fmoto.Free;
  inherited;
end;

procedure TRodo.SetOcc(const Value: TOccCollection);
begin
 FOcc.Assign(Value);
end;

procedure TRodo.SetVeic(const Value: TVeicCollection);
begin
 Fveic.Assign(Value);
end;

procedure TRodo.SetLacres(const Value: TLacresCollection);
begin
  FLacres.Assign(Value);
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
  FEmiOCC := TEmiOCC.Create;
end;

destructor TOccCollectionItem.Destroy;
begin
  FEmiOCC.Free;
  inherited;
end;

{ TValePed }

constructor TValePed.Create(AOwner: TRodo);
begin
  Fdisp := TDispCollection.Create(Self);
end;

destructor TValePed.Destroy;
begin
  Fdisp.Free;
  inherited;
end;

procedure TValePed.SetDisp(const Value: TDispCollection);
begin
  Fdisp.Assign(Value);
end;

{ TDispCollection }

function TDispCollection.Add: TDispCollectionItem;
begin
  Result := TDispCollectionItem(inherited Add);
  Result.create;
end;

constructor TDispCollection.Create(AOwner: TValePed);
begin
  inherited Create(TDispCollectionItem);
end;

function TDispCollection.GetItem(Index: Integer): TDispCollectionItem;
begin
  Result := TDispCollectionItem(inherited GetItem(Index));
end;

procedure TDispCollection.SetItem(Index: Integer;
  Value: TDispCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDispCollectionItem }

constructor TDispCollectionItem.Create;
begin

end;

destructor TDispCollectionItem.Destroy;
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
  FProp := TProp.Create;
end;

destructor TVeicCollectionItem.Destroy;
begin
  FProp.Free;
  inherited;
end;

{ TLacresCollection }

function TLacresCollection.Add: TLacresCollectionItem;
begin
  Result := TLacresCollectionItem(inherited Add);
  Result.create;
end;

constructor TLacresCollection.Create(AOwner: TRodo);
begin
  inherited Create(TLacresCollectionItem);
end;

function TLacresCollection.GetItem(Index: Integer): TLacresCollectionItem;
begin
  Result := TLacresCollectionItem(inherited GetItem(Index));
end;

procedure TLacresCollection.SetItem(Index: Integer;
  Value: TLacresCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TLacresCollectionItem }

constructor TLacresCollectionItem.Create;
begin

end;

destructor TLacresCollectionItem.Destroy;
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

{ Taereo }

constructor Taereo.Create(AOwner: TCTe);
begin
  Ftarifa := Ttarifa.Create;
end;

destructor Taereo.Destroy;
begin
  Ftarifa.Free;
  inherited;
end;

{ Taquav }

constructor Taquav.Create(AOwner: TCTe);
begin
 Flacre := TlacreCollection.Create(Self);
end;

destructor Taquav.Destroy;
begin
  Flacre.Free;
  inherited;
end;

procedure Taquav.SetLacre(const Value: TLacreCollection);
begin
  FLacre.Assign(Value);
end;

{ TbalsaCollection }

function TbalsaCollection.Add: TbalsaCollectionItem;
begin
  Result := TbalsaCollectionItem(inherited Add);
  Result.create;
end;

constructor TbalsaCollection.Create(AOwner: Taquav);
begin
  inherited Create(TbalsaCollectionItem);
end;

function TbalsaCollection.GetItem(Index: Integer): TbalsaCollectionItem;
begin
  Result := TbalsaCollectionItem(inherited GetItem(Index));
end;

procedure TbalsaCollection.SetItem(Index: Integer;
  Value: TbalsaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TbalsaCollectionItem }

constructor TbalsaCollectionItem.Create;
begin

end;

destructor TbalsaCollectionItem.Destroy;
begin

  inherited;
end;

{ TLacreCollection }

function TLacreCollection.Add: TLacreCollectionItem;
begin
  Result := TLacreCollectionItem(inherited Add);
  Result.create;
end;

constructor TLacreCollection.Create(AOwner: Taquav);
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

{ TdetContCollection }
{ TperiCollection }

function TperiCollection.Add: TperiCollectionItem;
begin
  Result := TperiCollectionItem(inherited Add);
  Result.create;
end;

constructor TperiCollection.Create(AOwner: TCTe);
begin
  inherited Create(TperiCollectionItem);
end;

function TperiCollection.GetItem(Index: Integer): TperiCollectionItem;
begin
  Result := TperiCollectionItem(inherited GetItem(Index));
end;

procedure TperiCollection.SetItem(Index: Integer;
  Value: TperiCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TperiCollectionItem }

constructor TperiCollectionItem.Create;
begin

end;

destructor TperiCollectionItem.Destroy;
begin

  inherited;
end;

{ TveicNovosCollection }

function TveicNovosCollection.Add: TveicNovosCollectionItem;
begin
  Result := TveicNovosCollectionItem(inherited Add);
  Result.create;
end;

constructor TveicNovosCollection.Create(AOwner: TCTe);
begin
  inherited Create(TveicNovosCollectionItem);
end;

function TveicNovosCollection.GetItem(
  Index: Integer): TveicNovosCollectionItem;
begin
  Result := TveicNovosCollectionItem(inherited GetItem(Index));
end;

procedure TveicNovosCollection.SetItem(Index: Integer;
  Value: TveicNovosCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TveicNovosCollectionItem }

constructor TveicNovosCollectionItem.Create;
begin

end;

destructor TveicNovosCollectionItem.Destroy;
begin

  inherited;
end;

{ TtomaICMS }

constructor TtomaICMS.Create(AOwner: TinfCteSub);
begin
  FrefNF := TrefNF.Create;
end;

destructor TtomaICMS.Destroy;
begin
  FrefNF.Free;
  inherited;
end;

{ TinfCTeSub }

constructor TinfCTeSub.Create(AOwner: TCTe);
begin
  FtomaICMS    := TtomaICMS.Create(Self);
  FtomaNaoICMS := TtomaNaoICMS.Create;
end;

destructor TinfCTeSub.Destroy;
begin
  FtomaICMS.Free;
  FtomaNaoICMS.Free;
  inherited;
end;

{ TinfCTeCompCollection }

function TinfCTeCompCollection.Add: TinfCTeCompCollectionItem;
begin
  Result := TinfCTeCompCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfCTeCompCollection.Create(AOwner: TCTe);
begin
  inherited Create(TinfCTeCompCollectionItem);
end;

function TinfCTeCompCollection.GetItem(
  Index: Integer): TinfCTeCompCollectionItem;
begin
  Result := TinfCTeCompCollectionItem(inherited GetItem(Index));
end;

procedure TinfCTeCompCollection.SetItem(Index: Integer;
  Value: TinfCTeCompCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfCTeCompCollectionItem }

constructor TinfCTeCompCollectionItem.Create;
begin
  FvPresComp := TvPresComp.Create(Self);
  FimpComp   := TimpComp.Create(Self);
end;

destructor TinfCTeCompCollectionItem.Destroy;
begin
  FvPresComp.Free;
  FimpComp.Free;
  inherited;
end;

{ TvPresComp }

procedure TvPresComp.SetcompCompItem(const Value: TcompCompCollection);
begin
  FcompComp.Assign(Value);
end;

constructor TvPresComp.Create(AOwner: TinfCTeCompCollectionItem);
begin
 FcompComp := TcompCompCollection.Create(Self);
end;

destructor TvPresComp.Destroy;
begin
  FcompComp.Free;
  inherited;
end;

{ TcompCompCollection }

function TcompCompCollection.Add: TcompCompCollectionItem;
begin
  Result := TcompCompCollectionItem(inherited Add);
  Result.create;
end;

constructor TcompCompCollection.Create(AOwner: TvPresComp);
begin
  inherited Create(TcompCompCollectionItem);
end;

function TcompCompCollection.GetItem(
  Index: Integer): TcompCompCollectionItem;
begin
  Result := TcompCompCollectionItem(inherited GetItem(Index));
end;

procedure TcompCompCollection.SetItem(Index: Integer;
  Value: TcompCompCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TcompCompCollectionItem }

constructor TcompCompCollectionItem.Create;
begin

end;

destructor TcompCompCollectionItem.Destroy;
begin

  inherited;
end;

{ TImpComp }

constructor TImpComp.Create(AOwner: TinfCTeCompCollectionItem);
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
  FCST00 := TCST00.create;
  FCST20 := TCST20.create;
  FCST45 := TCST45.create;
  FCST60 := TCST60.create;
  FCST80 := TCST80.create;
  FCST81 := TCST81.create;
  FCST90 := TCST90.create;
  FICMSOutraUF := TICMSOutraUF.Create;
  FICMSSN      := TICMSSN.Create;
end;

destructor TICMSComp.Destroy;
begin
  FCST00.Free;
  FCST20.Free;
  FCST45.Free;
  FCST60.Free;
  FCST80.Free;
  FCST81.Free;
  FCST90.Free;
  FICMSOutraUF.Free;
  FICMSSN.Free;
  inherited;
end;

{ TInfCTeAnuEnt }

constructor TInfCTeAnuEnt.Create(AOwner: TCTe);
begin
  inherited Create;
end;

destructor TInfCTeAnuEnt.Destroy;
begin
  inherited;
end;

{ Tferrov }

constructor Tferrov.Create(AOwner: TCTe);
begin
  inherited Create;
  FferroSub := TferroSub.Create(self);
  FDCL      := TDCLCollection.Create(Self);
  FdetVag   := TdetVagCollection.Create(Self);
end;

destructor Tferrov.Destroy;
begin
  FferroSub.Free;
  FDCL.Free;
  FdetVag.Free;
  inherited;
end;

procedure Tferrov.SetDCL(const Value: TDCLCollection);
begin
  FDCL.Assign(Value);
end;

procedure Tferrov.SetdetVag(const Value: TdetVagCollection);
begin
 FdetVag.Assign(Value);
end;

{ TferroSub }

constructor TferroSub.Create(AOwner: Tferrov);
begin
  inherited Create;
  FEnderFerro := TEnderFerro.Create;
end;

destructor TferroSub.Destroy;
begin
  FEnderFerro.Free;
  inherited;
end;

{ TDCLCollection }

function TDCLCollection.Add: TDCLCollectionItem;
begin
  Result := TDCLCollectionItem(inherited Add);
  Result.create;
end;

constructor TDCLCollection.Create(AOwner: Tferrov);
begin
  inherited Create(TDCLCollectionItem);
end;

function TDCLCollection.GetItem(Index: Integer): TDCLCollectionItem;
begin
  Result := TDCLCollectionItem(inherited GetItem(Index));
end;

procedure TDCLCollection.SetItem(Index: Integer;
  Value: TDCLCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDCLCollectionItem }

constructor TDCLCollectionItem.Create;
begin
  FdetVagDCL := TdetVagDCLCollection.Create(Self);
end;

destructor TDCLCollectionItem.Destroy;
begin
  FdetVagDCL.Free;
  inherited;
end;

procedure TDCLCollectionItem.SetdetVagDCL(
  const Value: TdetVagDCLCollection);
begin
  FdetVagDCL.Assign(Value);
end;

{ TdetVagDCLCollection }

function TdetVagDCLCollection.Add: TdetVagDCLCollectionItem;
begin
  Result := TdetVagDCLCollectionItem(inherited Add);
  Result.create;
end;

constructor TdetVagDCLCollection.Create(AOwner: TDCLCollectionItem);
begin
  inherited Create(TdetVagDCLCollectionItem);
end;

function TdetVagDCLCollection.GetItem(
  Index: Integer): TdetVagDCLCollectionItem;
begin
  Result := TdetVagDCLCollectionItem(inherited GetItem(Index));
end;

procedure TdetVagDCLCollection.SetItem(Index: Integer;
  Value: TdetVagDCLCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TdetVagDCLCollectionItem }

constructor TdetVagDCLCollectionItem.Create;
begin
  FlacDetVagDCL := TlacDetVagDCLCollection.Create(Self);
  FcontDCL      := TcontDCLCollection.Create(Self);
end;

destructor TdetVagDCLCollectionItem.Destroy;
begin
  FlacDetVagDCL.Free;
  FcontDCL.Free;
  inherited;
end;

procedure TdetVagDCLCollectionItem.SetlacDetVagDCL(
  const Value: TlacDetVagDCLCollection);
begin
 FlacDetVagDCL.Assign(Value);
end;

procedure TdetVagDCLCollectionItem.SetcontDCL(
  const Value: TcontDCLCollection);
begin
  FcontDCL.Assign(Value);
end;

{ TlacDetVagDCLCollection }

function TlacDetVagDCLCollection.Add: TlacDetVagDCLCollectionItem;
begin
  Result := TlacDetVagDCLCollectionItem(inherited Add);
  Result.create;
end;

constructor TlacDetVagDCLCollection.Create(
  AOwner: TdetVagDCLCollectionItem);
begin
  inherited Create(TlacDetVagDCLCollectionItem);
end;

function TlacDetVagDCLCollection.GetItem(
  Index: Integer): TlacDetVagDCLCollectionItem;
begin
  Result := TlacDetVagDCLCollectionItem(inherited GetItem(Index));
end;

procedure TlacDetVagDCLCollection.SetItem(Index: Integer;
  Value: TlacDetVagDCLCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TlacDetVagDCLCollectionItem }

constructor TlacDetVagDCLCollectionItem.Create;
begin

end;

destructor TlacDetVagDCLCollectionItem.Destroy;
begin

  inherited;
end;

{ TcontDCLCollection }

function TcontDCLCollection.Add: TcontDCLCollectionItem;
begin
  Result := TcontDCLCollectionItem(inherited Add);
  Result.create;
end;

constructor TcontDCLCollection.Create(AOwner: TdetVagDCLCollectionItem);
begin
  inherited Create(TcontDCLCollectionItem);
end;

function TcontDCLCollection.GetItem(
  Index: Integer): TcontDCLCollectionItem;
begin
  Result := TcontDCLCollectionItem(inherited GetItem(Index));
end;

procedure TcontDCLCollection.SetItem(Index: Integer;
  Value: TcontDCLCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TcontDCLCollectionItem }

constructor TcontDCLCollectionItem.Create;
begin

end;

destructor TcontDCLCollectionItem.Destroy;
begin

  inherited;
end;

{ TdetVagCollection }

function TdetVagCollection.Add: TdetVagCollectionItem;
begin
  Result := TdetVagCollectionItem(inherited Add);
  Result.create;
end;

constructor TdetVagCollection.Create(AOwner: Tferrov);
begin
  inherited Create(TdetVagCollectionItem);
end;

function TdetVagCollection.GetItem(Index: Integer): TdetVagCollectionItem;
begin
  Result := TdetVagCollectionItem(inherited GetItem(Index));
end;

procedure TdetVagCollection.SetItem(Index: Integer;
  Value: TdetVagCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TdetVagCollectionItem }

constructor TdetVagCollectionItem.Create;
begin
  FlacDetVag := TlacDetVagCollection.Create(Self);
  FcontVag   := TcontVagCollection.Create(Self);
end;

destructor TdetVagCollectionItem.Destroy;
begin
  FlacDetVag.Free;
  FcontVag.Free;
  inherited;
end;

procedure TdetVagCollectionItem.SetlacDetVag(
  const Value: TlacDetVagCollection);
begin
 FlacDetVag.Assign(Value);
end;

procedure TdetVagCollectionItem.SetcontVag(
  const Value: TcontVagCollection);
begin
 FcontVag.Assign(Value);
end;

{ TlacDetVagCollection }

function TlacDetVagCollection.Add: TlacDetVagCollectionItem;
begin
  Result := TlacDetVagCollectionItem(inherited Add);
  Result.create;
end;

constructor TlacDetVagCollection.Create(AOwner: TdetVagCollectionItem);
begin
  inherited Create(TlacDetVagCollectionItem);
end;

function TlacDetVagCollection.GetItem(
  Index: Integer): TlacDetVagCollectionItem;
begin
  Result := TlacDetVagCollectionItem(inherited GetItem(Index));
end;

procedure TlacDetVagCollection.SetItem(Index: Integer;
  Value: TlacDetVagCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TlacDetVagCollectionItem }

constructor TlacDetVagCollectionItem.Create;
begin

end;

destructor TlacDetVagCollectionItem.Destroy;
begin

  inherited;
end;

{ TcontVagCollection }

function TcontVagCollection.Add: TcontVagCollectionItem;
begin
  Result := TcontVagCollectionItem(inherited Add);
  Result.create;
end;

constructor TcontVagCollection.Create(AOwner: TdetVagCollectionItem);
begin
  inherited Create(TcontVagCollectionItem);
end;

function TcontVagCollection.GetItem(
  Index: Integer): TcontVagCollectionItem;
begin
  Result := TcontVagCollectionItem(inherited GetItem(Index));
end;

procedure TcontVagCollection.SetItem(Index: Integer;
  Value: TcontVagCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TcontVagCollectionItem }

constructor TcontVagCollectionItem.Create;
begin

end;

destructor TcontVagCollectionItem.Destroy;
begin

  inherited;
end;

{Cobr}

constructor TCobr.Create(AOwner: TCTe);
begin
  inherited Create;
  FFat := TFat.Create;
  FDup := TDupCollection.Create(self);
end;

destructor TCobr.Destroy;
begin
  FFat.Free;
  FDup.Free;
  inherited;
end;

procedure TCobr.SetDup(Value: TDupCollection);
begin
  FDup.Assign(Value);
end;

{ TDupCollection }

constructor TDupCollection.Create(AOwner: TCobr);
begin
  inherited Create(TDupCollectionItem);
end;

destructor TDupCollection.Destroy;
begin
  inherited;
end;

function TDupCollection.Add: TDupCollectionItem;
begin
  Result := TDupCollectionItem(inherited Add);
end;

function TDupCollection.GetItem(Index: Integer): TDupCollectionItem;
begin
  Result := TDupCollectionItem(inherited GetItem(Index));
end;

procedure TDupCollection.SetItem(Index: Integer; Value: TDupCollectionItem);
begin
  inherited SetItem(Index, Value);
end;
