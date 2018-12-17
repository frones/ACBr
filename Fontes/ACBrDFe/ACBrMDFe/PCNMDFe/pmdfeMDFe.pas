{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pmdfeMDFe;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnConversao, pmdfeConversaoMDFe, pcnSignature, pmdfeProcMDFe, pcnGerador;

type

  TinfMDFe                     = class;
  TinfMunCarregaCollection     = class;
  TinfMunCarregaCollectionItem = class;
  TinfPercursoCollection       = class;
  TinfPercursoCollectionItem   = class;
  Tide                         = class;
  Temit                        = class;
  TenderEmit                   = class;

  // Informações do modal Rodoviário
  Trodo                      = class;
  Tprop                      = class;
  TveicTracao                = class;
  TcondutorCollection        = class;
  TcondutorCollectionItem    = class;
  TveicReboqueCollection     = class;
  TveicReboqueCollectionItem = class;
  TvalePed                   = class;
  TdispCollection            = class;
  TdispCollectionItem        = class;
  TinfANTT                   = class;
  TinfCIOTCollection         = class;
  TinfCIOTCollectionItem     = class;
  TinfContratanteCollection     = class;
  TinfContratanteCollectionItem = class;
  TlacRodoCollection         = class;

  // Informações do modal Aéreo
  Taereo = class;

  // Informações do modal Aquaviário
  Taquav                           = class;
  TinfTermCarregCollection         = class;
  TinfTermCarregCollectionItem     = class;
  TinfTermDescarregCollection      = class;
  TinfTermDescarregCollectionItem  = class;
  TinfEmbCombCollection            = class;
  TinfEmbCombCollectionItem        = class;
  TinfUnidCargaVaziaCollection     = class;
  TinfUnidCargaVaziaCollectionItem = class;
  TinfUnidTranspVaziaCollection     = class;
  TinfUnidTranspVaziaCollectionItem = class;

  // Informações do modal Ferroviário
  Tferrov            = class;
  TvagCollection     = class;
  TvagCollectionItem = class;

  TinfDoc                       = class;
  TinfMunDescargaCollection     = class;
  TinfMunDescargaCollectionItem = class;

  TPeriCTeCollection            = class;
  TPeriNFeCollection            = class;
  TPeriMDFeCollection           = class;
  TPeriCollectionItem           = class;

  TinfCTeCollection           = class;
  TinfCTeCollectionItem       = class;
  TinfUnidTranspCTeCollection = class;

  TinfUnidTranspCollectionItem = class;
  TlacUnidTranspCollection     = class;
  TlacUnidTranspCollectionItem = class;
  TinfUnidCargaCollection      = class;
  TinfUnidCargaCollectionItem  = class;
  TlacUnidCargaCollection      = class;
  TlacUnidCargaCollectionItem  = class;

  TinfCTCollection           = class;
  TinfCTCollectionItem       = class;
  TinfUnidTranspCTCollection = class;

  TinfNFeCollection           = class;
  TinfNFeCollectionItem       = class;
  TinfUnidTranspNFeCollection = class;

  TinfNFCollection           = class;
  TinfNFCollectionItem       = class;
  TinfUnidTranspNFCollection = class;

  TinfMDFeTranspCollection     = class;
  TinfMDFeTranspCollectionItem = class;
  TinfUnidTranspMDFeCollection = class;

  TSegCollection     = class;
  TSegCollectionItem = class;

  TAverCollection     = class;
  TAverCollectionItem = class;

  Ttot                  = class;
  TlacresCollection     = class;
  TlacresCollectionItem = class;
  TautXMLCollection     = class;
  TautXMLCollectionItem = class;
  TinfAdic              = class;

  TMDFe = class;

  TinfMDFe = class(TPersistent)
  private
    FId: String;
    FVersao: Double;
    function GetVersaoStr: String;
  published
    property Id: String read FId write FId;
    property versao: Double read FVersao write FVersao;
    property VersaoStr: String read GetVersaoStr;
  end;

  TinfMunCarregaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfMunCarregaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfMunCarregaCollectionItem);
  public
    constructor Create(AOwner: Tide);
    function Add: TinfMunCarregaCollectionItem;
    property Items[Index: Integer]: TinfMunCarregaCollectionItem read GetItem write SetItem; default;
  end;

  TinfMunCarregaCollectionItem = class(TCollectionItem)
  private
    FcMunCarrega: Integer;
    FxMunCarrega: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property cMunCarrega: Integer read FcMunCarrega write FcMunCarrega;
    property xMunCarrega: String  read FxMunCarrega write FxMunCarrega;
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

  TIde = class(TPersistent)
  private
    FcUF: Integer;
    FtpAmb: TpcnTipoAmbiente;
    FtpEmit: TTpEmitenteMDFe;
    FtpTransp: TTransportadorMDFe;
    Fmod: String;
    Fserie: Integer;
    FnMDF: Integer;
    FcMDF: Integer;
    FcDV: Integer;
    Fmodal: TModalMDFe;
    FdhEmi: TDateTime;
    FtpEmis: TpcnTipoEmissao;
    FprocEmi: TpcnProcessoEmissao;
    FverProc: String;
    FUFIni: String;
    FUFFim: String;
    FinfMunCarrega: TinfMunCarregaCollection;
    FinfPercurso: TinfPercursoCollection;
    FdhIniViagem: TDateTime;
    FindCanalVerde: TIndicador;

    procedure SetinfMunCarrega(Value: TinfMunCarregaCollection);
    procedure SetinfPercurso(Value: TinfPercursoCollection);
  public
    constructor Create(AOwner: TMDFe);
    destructor Destroy; override;
  published
    property cUF: Integer                            read FcUF           write FcUF;
    property tpAmb: TpcnTipoAmbiente                 read FtpAmb         write FtpAmb;
    property tpEmit: TTpEmitenteMDFe                 read FtpEmit        write FtpEmit;
    property tpTransp: TTransportadorMDFe            read FtpTransp      write FtpTransp;
    property modelo: String                          read Fmod           write Fmod;
    property serie: Integer                          read Fserie         write Fserie;
    property nMDF: Integer                           read FnMDF          write FnMDF;
    property cMDF: Integer                           read FcMDF          write FcMDF;
    property cDV: Integer                            read FcDV           write FcDV;
    property modal: TModalMDFe                       read Fmodal         write Fmodal;
    property dhEmi: TDateTime                        read FdhEmi         write FdhEmi;
    property tpEmis: TpcnTipoEmissao                 read FtpEmis        write FtpEmis;
    property procEmi: TpcnProcessoEmissao            read FprocEmi       write FprocEmi;
    property verProc: String                         read FverProc       write FverProc;
    property UFIni: String                           read FUFIni         write FUFIni;
    property UFFim: String                           read FUFFim         write FUFFim;
    property infMunCarrega: TinfMunCarregaCollection read FinfMunCarrega write SetinfMunCarrega;
    property infPercurso: TinfPercursoCollection     read FinfPercurso   write SetinfPercurso;
    property dhIniViagem: TDateTime                  read FdhIniViagem   write FdhIniViagem;
    property indCanalVerde: TIndicador               read FindCanalVerde write FindCanalVerde default tiNao;
  end;

  Temit = class(TPersistent)
  private
    FCNPJCPF: String;
    FIE: String;
    FxNome: String;
    FxFant: String;
    FenderEmit: TenderEmit;
  public
    constructor Create(AOwner: TMDFe);
    destructor Destroy; override;
  published
    property CNPJCPF: String       read FCNPJCPF   write FCNPJCPF;
    property IE: String            read FIE        write FIE;
    property xNome: String         read FxNome     write FxNome;
    property xFant: String         read FxFant     write FxFant;
    property enderEmit: TenderEmit read FenderEmit write FenderEmit;
  end;

  TenderEmit = class(TPersistent)
  private
    FxLgr: String;
    Fnro: String;
    FxCpl: String;
    FxBairro: String;
    FcMun: Integer;
    FxMun: String;
    FCEP: Integer;
    FUF: String;
    Ffone: String;
    Femail: String;
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
    property email: String   read Femail   write Femail;
  end;

  TRodo = class(TPersistent)
  private
    FRNTRC: String;
    FCIOT: String;
    FinfANTT: TinfANTT;
    FveicTracao: TveicTracao;
    FveicReboque: TveicReboqueCollection;
    FvalePed: TvalePed;
    FcodAgPorto: String;
    FlacRodo: TlacRodoCollection;

    procedure SetveicReboque(const Value: TveicReboqueCollection);
    procedure SetlacRodo(const Value: TlacRodoCollection);
  public
    constructor Create(AOwner: TMDFe);
    destructor Destroy; override;
  published
    property RNTRC: String                       read FRNTRC       write FRNTRC;
    property CIOT: String                        read FCIOT        write FCIOT;
    property infANTT: TinfANTT                   read FinfANTT     write FinfANTT;
    property veicTracao: TveicTracao             read FveicTracao  write FveicTracao;
    property veicReboque: TveicReboqueCollection read FveicReboque write SetveicReboque;
    property valePed: TvalePed                   read FvalePed     write FvalePed;
    property codAgPorto: String                  read FcodAgPorto  write FcodAgPorto;
    property lacRodo: TlacRodoCollection         read FlacRodo     write SetlacRodo;
  end;

  TinfANTT    = class(TPersistent)
  private
    FRNTRC: String;
    FinfCIOT: TinfCIOTCollection;
    FinfContratante: TinfContratanteCollection;
    FvalePed: TvalePed;
    procedure SetinfCIOT(const Value: TinfCIOTCollection);
    procedure SetinfContratante(const Value: TinfContratanteCollection);


  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property RNTRC: String               read FRNTRC   write FRNTRC;
    property infCIOT: TinfCIOTCollection read FinfCIOT write SetinfCIOT;
    property infContratante: TinfContratanteCollection read FinfContratante write SetinfContratante;
    property valePed: TvalePed                   read FvalePed     write FvalePed;
    (*
    property cInt: String                  read FcInt     write FcInt;
    property placa: String                 read Fplaca    write Fplaca;
    property RENAVAM: String               read FRENAVAM  write FRENAVAM;
    property tara: Integer                 read Ftara     write Ftara;
    property capKG: Integer                read FcapKG    write FcapKG;
    property capM3: Integer                read FcapM3    write FcapM3;
    property prop: Tprop                   read Fprop     write Fprop;
    property condutor: TcondutorCollection read Fcondutor write Setcondutor;
    property tpRod: TpcteTipoRodado        read FtpRod    write FtpRod;
    property tpCar: TpcteTipoCarroceria    read FtpCar    write FtpCar;
    property UF: String                    read FUF       write FUF;
    *)
  end;

  TinfCIOTCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfCIOTCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfCIOTCollectionItem);
  public
    constructor Create(AOwner: TinfANTT);
    function Add: TinfCIOTCollectionItem;
    property Items[Index: Integer]: TinfCIOTCollectionItem read GetItem write SetItem; default;
  end;

  TinfCIOTCollectionItem = class(TCollectionItem)
  private
    FCIOT: String;
    FCNPJCPF: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CIOT: String    read FCIOT    write FCIOT;
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
  end;

  TinfContratanteCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfContratanteCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfContratanteCollectionItem);
  public
    constructor Create(AOwner: TinfANTT);
    function Add: TinfContratanteCollectionItem;
    property Items[Index: Integer]: TinfContratanteCollectionItem read GetItem write SetItem; default;
  end;

  TinfContratanteCollectionItem = class(TCollectionItem)
  private
    FCNPJCPF: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
  end;

  TveicTracao = class(TPersistent)
  private
    FcInt: String;
    Fplaca: String;
    FRENAVAM: String;
    Ftara: Integer;
    FcapKG: Integer;
    FcapM3: Integer;
    Fprop: Tprop;
    Fcondutor: TcondutorCollection;
    FtpRod: TpcteTipoRodado;
    FtpCar: TpcteTipoCarroceria;
    FUF: String;

    procedure Setcondutor(const Value: TcondutorCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property cInt: String                  read FcInt     write FcInt;
    property placa: String                 read Fplaca    write Fplaca;
    property RENAVAM: String               read FRENAVAM  write FRENAVAM;
    property tara: Integer                 read Ftara     write Ftara;
    property capKG: Integer                read FcapKG    write FcapKG;
    property capM3: Integer                read FcapM3    write FcapM3;
    property prop: Tprop                   read Fprop     write Fprop;
    property condutor: TcondutorCollection read Fcondutor write Setcondutor;
    property tpRod: TpcteTipoRodado        read FtpRod    write FtpRod;
    property tpCar: TpcteTipoCarroceria    read FtpCar    write FtpCar;
    property UF: String                    read FUF       write FUF;
  end;

  Tprop = class(TPersistent)
  private
    FCNPJCPF: String;
    FRNTRC: String;
    FxNome: String;
    FIE: String;
    FUF: String;
    FtpProp: TpcteProp;
  published
    property CNPJCPF: String   read FCNPJCPF write FCNPJCPF;
    property RNTRC: String     read FRNTRC   write FRNTRC;
    property xNome: String     read FxNome   write FxNome;
    property IE: String        read FIE      write FIE;
    property UF: String        read FUF      write FUF;
    property tpProp: TpcteProp read FtpProp  write FtpProp;
  end;

  TcondutorCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TcondutorCollectionItem;
    procedure SetItem(Index: Integer; Value: TcondutorCollectionItem);
  public
    constructor Create(AOwner: TveicTracao);
    function Add: TcondutorCollectionItem;
    property Items[Index: Integer]: TcondutorCollectionItem read GetItem write SetItem; default;
  end;

  TcondutorCollectionItem = class(TCollectionItem)
  private
    FxNome: String;
    FCPF: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property xNome: String read FxNome write FxNome;
    property CPF: String   read FCPF   write FCPF;
  end;

  TveicReboqueCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TveicReboqueCollectionItem;
    procedure SetItem(Index: Integer; Value: TveicReboqueCollectionItem);
  public
    constructor Create(AOwner: TRodo);
    function Add: TveicReboqueCollectionItem;
    property Items[Index: Integer]: TveicReboqueCollectionItem read GetItem write SetItem; default;
  end;

  TveicReboqueCollectionItem = class(TCollectionItem)
  private
    FcInt: String;
    Fplaca: String;
    FRENAVAM: String;
    Ftara: Integer;
    FcapKG: Integer;
    FcapM3: Integer;
    Fprop: Tprop;
    FtpCar: TpcteTipoCarroceria;
    FUF: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property cInt: String               read FcInt    write FcInt;
    property placa: String              read Fplaca   write Fplaca;
    property RENAVAM: String            read FRENAVAM write FRENAVAM;
    property tara: Integer              read Ftara    write Ftara;
    property capKG: Integer             read FcapKG   write FcapKG;
    property capM3: Integer             read FcapM3   write FcapM3;
    property prop: Tprop                read Fprop    write Fprop;
    property tpCar: TpcteTipoCarroceria read FtpCar   write FtpCar;
    property UF: String                 read FUF      write FUF;
  end;

  TvalePed = class(TPersistent)
  private
    Fdisp: TdispCollection;

    procedure Setdisp(const Value: TdispCollection);
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
  published
    property disp: TdispCollection read Fdisp write Setdisp;
  end;

  TdispCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TdispCollectionItem;
    procedure SetItem(Index: Integer; Value: TdispCollectionItem);
  public
    constructor Create(AOwner: TvalePed);
    function Add: TdispCollectionItem;
    property Items[Index: Integer]: TdispCollectionItem read GetItem write SetItem; default;
  end;

  TdispCollectionItem = class(TCollectionItem)
  private
    FCNPJForn: String;
    FCNPJPg: String;
    FnCompra: String;
    FvValePed: Double;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CNPJForn: String read FCNPJForn write FCNPJForn;
    property CNPJPg: String   read FCNPJPg   write FCNPJPg;
    property nCompra: String  read FnCompra  write FnCompra;
    property vValePed: Double read FvValePed write FvValePed;
  end;

  Taereo = class(TPersistent)
  private
    Fnac: Integer;
    Fmatr: Integer;
    FnVoo: String;
    FcAerEmb: String;
    FcAerDes: String;
    FdVoo: TDateTime;
  published
    property nac: Integer    read Fnac     write Fnac;
    property matr: Integer   read Fmatr    write Fmatr;
    property nVoo: String    read FnVoo    write FnVoo;
    property cAerEmb: String read FcAerEmb write FcAerEmb;
    property cAerDes: String read FcAerDes write FcAerDes;
    property dVoo: TDateTime read FdVoo    write FdVoo;
  end;

  Taquav = class(TPersistent)
  private
    FCNPJAgeNav: String;
    Firin : String;
    FtpEmb: String;
    FcEmbar: String;
    FxEmbar: String;
    FnViagem: String;
    FcPrtEmb: String;
    FcPrtDest: String;
    FprtTrans : String;
    FtpNav    : TTipoNavegacao;
    FinfTermCarreg: TinfTermCarregCollection;
    FinfTermDescarreg: TinfTermDescarregCollection;
    FinfEmbComb: TinfEmbCombCollection;
    FinfUnidCargaVazia: TinfUnidCargaVaziaCollection;
    FinfUnidTranspVazia: TinfUnidTranspVaziaCollection;

    procedure SetinfTermCarreg(const Value: TinfTermCarregCollection);
    procedure SetinfTermDescarreg(const Value: TinfTermDescarregCollection);
    procedure SetinfEmbComb(const Value: TinfEmbCombCollection);
    procedure SetinfUnidCargaVazia(const Value: TinfUnidCargaVaziaCollection);
    procedure SetinfUnidTranspVazia(const Value: TinfUnidTranspVaziaCollection);
  public
    constructor Create(AOwner: TMDFe);
    destructor Destroy; override;
  published
    property CNPJAgeNav: String                              read FCNPJAgeNav        write FCNPJAgeNav;
    property irin: String                                    read Firin              write Firin;
    property tpEmb: String                                   read FtpEmb             write FtpEmb;
    property cEmbar: String                                  read FcEmbar            write FcEmbar;
    property xEmbar: String                                  read FxEmbar            write FxEmbar;
    property nViagem: String                                 read FnViagem           write FnViagem;
    property cPrtEmb: String                                 read FcPrtEmb           write FcPrtEmb;
    property cPrtDest: String                                read FcPrtDest          write FcPrtDest;
    property prtTrans: String                                read FprtTrans          write FprtTrans;
    property tpNav: TTipoNavegacao                           read FtpNav             write FtpNav;
    property infTermCarreg: TinfTermCarregCollection         read FinfTermCarreg     write SetinfTermCarreg;
    property infTermDescarreg: TinfTermDescarregCollection   read FinfTermDescarreg  write SetinfTermDescarreg;
    property infEmbComb: TinfEmbCombCollection               read FinfEmbComb        write SetinfEmbComb;
    property infUnidCargaVazia: TinfUnidCargaVaziaCollection read FinfUnidCargaVazia write SetinfUnidCargaVazia;
    property infUnidTranspVazia: TinfUnidTranspVaziaCollection read FinfUnidTranspVazia write SetinfUnidTranspVazia;
  end;

  TinfTermCarregCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfTermCarregCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfTermCarregCollectionItem);
  public
    constructor Create(AOwner: Taquav);
    function Add: TinfTermCarregCollectionItem;
    property Items[Index: Integer]: TinfTermCarregCollectionItem read GetItem write SetItem; default;
  end;

  TinfTermCarregCollectionItem = class(TCollectionItem)
  private
    FcTermCarreg: String;
    FxTermCarreg: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property cTermCarreg: String read FcTermCarreg write FcTermCarreg;
    property xTermCarreg: String read FxTermCarreg write FxTermCarreg;
  end;

  TinfTermDescarregCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfTermDescarregCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfTermDescarregCollectionItem);
  public
    constructor Create(AOwner: Taquav);
    function Add: TinfTermDescarregCollectionItem;
    property Items[Index: Integer]: TinfTermDescarregCollectionItem read GetItem write SetItem; default;
  end;

  TinfTermDescarregCollectionItem = class(TCollectionItem)
  private
    FcTermDescarreg: String;
    FxTermDescarreg: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property cTermDescarreg: String read FcTermDescarreg write FcTermDescarreg;
    property xTermDescarreg: String read FxTermDescarreg write FxTermDescarreg;
  end;

  TinfEmbCombCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfEmbCombCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfEmbCombCollectionItem);
  public
    constructor Create(AOwner: Taquav);
    function Add: TinfEmbCombCollectionItem;
    property Items[Index: Integer]: TinfEmbCombCollectionItem read GetItem write SetItem; default;
  end;

  TinfEmbCombCollectionItem = class(TCollectionItem)
  private
    FcEmbComb: String;
    FxBalsa  : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property cEmbComb: String read FcEmbComb write FcEmbComb;
    property xBalsa: String read FxBalsa write FxBalsa;
  end;

  TinfUnidCargaVaziaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfUnidCargaVaziaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidCargaVaziaCollectionItem);
  public
    constructor Create(AOwner: Taquav);
    function Add: TinfUnidCargaVaziaCollectionItem;
    property Items[Index: Integer]: TinfUnidCargaVaziaCollectionItem read GetItem write SetItem; default;
  end;

  TinfUnidCargaVaziaCollectionItem = class(TCollectionItem)
  private
    FidUnidCargaVazia: String;
    FtpUnidCargaVazia: TpcnUnidCarga;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property idUnidCargaVazia: String        read FidUnidCargaVazia write FidUnidCargaVazia;
    property tpUnidCargaVazia: TpcnUnidCarga read FtpUnidCargaVazia write FtpUnidCargaVazia;
  end;

  TinfUnidTranspVaziaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfUnidTranspVaziaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidTranspVaziaCollectionItem);
  public
    constructor Create(AOwner: Taquav);
    function Add: TinfUnidTranspVaziaCollectionItem;
    property Items[Index: Integer]: TinfUnidTranspVaziaCollectionItem read GetItem write SetItem; default;
  end;

  TinfUnidTranspVaziaCollectionItem = class(TCollectionItem)
  private
    FidUnidTranspVazia: String;
    FtpUnidTranspVazia: TpcnUnidTransp;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property idUnidTranspVazia: String        read FidUnidTranspVazia write FidUnidTranspVazia;
    property tpUnidTranspVazia: TpcnUnidTransp read FtpUnidTranspVazia write FtpUnidTranspVazia;
  end;

  Tferrov = class(TPersistent)
  private
    FxPref: String;
    FdhTrem: TDateTime;
    FxOri: String;
    FxDest: String;
    FqVag: Integer;
    Fvag: TvagCollection;

    procedure Setvag(const Value: TvagCollection);
  public
    constructor Create(AOwner: TMDFe);
    destructor Destroy; override;
  published
    property xPref: String       read FxPref  write FxPref;
    property dhTrem: TDateTime   read FdhTrem write FdhTrem;
    property xOri: String        read FxOri   write FxOri;
    property xDest: String       read FxDest  write FxDest;
    property qVag: Integer       read FqVag   write FqVag;
    property vag: TvagCollection read Fvag    write Setvag;
  end;

  TvagCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TvagCollectionItem;
    procedure SetItem(Index: Integer; Value: TvagCollectionItem);
  public
    constructor Create(AOwner: Tferrov);
    function Add: TvagCollectionItem;
    property Items[Index: Integer]: TvagCollectionItem read GetItem write SetItem; default;
  end;

  TvagCollectionItem = class(TCollectionItem)
  private
    FpesoBC: Double;
    FpesoR: Double;
    FtpVag: String;
    Fserie: String;
    FnVag: Integer;
    FnSeq: Integer;
    FTU: Double;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property pesoBC: Double read FpesoBC write FpesoBC;
    property pesoR: Double  read FpesoR  write FpesoR;
    property tpVag: String  read FtpVag  write FtpVag;
    property serie: String  read Fserie  write Fserie;
    property nVag: Integer  read FnVag   write FnVag;
    property nSeq: Integer  read FnSeq   write FnSeq;
    property TU: Double     read FTU     write FTU;
  end;

  TPeriCTeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TPeriCollectionItem;
    procedure SetItem(Index: Integer; Value: TPeriCollectionItem);
  public
    constructor Create(AOwner:TinfCTeCollectionItem);
    function Add: TPeriCollectionItem;
    property Items[Index: Integer]: TPeriCollectionItem read GetItem write SetItem; default;
  end;

  TPeriNFeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TPeriCollectionItem;
    procedure SetItem(Index: Integer; Value: TPeriCollectionItem);
  public
    constructor Create(AOwner: TinfNFeCollectionItem);
    function Add: TPeriCollectionItem;
    property Items[Index: Integer]: TPeriCollectionItem read GetItem write SetItem; default;
  end;

  TPeriMDFeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TPeriCollectionItem;
    procedure SetItem(Index: Integer; Value: TPeriCollectionItem);
  public
    constructor Create(AOwner: TinfMDFeTranspCollectionItem);
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
  end;

  TinfDoc = class(TPersistent)
  private
    FinfMunDescarga: TinfMunDescargaCollection;

    procedure SetinfMunDescarga(const Value: TinfMunDescargaCollection);
  public
    constructor Create(AOwner: TMDFe);
    destructor Destroy; override;
  published
    property infMunDescarga: TinfMunDescargaCollection read FinfMunDescarga write SetinfMunDescarga;
  end;

  TinfMunDescargaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfMunDescargaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfMunDescargaCollectionItem);
  public
    constructor Create(AOwner: TinfDoc);
    function Add: TinfMunDescargaCollectionItem;
    property Items[Index: Integer]: TinfMunDescargaCollectionItem read GetItem write SetItem; default;
  end;

  TinfMunDescargaCollectionItem = class(TCollectionItem)
  private
    FcMunDescarga: Integer;
    FxMunDescarga: String;
    FinfCTe: TinfCTeCollection;
    FinfCT: TinfCTCollection;
    FinfNFe: TinfNFeCollection;
    FinfNF: TinfNFCollection;
    FinfMDFeTransp: TinfMDFeTranspCollection;

    procedure SetinfCTe(const Value: TinfCTeCollection);
    procedure SetinfCT(const Value: TinfCTCollection);
    procedure SetinfNFe(const Value: TinfNFeCollection);
    procedure SetinfNF(const Value: TinfNFCollection);
    procedure SetinfMDFeTransp(const Value: TinfMDFeTranspCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property cMunDescarga: Integer                   read FcMunDescarga  write FcMunDescarga;
    property xMunDescarga: String                    read FxMunDescarga  write FxMunDescarga;
    property infCTe: TinfCTeCollection               read FinfCTe        write SetinfCTe;
    property infCT: TinfCTCollection                 read FinfCT         write SetinfCT;
    property infNFe: TinfNFeCollection               read FinfNFe        write SetinfNFe;
    property infNF: TinfNFCollection                 read FinfNF         write SetinfNF;
    property infMDFeTransp: TinfMDFeTranspCollection read FinfMDFeTransp write SetinfMDFeTransp;
  end;

  TinfEntregaParcial = class(TPersistent)
  private
    FqtdTotal: Double;
    FqtdParcial: Double;
  published
    property qtdTotal: Double read FqtdTotal write FqtdTotal;
    property qtdParcial: Double read FqtdParcial write FqtdParcial;
  end;

  TinfCTeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfCTeCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfCTeCollectionItem);
  public
    constructor Create(AOwner: TinfMunDescargaCollectionItem);
    function Add: TinfCTeCollectionItem;
    property Items[Index: Integer]: TinfCTeCollectionItem read GetItem write SetItem; default;
  end;

  TinfCTeCollectionItem = class(TCollectionItem)
  private
    FchCTe: String;
    FSegCodBarra: String;
    FindReentrega: String;
    FinfUnidTransp: TinfUnidTranspCTeCollection;
    Fperi: TPeriCTeCollection;
    FinfEntregaParcial: TinfEntregaParcial;

    procedure SetinfUnidTransp(const Value: TinfUnidTranspCTeCollection);
    procedure SetPeri(const Value: TPeriCTeCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property chCTe: String                              read FchCTe             write FchCTe;
    property SegCodBarra: String                        read FSegCodBarra       write FSegCodBarra;
    property indReentrega: String                       read FindReentrega      write FindReentrega;
    property infUnidTransp: TinfUnidTranspCTeCollection read FinfUnidTransp     write SetinfUnidTransp;
    property peri: TPeriCTeCollection                   read Fperi              write SetPeri;
    property infEntregaParcial: TinfEntregaParcial      read FinfEntregaParcial write FinfEntregaParcial;
  end;

  TinfUnidTranspCTeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfUnidTranspCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidTranspCollectionItem);
  public
    constructor Create(AOwner: TinfCTeCollectionItem);
    function Add: TinfUnidTranspCollectionItem;
    property Items[Index: Integer]: TinfUnidTranspCollectionItem read GetItem write SetItem; default;
  end;

  TinfUnidTranspCollectionItem = class(TCollectionItem)
  private
    FtpUnidTransp: TpcnUnidTransp;
    FidUnidTransp: String;
    FlacUnidTransp: TlacUnidTranspCollection;
    FinfUnidCarga: TinfUnidCargaCollection;
    FqtdRat: Double;

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
    FnLacre: String;
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
    FtpUnidCarga: TpcnUnidCarga;
    FidUnidCarga: String;
    FlacUnidCarga: TlacUnidCargaCollection;
    FqtdRat: Double;

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
    FnLacre: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nLacre: String read FnLacre write FnLacre;
  end;

  TinfCTCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfCTCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfCTCollectionItem);
  public
    constructor Create(AOwner: TinfMunDescargaCollectionItem);
    function Add: TinfCTCollectionItem;
    property Items[Index: Integer]: TinfCTCollectionItem read GetItem write SetItem; default;
  end;

  TinfCTCollectionItem = class(TCollectionItem)
  private
    FnCT: String;
    Fserie: Integer;
    Fsubser: Integer;
    FdEmi: TDateTime;
    FvCarga: Double;
    FinfUnidTransp: TinfUnidTranspCTCollection;

    procedure SetinfUnidTransp(const Value: TinfUnidTranspCTCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nCT: String                               read FnCT           write FnCT;
    property serie: Integer                            read Fserie         write Fserie;
    property subser: Integer                           read Fsubser        write Fsubser;
    property dEmi: TDateTime                           read FdEmi          write FdEmi;
    property vCarga: Double                            read FvCarga        write FvCarga;
    property infUnidTransp: TinfUnidTranspCTCollection read FinfUnidTransp write SetinfUnidTransp;
  end;

  TinfUnidTranspCTCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfUnidTranspCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidTranspCollectionItem);
  public
    constructor Create(AOwner: TinfCTCollectionItem);
    function Add: TinfUnidTranspCollectionItem;
    property Items[Index: Integer]: TinfUnidTranspCollectionItem read GetItem write SetItem; default;
  end;

  TinfNFeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfNFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfNFeCollectionItem);
  public
    constructor Create(AOwner: TinfMunDescargaCollectionItem);
    function Add: TinfNFeCollectionItem;
    property Items[Index: Integer]: TinfNFeCollectionItem read GetItem write SetItem; default;
  end;

  TinfNFeCollectionItem = class(TCollectionItem)
  private
    FchNFe: String;
    FSegCodBarra: String;
    FindReentrega: String;
    FinfUnidTransp: TinfUnidTranspNFeCollection;
    Fperi: TPeriNFeCollection;

    procedure SetinfUnidTransp(const Value: TinfUnidTranspNFeCollection);
    procedure SetPeri(const Value: TPeriNFeCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property chNFe: String                              read FchNFe         write FchNFe;
    property SegCodBarra: String                        read FSegCodBarra   write FSegCodBarra;
    property indReentrega: String                       read FindReentrega  write FindReentrega;
    property infUnidTransp: TinfUnidTranspNFeCollection read FinfUnidTransp write SetinfUnidTransp;
    property peri: TPeriNFeCollection                   read Fperi          write SetPeri;
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

  TinfNFCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfNFCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfNFCollectionItem);
  public
    constructor Create(AOwner: TinfMunDescargaCollectionItem);
    function Add: TinfNFCollectionItem;
    property Items[Index: Integer]: TinfNFCollectionItem read GetItem write SetItem; default;
  end;

  TinfNFCollectionItem = class(TCollectionItem)
  private
    FCNPJ: String;
    FUF: String;
    FnNF: Integer;
    Fserie: Integer;
    FdEmi: TDateTime;
    FvNF: Double;
    FPIN: Integer;
    FinfUnidTransp: TinfUnidTranspNFCollection;

    procedure SetinfUnidTransp(const Value: TinfUnidTranspNFCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CNPJ: String                              read FCNPJ          write FCNPJ;
    property UF: String                                read FUF            write FUF;
    property nNF: Integer                              read FnNF           write FnNF;
    property serie: Integer                            read Fserie         write Fserie;
    property dEmi: TDateTime                           read FdEmi          write FdEmi;
    property vNF: Double                               read FvNF           write FvNF;
    property PIN: Integer                              read FPIN           write FPIN;
    property infUnidTransp: TinfUnidTranspNFCollection read FinfUnidTransp write SetinfUnidTransp;
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

  TinfMDFeTranspCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfMDFeTranspCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfMDFeTranspCollectionItem);
  public
    constructor Create(AOwner: TinfMunDescargaCollectionItem);
    function Add: TinfMDFeTranspCollectionItem;
    property Items[Index: Integer]: TinfMDFeTranspCollectionItem read GetItem write SetItem; default;
  end;

  TinfMDFeTranspCollectionItem = class(TCollectionItem)
  private
    FchMDFe: String;
    FindReentrega: String;
    FinfUnidTransp: TinfUnidTranspMDFeCollection;
    Fperi: TPeriMDFeCollection;

    procedure SetinfUnidTransp(const Value: TinfUnidTranspMDFeCollection);
    procedure SetPeri(const Value: TPeriMDFeCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property chMDFe: String                              read FchMDFe        write FchMDFe;
    property indReentrega: String                        read FindReentrega  write FindReentrega;
    property infUnidTransp: TinfUnidTranspMDFeCollection read FinfUnidTransp write SetinfUnidTransp;
    property peri: TPeriMDFeCollection                   read Fperi          write SetPeri;
  end;

  TinfUnidTranspMDFeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfUnidTranspCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidTranspCollectionItem);
  public
    constructor Create(AOwner: TinfMDFeTranspCollectionItem);
    function Add: TinfUnidTranspCollectionItem;
    property Items[Index: Integer]: TinfUnidTranspCollectionItem read GetItem write SetItem; default;
  end;

  TSegCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TSegCollectionItem;
    procedure SetItem(Index: Integer; Value: TSegCollectionItem);
  public
    constructor Create(AOwner: TMDFe);
    function Add: TSegCollectionItem;
    property Items[Index: Integer]: TSegCollectionItem read GetItem write SetItem; default;
  end;

  TSegCollectionItem = class(TCollectionItem)
  private
    FrespSeg: TRspSegMDFe;
    FCNPJCPF: String;
    FxSeg: String;
    FCNPJ: String;
    FnApol: String;
    FAver: TAverCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property respSeg: TRspSegMDFe read FrespSeg write FrespSeg;
    property CNPJCPF: String      read FCNPJCPF write FCNPJCPF;
    property xSeg: String         read FxSeg    write FxSeg;
    property CNPJ: String         read FCNPJ    write FCNPJ;
    property nApol: String        read FnApol   write FnApol;
    property aver: TAverCollection read FAver   write FAver;
  end;

  TAverCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TAverCollectionItem;
    procedure SetItem(Index: Integer; Value: TAverCollectionItem);
  public
    constructor Create(AOwner: TSegCollectionItem);
    function Add: TAverCollectionItem;
    property Items[Index: Integer]: TAverCollectionItem read GetItem write SetItem; default;
  end;

  TAverCollectionItem = class(TCollectionItem)
  private
    FnAver: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nAver: String        read FnAver   write FnAver;
  end;

  Ttot = class(TPersistent)
  private
    FqCTe: Integer;
    FqCT: Integer;
    FqNFe: Integer;
    FqNF: Integer;
    FqMDFe: Integer;
    FvCarga: Double;
    FcUnid: TUnidMed;
    FqCarga: Double;
  published
    property qCTe: Integer   read FqCTe   write FqCTe;
    property qCT: Integer    read FqCT    write FqCT;
    property qNFe: Integer   read FqNFe   write FqNFe;
    property qNF: Integer    read FqNF    write FqNF;
    property qMDFe: Integer  read FqMDFe  write FqMDFe;
    property vCarga: Double  read FvCarga write FvCarga;
    property cUnid: TUnidMed read FcUnid  write FcUnid;
    property qCarga: Double  read FqCarga write FqCarga;
  end;

  TlacRodoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TlacresCollectionItem;
    procedure SetItem(Index: Integer; Value: TlacresCollectionItem);
  public
    constructor Create(AOwner: TRodo);
    function Add: TlacresCollectionItem;
    property Items[Index: Integer]: TlacresCollectionItem read GetItem write SetItem; default;
  end;

  TlacresCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TlacresCollectionItem;
    procedure SetItem(Index: Integer; Value: TlacresCollectionItem);
  public
    constructor Create(AOwner: TMDFe);
    function Add: TlacresCollectionItem;
    property Items[Index: Integer]: TlacresCollectionItem read GetItem write SetItem; default;
  end;

  TlacresCollectionItem = class(TCollectionItem)
  private
    FnLacre: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nLacre: String read FnLacre write FnLacre;
  end;

  TautXMLCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TautXMLCollectionItem;
    procedure SetItem(Index: Integer; Value: TautXMLCollectionItem);
  public
    constructor Create(AOwner: TMDFe);
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

  TinfAdic = class(TPersistent)
  private
    FinfAdFisco: String;
    FinfCpl: String;
  published
    property infAdFisco: String read FinfAdFisco write FinfAdFisco;
    property infCpl: String     read FinfCpl     write FinfCpl;
  end;

  TinfRespTec = class(TPersistent)
  private
    FCNPJ: String;
    FxContato: String;
    Femail: String;
    Ffone: String;
    FidCSRT: Integer;
    FhashCSRT: String;
  published
    property CNPJ: String     read FCNPJ     write FCNPJ;
    property xContato: String read FxContato write FxContato;
    property email: String    read Femail    write Femail;
    property fone: String     read Ffone     write Ffone;
    property idCSRT: Integer  read FidCSRT   write FidCSRT;
    property hashCSRT: String read FhashCSRT write FhashCSRT;
 end;

  TMDFe = class(TPersistent)
  private
    FinfMDFe: TinfMDFe;
    FIde: TIde;
    Femit: Temit;

    Frodo: Trodo;
    Faereo: Taereo;
    Faquav: Taquav;
    Fferrov: Tferrov;

    FinfDoc: TinfDoc;
    Fseg: TSegCollection;
    Ftot: Ttot;
    Flacres: TlacresCollection;
    FautXML: TautXMLCollection;
    FinfAdic: TinfAdic;
    FinfRespTec: TinfRespTec;

    FProcMDFe: TProcMDFe;
    FSignature: TSignature;

    procedure Setlacres(const Value: TlacresCollection);
    procedure SetautXML(const Value: TautXMLCollection);
    procedure SetSeg(const Value: TSegCollection);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property infMDFe: TinfMDFe  read FinfMDFe write FinfMDFe;
    property Ide: TIde          read FIde     write FIde;
    property emit: Temit        read Femit    write Femit;

    property rodo: Trodo     read Frodo   write Frodo;
    property aereo: Taereo   read Faereo  write Faereo;
    property aquav: Taquav   read Faquav  write Faquav;
    property ferrov: Tferrov read Fferrov write Fferrov;

    property infDoc: TinfDoc           read FinfDoc  write FinfDoc;
    property seg: TSegCollection       read Fseg     write SetSeg;
    property tot: Ttot                 read Ftot     write Ftot;
    property lacres: TlacresCollection read Flacres  write Setlacres;
    property autXML: TautXMLCollection read FautXML  write SetautXML;
    property infAdic: TinfAdic         read FinfAdic write FinfAdic;

    property infRespTec: TinfRespTec read FinfRespTec write FinfRespTec;

    property procMDFe: TProcMDFe   read FProcMDFe  write FProcMDFe;
    property signature: Tsignature read Fsignature write Fsignature;
  end;

const
  CMUN_EXTERIOR: Integer = 9999999;
  XMUN_EXTERIOR: String = 'EXTERIOR';
  UF_EXTERIOR: String = 'EX';

implementation

Uses
  ACBrUtil;

{ TMDFe }

constructor TMDFe.Create;
begin
  FinfMDFe := TInfMDFe.Create;
  Fide     := Tide.Create(Self);
  Femit    := Temit.Create(Self);

  Frodo   := Trodo.Create(Self);
  Faereo  := Taereo.Create;
  Faquav  := Taquav.Create(Self);
  Fferrov := Tferrov.Create(Self);

  FinfDoc  := TinfDoc.Create(Self);
  Fseg     := TSegCollection.Create(Self);
  Ftot     := Ttot.Create;
  Flacres  := TlacresCollection.Create(Self);
  FautXML  := TautXMLCollection.Create(Self);
  FinfAdic := TinfAdic.Create;

  FinfRespTec := TinfRespTec.Create;

  FProcMDFe  := TProcMDFe.create;
  Fsignature := Tsignature.create;
end;

destructor TMDFe.Destroy;
begin
  FinfMDFe.Free;
  Fide.Free;
  Femit.Free;

  Frodo.Free;
  Faereo.Free;
  Faquav.Free;
  Fferrov.Free;

  FinfDoc.Free;
  Fseg.Free;
  Ftot.Free;
  Flacres.Free;
  FautXML.Free;
  FinfAdic.Free;
  FinfRespTec.Free;

  FProcMDFe.Free;
  Fsignature.Free;
  inherited;
end;

procedure TMDFe.Setlacres(const Value: TlacresCollection);
begin
  Flacres.Assign(Value);
end;

procedure TMDFe.SetautXML(const Value: TautXMLCollection);
begin
  FautXML := Value;
end;

procedure TMDFe.SetSeg(const Value: TSegCollection);
begin
  Fseg := Value;
end;

{ TinfMunCarregaCollection }

function TinfMunCarregaCollection.Add: TinfMunCarregaCollectionItem;
begin
  Result := TinfMunCarregaCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfMunCarregaCollection.Create(AOwner: Tide);
begin
  inherited Create(TinfMunCarregaCollectionItem);
end;

function TinfMunCarregaCollection.GetItem(Index: Integer): TinfMunCarregaCollectionItem;
begin
  Result := TinfMunCarregaCollectionItem(inherited GetItem(Index));
end;

procedure TinfMunCarregaCollection.SetItem(Index: Integer; Value: TinfMunCarregaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfMunCarregaCollectionItem }

constructor TinfMunCarregaCollectionItem.Create;
begin

end;

destructor TinfMunCarregaCollectionItem.Destroy;
begin

  inherited;
end;

{ TIde }

constructor TIde.Create(AOwner: TMDFe);
begin
  inherited Create;
  FinfMunCarrega := TinfMunCarregaCollection.Create(Self);
  FinfPercurso   := TinfPercursoCollection.Create(Self);
  FindCanalVerde := tiNao;
end;

destructor TIde.Destroy;
begin
  FinfMunCarrega.Free;
  FinfPercurso.Free;
  inherited;
end;

procedure TIde.SetinfMunCarrega(Value: TinfMunCarregaCollection);
begin
  FinfMunCarrega.Assign(Value);
end;

procedure TIde.SetinfPercurso(Value: TinfPercursoCollection);
begin
  FinfPercurso.Assign(Value);
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

{ Temit }

constructor Temit.Create(AOwner: TMDFe);
begin
  inherited Create;
  FenderEmit := TenderEmit.Create;
end;

destructor Temit.Destroy;
begin
  FenderEmit.Free;
  inherited;
end;

{ TRodo }

constructor TRodo.Create(AOwner: TMDFe);
begin
  inherited Create;
  FinfANTT     := TinfANTT.Create;
  FveicTracao  := TveicTracao.Create;
  FveicReboque := TveicReboqueCollection.Create(Self);
  FvalePed     := TvalePed.Create(Self);
  FlacRodo     := TlacRodoCollection.Create(Self);
end;

destructor TRodo.Destroy;
begin
  FinfANTT.Free;
  FveicTracao.Free;
  FveicReboque.Free;
  FvalePed.Free;
  FlacRodo.Free;
  inherited;
end;

procedure TRodo.SetlacRodo(const Value: TlacRodoCollection);
begin
  FlacRodo := Value;
end;

procedure TRodo.SetveicReboque(const Value: TveicReboqueCollection);
begin
  FveicReboque.Assign(Value);
end;

{ TveicTracao }

constructor TveicTracao.Create;
begin
  inherited Create;
  Fprop     := Tprop.Create;
  Fcondutor := TcondutorCollection.Create(Self);
end;

destructor TveicTracao.Destroy;
begin
  Fprop.Free;
  Fcondutor.Free;
  inherited;
end;

procedure TveicTracao.Setcondutor(const Value: TcondutorCollection);
begin
  Fcondutor.Assign(Value);
end;

{ TcondutorCollection }

function TcondutorCollection.Add: TcondutorCollectionItem;
begin
  Result := TcondutorCollectionItem(inherited Add);
  Result.create;
end;

constructor TcondutorCollection.Create(AOwner: TveicTracao);
begin
  inherited Create(TcondutorCollectionItem);
end;

function TcondutorCollection.GetItem(
  Index: Integer): TcondutorCollectionItem;
begin
  Result := TcondutorCollectionItem(inherited GetItem(Index));
end;

procedure TcondutorCollection.SetItem(Index: Integer;
  Value: TcondutorCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TcondutorCollectionItem }

constructor TcondutorCollectionItem.Create;
begin

end;

destructor TcondutorCollectionItem.Destroy;
begin

  inherited;
end;

{ TveicReboqueCollection }

function TveicReboqueCollection.Add: TveicReboqueCollectionItem;
begin
  Result := TveicReboqueCollectionItem(inherited Add);
  Result.create;
end;

constructor TveicReboqueCollection.Create(AOwner: TRodo);
begin
  inherited Create(TveicReboqueCollectionItem);
end;

function TveicReboqueCollection.GetItem(
  Index: Integer): TveicReboqueCollectionItem;
begin
  Result := TveicReboqueCollectionItem(inherited GetItem(Index));
end;

procedure TveicReboqueCollection.SetItem(Index: Integer;
  Value: TveicReboqueCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TveicReboqueCollectionItem }

constructor TveicReboqueCollectionItem.Create;
begin
  Fprop := Tprop.Create;
end;

destructor TveicReboqueCollectionItem.Destroy;
begin
  Fprop.Free;
  inherited;
end;

{ TvalePed }

constructor TvalePed.Create(AOwner: TObject);
begin
  inherited Create;
  Fdisp := TdispCollection.Create(Self);
end;

destructor TvalePed.Destroy;
begin
  Fdisp.Free;
  inherited;
end;

procedure TvalePed.Setdisp(const Value: TdispCollection);
begin
  Fdisp.Assign(Value);
end;

{ TdispCollection }

function TdispCollection.Add: TdispCollectionItem;
begin
  Result := TdispCollectionItem(inherited Add);
  Result.create;
end;

constructor TdispCollection.Create(AOwner: TvalePed);
begin
  inherited Create(TdispCollectionItem);
end;

function TdispCollection.GetItem(Index: Integer): TdispCollectionItem;
begin
  Result := TdispCollectionItem(inherited GetItem(Index));
end;

procedure TdispCollection.SetItem(Index: Integer;
  Value: TdispCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TdispCollectionItem }

constructor TdispCollectionItem.Create;
begin

end;

destructor TdispCollectionItem.Destroy;
begin

  inherited;
end;

{ Tferrov }

constructor Tferrov.Create(AOwner: TMDFe);
begin
  inherited Create;
  Fvag := TvagCollection.Create(Self);
end;

destructor Tferrov.Destroy;
begin
  Fvag.Free;
  inherited;
end;

procedure Tferrov.Setvag(const Value: TvagCollection);
begin
  Fvag.Assign(Value);
end;

{ TvagCollection }

function TvagCollection.Add: TvagCollectionItem;
begin
  Result := TvagCollectionItem(inherited Add);
  Result.create;
end;

constructor TvagCollection.Create(AOwner: Tferrov);
begin
  inherited Create(TvagCollectionItem);
end;

function TvagCollection.GetItem(Index: Integer): TvagCollectionItem;
begin
  Result := TvagCollectionItem(inherited GetItem(Index));
end;

procedure TvagCollection.SetItem(Index: Integer;
  Value: TvagCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TvagCollectionItem }

constructor TvagCollectionItem.Create;
begin

end;

destructor TvagCollectionItem.Destroy;
begin

  inherited;
end;

{ TinfDoc }

constructor TinfDoc.Create(AOwner: TMDFe);
begin
  inherited Create;
  FinfMunDescarga := TinfMunDescargaCollection.Create(Self);
end;

destructor TinfDoc.Destroy;
begin
  FinfMunDescarga.Free;
  inherited;
end;

procedure TinfDoc.SetinfMunDescarga(
  const Value: TinfMunDescargaCollection);
begin
  FinfMunDescarga.Assign(Value);
end;

{ TinfMunDescargaCollection }

function TinfMunDescargaCollection.Add: TinfMunDescargaCollectionItem;
begin
  Result := TinfMunDescargaCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfMunDescargaCollection.Create(AOwner: TinfDoc);
begin
  inherited Create(TinfMunDescargaCollectionItem);
end;

function TinfMunDescargaCollection.GetItem(
  Index: Integer): TinfMunDescargaCollectionItem;
begin
  Result := TinfMunDescargaCollectionItem(inherited GetItem(Index));
end;

procedure TinfMunDescargaCollection.SetItem(Index: Integer;
  Value: TinfMunDescargaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfMunDescargaCollectionItem }

constructor TinfMunDescargaCollectionItem.Create;
begin
  FinfCTe        := TinfCTeCollection.Create(Self);
  FinfCT         := TinfCTCollection.Create(Self);
  FinfNFe        := TinfNFeCollection.Create(Self);
  FinfNF         := TinfNFCollection.Create(Self);
  FinfMDFeTransp := TinfMDFeTranspCollection.Create(Self);
end;

destructor TinfMunDescargaCollectionItem.Destroy;
begin
  FinfCTe.Free;
  FinfCT.Free;
  FinfNFe.Free;
  FinfNF.Free;
  FinfMDFeTransp.Free;
  inherited;
end;

procedure TinfMunDescargaCollectionItem.SetinfCTe(
  const Value: TinfCTeCollection);
begin
  FinfCTe.Assign(Value);
end;

procedure TinfMunDescargaCollectionItem.SetinfCT(
  const Value: TinfCTCollection);
begin
  FinfCT.Assign(Value);
end;

procedure TinfMunDescargaCollectionItem.SetinfNFe(
  const Value: TinfNFeCollection);
begin
  FinfNFe.Assign(Value);
end;

procedure TinfMunDescargaCollectionItem.SetinfNF(
  const Value: TinfNFCollection);
begin
  FinfNF.Assign(Value);
end;

procedure TinfMunDescargaCollectionItem.SetinfMDFeTransp(
  const Value: TinfMDFeTranspCollection);
begin
  FinfMDFeTransp := Value;
end;

{ TinfCTeCollection }

function TinfCTeCollection.Add: TinfCTeCollectionItem;
begin
  Result := TinfCTeCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfCTeCollection.Create(
  AOwner: TinfMunDescargaCollectionItem);
begin
  inherited Create(TinfCTeCollectionItem);
end;

function TinfCTeCollection.GetItem(Index: Integer): TinfCTeCollectionItem;
begin
  Result := TinfCTeCollectionItem(inherited GetItem(Index));
end;

procedure TinfCTeCollection.SetItem(Index: Integer;
  Value: TinfCTeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfCTeCollectionItem }

constructor TinfCTeCollectionItem.Create;
begin
  FinfUnidTransp := TInfUnidTranspCTeCollection.Create(Self);
  FPeri := TPeriCTeCollection.Create(Self);
  FinfEntregaParcial := TinfEntregaParcial.Create;
end;

destructor TinfCTeCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  FPeri.Free;
  FinfEntregaParcial.Free;

  inherited;
end;

procedure TinfCTeCollectionItem.SetinfUnidTransp(
  const Value: TinfUnidTranspCTeCollection);
begin
  FinfUnidTransp := Value;
end;

procedure TinfCTeCollectionItem.SetPeri(const Value: TPeriCTeCollection);
begin
  Fperi := Value;
end;

{ TinfCTCollection }

function TinfCTCollection.Add: TinfCTCollectionItem;
begin
  Result := TinfCTCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfCTCollection.Create(AOwner: TinfMunDescargaCollectionItem);
begin
  inherited Create(TinfCTCollectionItem);
end;

function TinfCTCollection.GetItem(Index: Integer): TinfCTCollectionItem;
begin
  Result := TinfCTCollectionItem(inherited GetItem(Index));
end;

procedure TinfCTCollection.SetItem(Index: Integer;
  Value: TinfCTCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfCTCollectionItem }

constructor TinfCTCollectionItem.Create;
begin
  FinfUnidTransp := TInfUnidTranspCTCollection.Create(Self);
end;

destructor TinfCTCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  inherited;
end;

procedure TinfCTCollectionItem.SetinfUnidTransp(
  const Value: TinfUnidTranspCTCollection);
begin
  FinfUnidTransp := Value;
end;

{ TinfNFeCollection }

function TinfNFeCollection.Add: TinfNFeCollectionItem;
begin
  Result := TinfNFeCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfNFeCollection.Create(
  AOwner: TinfMunDescargaCollectionItem);
begin
  inherited Create(TinfNFeCollectionItem);
end;

function TinfNFeCollection.GetItem(Index: Integer): TinfNFeCollectionItem;
begin
  Result := TinfNFeCollectionItem(inherited GetItem(Index));
end;

procedure TinfNFeCollection.SetItem(Index: Integer;
  Value: TinfNFeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfNFeCollectionItem }

constructor TinfNFeCollectionItem.Create;
begin
  FinfUnidTransp := TInfUnidTranspNFeCollection.Create(Self);
  FPeri := TPeriNFeCollection.Create(Self);
end;

destructor TinfNFeCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  FPeri.Free;
  inherited;
end;

procedure TinfNFeCollectionItem.SetinfUnidTransp(
  const Value: TinfUnidTranspNFeCollection);
begin
  FinfUnidTransp := Value;
end;

procedure TinfNFeCollectionItem.SetPeri(const Value: TPeriNFeCollection);
begin
  Fperi := Value;
end;

{ TinfNFCollection }

function TinfNFCollection.Add: TinfNFCollectionItem;
begin
  Result := TinfNFCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfNFCollection.Create(AOwner: TinfMunDescargaCollectionItem);
begin
  inherited Create(TinfNFCollectionItem);
end;

function TinfNFCollection.GetItem(Index: Integer): TinfNFCollectionItem;
begin
  Result := TinfNFCollectionItem(inherited GetItem(Index));
end;

procedure TinfNFCollection.SetItem(Index: Integer;
  Value: TinfNFCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfNFCollectionItem }

constructor TinfNFCollectionItem.Create;
begin
  FinfUnidTransp := TInfUnidTranspNFCollection.Create(Self);
end;

destructor TinfNFCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  inherited;
end;

procedure TinfNFCollectionItem.SetinfUnidTransp(
  const Value: TinfUnidTranspNFCollection);
begin
  FinfUnidTransp := Value;
end;

{ TlacresCollection }

function TlacresCollection.Add: TlacresCollectionItem;
begin
  Result := TlacresCollectionItem(inherited Add);
  Result.create;
end;

constructor TlacresCollection.Create(AOwner: TMDFe);
begin
  inherited Create(TlacresCollectionItem);
end;

function TlacresCollection.GetItem(Index: Integer): TlacresCollectionItem;
begin
  Result := TlacresCollectionItem(inherited GetItem(Index));
end;

procedure TlacresCollection.SetItem(Index: Integer;
  Value: TlacresCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TlacresCollectionItem }

constructor TlacresCollectionItem.Create;
begin

end;

destructor TlacresCollectionItem.Destroy;
begin

  inherited;
end;

{ TinfTermCarregCollection }

function TinfTermCarregCollection.Add: TinfTermCarregCollectionItem;
begin
  Result := TinfTermCarregCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfTermCarregCollection.Create(AOwner: Taquav);
begin
  inherited Create(TinfTermCarregCollectionItem);
end;

function TinfTermCarregCollection.GetItem(
  Index: Integer): TinfTermCarregCollectionItem;
begin
  Result := TinfTermCarregCollectionItem(inherited GetItem(Index));
end;

procedure TinfTermCarregCollection.SetItem(Index: Integer;
  Value: TinfTermCarregCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfTermCarregCollectionItem }

constructor TinfTermCarregCollectionItem.Create;
begin

end;

destructor TinfTermCarregCollectionItem.Destroy;
begin

  inherited;
end;

{ Taquav }

constructor Taquav.Create(AOwner: TMDFe);
begin
  inherited Create;
  FinfTermCarreg     := TinfTermCarregCollection.Create(Self);
  FinfTermDescarreg  := TinfTermDescarregCollection.Create(Self);
  FinfEmbComb        := TinfEmbCombCollection.Create(Self);
  FinfUnidCargaVazia := TinfUnidCargaVaziaCollection.Create(Self);
  FinfUnidTranspVazia := TinfUnidTranspVaziaCollection.Create(Self);
end;

destructor Taquav.Destroy;
begin
  FinfTermCarreg.Free;
  FinfTermDescarreg.Free;
  FinfEmbComb.Free;
  FinfUnidCargaVazia.Free;
  FinfUnidTranspVazia.Free;
  inherited;
end;

procedure Taquav.SetinfEmbComb(const Value: TinfEmbCombCollection);
begin
  FinfEmbComb := Value;
end;

procedure Taquav.SetinfTermCarreg(const Value: TinfTermCarregCollection);
begin
  FinfTermCarreg := Value;
end;

procedure Taquav.SetinfTermDescarreg(const Value: TinfTermDescarregCollection);
begin
  FinfTermDescarreg := Value;
end;

procedure Taquav.SetinfUnidCargaVazia(
  const Value: TinfUnidCargaVaziaCollection);
begin
  FinfUnidCargaVazia := Value;
end;

procedure Taquav.SetinfUnidTranspVazia(
  const Value: TinfUnidTranspVaziaCollection);
begin
  FinfUnidTranspVazia := Value;
end;

{ TinfTermDescarregCollection }

function TinfTermDescarregCollection.Add: TinfTermDescarregCollectionItem;
begin
  Result := TinfTermDescarregCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfTermDescarregCollection.Create(AOwner: Taquav);
begin
  inherited Create(TinfTermDescarregCollectionItem);
end;

function TinfTermDescarregCollection.GetItem(
  Index: Integer): TinfTermDescarregCollectionItem;
begin
  Result := TinfTermDescarregCollectionItem(inherited GetItem(Index));
end;

procedure TinfTermDescarregCollection.SetItem(Index: Integer;
  Value: TinfTermDescarregCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfTermDescarregCollectionItem }

constructor TinfTermDescarregCollectionItem.Create;
begin

end;

destructor TinfTermDescarregCollectionItem.Destroy;
begin

  inherited;
end;

{ TinfEmbCombCollection }

function TinfEmbCombCollection.Add: TinfEmbCombCollectionItem;
begin
  Result := TinfEmbCombCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfEmbCombCollection.Create(AOwner: Taquav);
begin
  inherited Create(TinfEmbCombCollectionItem);
end;

function TinfEmbCombCollection.GetItem(
  Index: Integer): TinfEmbCombCollectionItem;
begin
  Result := TinfEmbCombCollectionItem(inherited GetItem(Index));
end;

procedure TinfEmbCombCollection.SetItem(Index: Integer;
  Value: TinfEmbCombCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfEmbCombCollectionItem }

constructor TinfEmbCombCollectionItem.Create;
begin

end;

destructor TinfEmbCombCollectionItem.Destroy;
begin

  inherited;
end;

{ TinfUnidCargaVaziaCollection }

function TinfUnidCargaVaziaCollection.Add: TinfUnidCargaVaziaCollectionItem;
begin
  Result := TinfUnidCargaVaziaCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfUnidCargaVaziaCollection.Create(AOwner: Taquav);
begin
  inherited Create(TinfUnidCargaVaziaCollectionItem);
end;

function TinfUnidCargaVaziaCollection.GetItem(
  Index: Integer): TinfUnidCargaVaziaCollectionItem;
begin
  Result := TinfUnidCargaVaziaCollectionItem(inherited GetItem(Index));
end;

procedure TinfUnidCargaVaziaCollection.SetItem(Index: Integer;
  Value: TinfUnidCargaVaziaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfUnidCargaVaziaCollectionItem }

constructor TinfUnidCargaVaziaCollectionItem.Create;
begin

end;

destructor TinfUnidCargaVaziaCollectionItem.Destroy;
begin

  inherited;
end;

{ TinfUnidTranspVaziaCollection }

function TinfUnidTranspVaziaCollection.Add: TinfUnidTranspVaziaCollectionItem;
begin
  Result := TinfUnidTranspVaziaCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfUnidTranspVaziaCollection.Create(AOwner: Taquav);
begin
  inherited Create(TinfUnidTranspVaziaCollectionItem);
end;

function TinfUnidTranspVaziaCollection.GetItem(
  Index: Integer): TinfUnidTranspVaziaCollectionItem;
begin
  Result := TinfUnidTranspVaziaCollectionItem(inherited GetItem(Index));
end;

procedure TinfUnidTranspVaziaCollection.SetItem(Index: Integer;
  Value: TinfUnidTranspVaziaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfUnidTranspVaziaCollectionItem }

constructor TinfUnidTranspVaziaCollectionItem.Create;
begin

end;

destructor TinfUnidTranspVaziaCollectionItem.Destroy;
begin

  inherited;
end;

{ TinfUnidTranspCTeCollection }

function TinfUnidTranspCTeCollection.Add: TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfUnidTranspCTeCollection.Create(
  AOwner: TinfCTeCollectionItem);
begin
  inherited Create(TinfUnidTranspCollectionItem);
end;

function TinfUnidTranspCTeCollection.GetItem(
  Index: Integer): TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited GetItem(Index));
end;

procedure TinfUnidTranspCTeCollection.SetItem(Index: Integer;
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

{ TinfUnidTranspCTCollection }

function TinfUnidTranspCTCollection.Add: TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfUnidTranspCTCollection.Create(
  AOwner: TinfCTCollectionItem);
begin
  inherited Create(TinfUnidTranspCollectionItem);
end;

function TinfUnidTranspCTCollection.GetItem(
  Index: Integer): TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited GetItem(Index));
end;

procedure TinfUnidTranspCTCollection.SetItem(Index: Integer;
  Value: TinfUnidTranspCollectionItem);
begin
  inherited Create(TinfUnidTranspCollectionItem);
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

{ TinfMDFeTranspCollection }

function TinfMDFeTranspCollection.Add: TinfMDFeTranspCollectionItem;
begin
  Result := TinfMDFeTranspCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfMDFeTranspCollection.Create(
  AOwner: TinfMunDescargaCollectionItem);
begin
  inherited Create(TinfMDFeTranspCollectionItem);
end;

function TinfMDFeTranspCollection.GetItem(
  Index: Integer): TinfMDFeTranspCollectionItem;
begin
  Result := TinfMDFeTranspCollectionItem(inherited GetItem(Index));
end;

procedure TinfMDFeTranspCollection.SetItem(Index: Integer;
  Value: TinfMDFeTranspCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfMDFeTranspCollectionItem }

constructor TinfMDFeTranspCollectionItem.Create;
begin
  FinfUnidTransp := TInfUnidTranspMDFeCollection.Create(Self);
  FPeri := TPeriMDFeCollection.Create(Self);
end;

destructor TinfMDFeTranspCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  FPeri.Free;
  inherited;
end;

procedure TinfMDFeTranspCollectionItem.SetinfUnidTransp(
  const Value: TinfUnidTranspMDFeCollection);
begin
  FinfUnidTransp := Value;
end;

procedure TinfMDFeTranspCollectionItem.SetPeri(
  const Value: TPeriMDFeCollection);
begin
  Fperi := Value;
end;

{ TinfUnidTranspMDFeCollection }

function TinfUnidTranspMDFeCollection.Add: TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfUnidTranspMDFeCollection.Create(
  AOwner: TinfMDFeTranspCollectionItem);
begin
  inherited Create(TinfUnidTranspCollectionItem);
end;

function TinfUnidTranspMDFeCollection.GetItem(
  Index: Integer): TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited GetItem(Index));
end;

procedure TinfUnidTranspMDFeCollection.SetItem(Index: Integer;
  Value: TinfUnidTranspCollectionItem);
begin
  inherited Create(TinfUnidTranspCollectionItem);
end;

{ TautXMLCollection }

function TautXMLCollection.Add: TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem(inherited Add);
  Result.create;
end;

constructor TautXMLCollection.Create(AOwner: TMDFe);
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

{ TinfMDFe }

function TinfMDFe.GetVersaoStr: String;
begin
  if FVersao <= 0 then
     Result := V1_00
  else
     Result := 'versao="'+FloatToString(FVersao,'.','#0.00')+'"';
end;

{ TPeriCollectionItem }

constructor TPeriCollectionItem.Create;
begin

end;

destructor TPeriCollectionItem.Destroy;
begin

  inherited;
end;

{ TPeriCTeCollection }

function TPeriCTeCollection.Add: TPeriCollectionItem;
begin
  Result := TPeriCollectionItem(inherited Add);
  Result.create;
end;

constructor TPeriCTeCollection.Create(AOwner: TinfCTeCollectionItem);
begin
  inherited Create(TPeriCollectionItem);
end;

function TPeriCTeCollection.GetItem(Index: Integer): TPeriCollectionItem;
begin
  Result := TPeriCollectionItem(inherited GetItem(Index));
end;

procedure TPeriCTeCollection.SetItem(Index: Integer;
  Value: TPeriCollectionItem);
begin
  inherited Create(TPeriCollectionItem);
end;

{ TPeriNFeCollection }

function TPeriNFeCollection.Add: TPeriCollectionItem;
begin
  Result := TPeriCollectionItem(inherited Add);
  Result.create;
end;

constructor TPeriNFeCollection.Create(AOwner: TinfNFeCollectionItem);
begin
  inherited Create(TPeriCollectionItem);
end;

function TPeriNFeCollection.GetItem(Index: Integer): TPeriCollectionItem;
begin
  Result := TPeriCollectionItem(inherited GetItem(Index));
end;

procedure TPeriNFeCollection.SetItem(Index: Integer;
  Value: TPeriCollectionItem);
begin
  inherited Create(TPeriCollectionItem);
end;

{ TPeriMDFeCollection }

function TPeriMDFeCollection.Add: TPeriCollectionItem;
begin
  Result := TPeriCollectionItem(inherited Add);
  Result.create;
end;

constructor TPeriMDFeCollection.Create(AOwner: TinfMDFeTranspCollectionItem);
begin
  inherited Create(TPeriCollectionItem);
end;

function TPeriMDFeCollection.GetItem(Index: Integer): TPeriCollectionItem;
begin
  Result := TPeriCollectionItem(inherited GetItem(Index));
end;

procedure TPeriMDFeCollection.SetItem(Index: Integer;
  Value: TPeriCollectionItem);
begin
  inherited Create(TPeriCollectionItem);
end;

{ TSegCollectionItem }

constructor TSegCollectionItem.Create;
begin
  FAver := TAverCollection.Create(Self);
end;

destructor TSegCollectionItem.Destroy;
begin
  FAver.Free;
  inherited;
end;

{ TSegCollection }

function TSegCollection.Add: TSegCollectionItem;
begin
  Result := TSegCollectionItem(inherited Add);
  Result.create;
end;

constructor TSegCollection.Create(AOwner: TMDFe);
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

{ TinfANTT }

constructor TinfANTT.Create;
begin
  FinfCIOT := TinfCIOTCollection.Create(Self);
  FinfContratante := TinfContratanteCollection.Create(Self);
  FvalePed := TvalePed.Create(Self);
end;

destructor TinfANTT.Destroy;
begin
  FinfCIOT.Free;
  FinfContratante.Free;
  FvalePed.Free;
  inherited;
end;

procedure TinfANTT.SetinfCIOT(const Value: TinfCIOTCollection);
begin
  FinfCIOT := Value;
end;

procedure TinfANTT.SetinfContratante(
  const Value: TinfContratanteCollection);
begin
  FinfContratante := Value;
end;

{ TinfCIOTCollection }

function TinfCIOTCollection.Add: TinfCIOTCollectionItem;
begin
  Result := TinfCIOTCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfCIOTCollection.Create(AOwner: TinfANTT);
begin
  inherited Create(TinfCIOTCollectionItem);
end;

function TinfCIOTCollection.GetItem(
  Index: Integer): TinfCIOTCollectionItem;
begin
  Result := TinfCIOTCollectionItem(inherited GetItem(Index));
end;

procedure TinfCIOTCollection.SetItem(Index: Integer;
  Value: TinfCIOTCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfCIOTCollectionItem }

constructor TinfCIOTCollectionItem.Create;
begin

end;

destructor TinfCIOTCollectionItem.Destroy;
begin

  inherited;
end;

{ TinfContratanteCollection }

function TinfContratanteCollection.Add: TinfContratanteCollectionItem;
begin
  Result := TinfContratanteCollectionItem(inherited Add);
  Result.create;
end;

constructor TinfContratanteCollection.Create(AOwner: TinfANTT);
begin
  inherited Create(TinfContratanteCollectionItem);
end;

function TinfContratanteCollection.GetItem(
  Index: Integer): TinfContratanteCollectionItem;
begin
  Result := TinfContratanteCollectionItem(inherited GetItem(Index));
end;

procedure TinfContratanteCollection.SetItem(Index: Integer;
  Value: TinfContratanteCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfContratanteCollectionItem }

constructor TinfContratanteCollectionItem.Create;
begin

end;

destructor TinfContratanteCollectionItem.Destroy;
begin

  inherited;
end;

{ TlacRodoCollection }

function TlacRodoCollection.Add: TlacresCollectionItem;
begin
  Result := TlacresCollectionItem(inherited Add);
  Result.create;
end;

constructor TlacRodoCollection.Create(AOwner: TRodo);
begin
  inherited Create(TlacresCollectionItem);
end;

function TlacRodoCollection.GetItem(Index: Integer): TlacresCollectionItem;
begin
  Result := TlacresCollectionItem(inherited GetItem(Index));
end;

procedure TlacRodoCollection.SetItem(Index: Integer;
  Value: TlacresCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TAverCollection }

function TAverCollection.Add: TAverCollectionItem;
begin
  Result := TAverCollectionItem(inherited Add);
  Result.create;
end;

constructor TAverCollection.Create(AOwner: TSegCollectionItem);
begin
  inherited Create(TAverCollectionItem);
end;

function TAverCollection.GetItem(Index: Integer): TAverCollectionItem;
begin
  Result := TAverCollectionItem(inherited GetItem(Index));
end;

procedure TAverCollection.SetItem(Index: Integer; Value: TAverCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TAverCollectionItem }

constructor TAverCollectionItem.Create;
begin

end;

destructor TAverCollectionItem.Destroy;
begin

  inherited;
end;

end.

