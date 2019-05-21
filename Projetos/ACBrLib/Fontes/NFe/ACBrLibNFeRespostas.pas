{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibNFeRespostas;

interface

uses
  SysUtils, Classes, contnrs,
  pcnConversao, pcnRetConsReciDFe, pcnRetConsCad, pcnRetConsSitNFe,
  pcnEventoNFe, pcnRetEnvEventoNFe, pcnRetDistDFeInt,
  ACBrLibResposta, ACBrNFe;

type

  { TLibNFeResposta }

  TLibNFeResposta = class(TACBrLibResposta)
  private
    FMsg: string;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property Msg: string read FMsg write FMsg;

  end;

  { TLibNFeServiceResposta }

  TLibNFeServiceResposta = class(TLibNFeResposta)
  private
    Fversao: string;
    FtpAmb: string;
    FverAplic: string;
    FcStat: integer;
    FxMotivo: string;
    FcUF: integer;
    FdhRecbto: TDateTime;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

    procedure ProcessarResposta(const ACBrNFe: TACBrNFe); virtual; abstract;

  published
    property Versao: string read Fversao write Fversao;
    property tpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property CUF: integer read FcUF write FcUF;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
  end;

  { TStatusServicoResposta }

  TStatusServicoResposta = class(TLibNFeServiceResposta)
  private
    FTMed: integer;
    FdhRetorno: TDateTime;
    FxObs: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

    procedure ProcessarResposta(const ACBrNFe: TACBrNFe); override;

  published
    property TMed: integer read FTMed write FTMed;
    property DhRetorno: TDateTime read FdhRetorno write FdhRetorno;
    property XObs: string read FxObs write FxObs;
  end;

  { TInutilizarNFeResposta }

  TInutilizarNFeResposta = class(TLibNFeServiceResposta)
  private
    FNomeArquivo: String;
    FXml: String;
    FNProt: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

    procedure ProcessarResposta(const ACBrNFe: TACBrNFe); override;

  published
    property NomeArquivo: String read FNomeArquivo write FNomeArquivo;
    property Xml: String read FXml write FXml;
    property NProt: String read FNProt write FNProt;
  end;

  { TEnvioResposta }

  TEnvioResposta = class(TLibNFeServiceResposta)
  private
    FtMed: integer;
    FnRec: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

    procedure ProcessarResposta(const ACBrNFe: TACBrNFe); override;

  published
   property TMed: integer read FtMed write FtMed;
   property NRec: string read FnRec write FnRec;
  end;

  { TRetornoItemResposta }

  TRetornoItemResposta = class(TACBrLibResposta)
  private
    FId: String;
    FtpAmb: String;
    FverAplic: String;
    FchNFe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FXML: String;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);

    procedure ProcessarResposta(const Item: TProtDFeCollectionItem);

  published
    property Id: String read FId write FId;
    property tpAmb: String read FtpAmb write FtpAmb;
    property verAplic: String read FverAplic write FverAplic;
    property chNFe: String read FchNFe write FchNFe;
    property dhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property nProt: String read FnProt write FnProt;
    property digVal: String read FdigVal write FdigVal;
    property cStat: Integer read FcStat write FcStat;
    property xMotivo: String read FxMotivo write FxMotivo;
    property XML: String read FXML write FXML;

  end;

  { TRetornoResposta }

  TRetornoResposta = class(TLibNFeServiceResposta)
  private
    FnRec: string;
    FcMsg: integer;
    FxMsg: string;
    FProtocolo: string;
    FChaveNFe: string;
    FItens: TObjectList;

    function GetItem(Index: Integer): TRetornoItemResposta;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;
    destructor Destroy; override;

    procedure ProcessarResposta(const ACBrNFe: TACBrNFe); override;
    function Gerar: String; override;

    property Items[Index: Integer]: TRetornoItemResposta read GetItem; default;

  published
    property nRec: string read FnRec write FnRec;
    property cMsg: integer read FcMsg write FcMsg;
    property xMsg: String read FxMsg write FxMsg;
    property Protocolo: String read FProtocolo write FProtocolo;
    property ChaveNFe: String read FChaveNFe write FChaveNFe;

  end;

  { TReciboResposta }

  TReciboResposta = class(TRetornoResposta)
  public
    procedure ProcessarResposta(const ACBrNFe: TACBrNFe); override;

  end;

  { TCancelamentoResposta }

  TCancelamentoResposta = class(TLibNFeServiceResposta)
  private
    FchNFe: string;
    FnProt: string;
    FtpEvento: string;
    FxEvento: string;
    FnSeqEvento: integer;
    FCNPJDest: string;
    FemailDest: string;
    Fxml: string;
    FArquivo: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

    procedure ProcessarResposta(const ACBrNFe: TACBrNFe); override;

  published
    property chNFe: string read FchNFe write FchNFe;
    property nProt: string read FnProt write FnProt;
    property tpEvento: string read FtpEvento write FtpEvento;
    property xEvento: string read FxEvento write FxEvento;
    property nSeqEvento: integer read FnSeqEvento write FnSeqEvento;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property emailDest: string read FemailDest write FemailDest;
    property XML: string read Fxml write Fxml;
    property Arquivo: string read FArquivo write FArquivo;
  end;

  { TConsultaCadastroItemResposta }

  TConsultaCadastroItemResposta = class(TACBrLibResposta)
  private
    Farquivo: string;
    FCEP: Integer;
    FcMun: Integer;
    FCNAE: Integer;
    FcSit: Integer;
    FdBaixa: TDateTime;
    FdIniAtiv: TDateTime;
    FdUltSit: TDateTime;
    FIE: string;
    FCNPJ: string;
    FCPF: string;
    FIEAtual: String;
    FIEUnica: String;
    Fnro: String;
    FUF: String;
    FxBairro: String;
    FxCpl: String;
    FxFant: String;
    FxLgr: String;
    FxMun: String;
    FxNome: String;
    FxRegApur: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);

    procedure ProcessarResposta(const Item: TInfCadCollectionItem);

  published
    property arquivo: String read Farquivo write Farquivo;
    property IE: string read FIE write FIE;
    property CNPJ: String read FCNPJ write FCNPJ;
    property CPF: String read FCPF write FCPF;
    property UF: String read FUF write FUF;
    property cSit: Integer read FcSit write FcSit;
    property xNome: String read FxNome write FxNome;
    property xFant: String read FxFant write FxFant;
    property xRegApur: String read FxRegApur write FxRegApur;
    property CNAE: Integer read FCNAE write FCNAE;
    property dIniAtiv: TDateTime read FdIniAtiv write FdIniAtiv;
    property dUltSit: TDateTime read FdUltSit write FdUltSit;
    property dBaixa: TDateTime read FdBaixa write FdBaixa;
    property IEUnica: String read FIEUnica write FIEUnica;
    property IEAtual: String read FIEAtual write FIEAtual;
    property xLgr: String read FxLgr write FxLgr;
    property nro: String read Fnro write Fnro;
    property xCpl: String read FxCpl write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: String read FxMun write FxMun;
    property CEP: Integer read FCEP write FCEP;
  end;

  { TConsultaCadastroResposta }

  TConsultaCadastroResposta = class(TLibNFeServiceResposta)
  private
    FIE: string;
    FCNPJ: string;
    FCPF: string;
    FUF: string;
    FdhCons: TDateTime;
    FItens: TObjectList;

    function GetItem(Index: Integer): TConsultaCadastroItemResposta;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;
    destructor Destroy; override;

    procedure ProcessarResposta(const ACBrNFe: TACBrNFe); override;
    function Gerar: String; override;

    property Items[Index: Integer]: TConsultaCadastroItemResposta read GetItem; default;

  published
    property IE: string read FIE write FIE;
    property CNPJ: string read FCNPJ write FCNPJ;
    property CPF: string read FCPF write FCPF;
    property UF: string read FUF write FUF;
    property dhCons: TDateTime read FdhCons write FdhCons;

  end;

  { TEventoItemResposta }

  TEventoItemResposta = class(TACBrLibResposta)
  private
    FtpAmb: String;
    FverAplic: String;
    FcStat: integer;
    FxMotivo: String;
    Farquivo: String;
    FchNFe: string;
    FCNPJDest: string;
    FcOrgao: integer;
    FdhRegEvento: TDateTime;
    FdigVal: String;
    FemailDest: string;
    FId: string;
    FnProt: String;
    FnSeqEvento: Integer;
    FtpEvento: string;
    FxEvento: string;
    FXML: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

    procedure ProcessarResposta(const AInfEvento: TRetInfEvento);

  published
    property tpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property chNFe: string read FchNFe write FchNFe;
    property nProt: String read FnProt write FnProt;
    property arquivo: String read Farquivo write Farquivo;
    property digVal: String read FdigVal write FdigVal;
    property Id: string read FId write FId;
    property cOrgao: integer read FcOrgao write FcOrgao;
    property dhRegEvento: TDateTime read FdhRegEvento write FdhRegEvento;
    property tpEvento: string read FtpEvento write FtpEvento;
    property xEvento: string read FxEvento write FxEvento;
    property nSeqEvento: Integer read FnSeqEvento write FnSeqEvento;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property emailDest: string read FemailDest write FemailDest;
    property XML: string read FXML write FXML;
  end;

  { TEventoResposta }

  TEventoResposta = class(TLibNFeServiceResposta)
  private
    FidLote: Integer;
    FcOrgao: Integer;
    FItems: TObjectList;

    function GetItem(Index: Integer): TEventoItemResposta;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;
    destructor Destroy; override;

    procedure ProcessarResposta(const ACBrNFe: TACBrNFe); override;
    function Gerar: String; override;

    property Items[Index: Integer]: TEventoItemResposta read GetItem;

  published
    property idLote: Integer read FidLote write FidLote;
    property cOrgao: Integer read FcOrgao write FcOrgao;
  end;

  { TDistribuicaoDFeItemResposta }

  TDistribuicaoDFeItemResposta = class(TACBrLibResposta)
  private
    Fversao: String;
    FtpAmb: String;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    Farquivo: String;
    FCNPJ: string;
    FCNPJDest: string;
    FcOrgao: integer;
    FcOrgaoAutor: integer;
    FcSitNFe: String;
    FcteChvCte: String;
    FcteDhemi: TDateTime;
    FcteDhRebcto: TDateTime;
    FcteModal: string;
    FcteNProt: string;
    FdescEvento: string;
    FdhEmi: TDateTime;
    FdhEvento: TDateTime;
    FdhRegEvento: TDateTime;
    FdigVal: String;
    FdhRecbto: TDateTime;
    FemailDest: string;
    FEmiCNPJ: string;
    FEmiIE: string;
    FEmixNome: string;
    FId: string;
    FIE: String;
    FnProt: String;
    FnSeqEvento: Integer;
    FNSU: string;
    FchNFe: string;
    FCNPJCPF: string;
    Fschema: String;
    FtpEvento: string;
    FtpNF: String;
    FverEvento: string;
    FvNF: Currency;
    FxEvento: string;
    FxJust: string;
    FXML: string;
    FxNome: string;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

    procedure ProcessarResposta(const AresDFe: TresDFe; const ANSU, AArquivo, AXml: String; const  ASchema: TSchemaDFe); overload;
    procedure ProcessarResposta(const AresEvento: TresEvento; const ANSU, AArquivo, AXml: String; const  ASchema: TSchemaDFe); overload;
    procedure ProcessarResposta(const AprocEvento: TprocEvento; const ANSU, AArquivo, AXml: String; const  ASchema: TSchemaDFe); overload;
    procedure ProcessarResposta(const ARetInfEvento: TprocEvento_RetInfEvento; const ANSU, AArquivo, AXml: String; const  ASchema: TSchemaDFe); overload;

  published
    property Versao: string read Fversao write Fversao;
    property tpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property NSU: string read FNSU write FNSU;
    property chNFe: string read FchNFe write FchNFe;
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property xNome: string read FxNome write FxNome;
    property IE: String read FIE write FIE;
    property dhEmi: TDateTime read FdhEmi write FdhEmi;
    property tpNF: String read FtpNF write FtpNF;
    property vNF: Currency read FvNF write FvNF;
    property digVal: String read FdigVal write FdigVal;
    property dhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property cSitNFe: String read FcSitNFe write FcSitNFe;
    property nProt: String read FnProt write FnProt;
    property XML: string read FXML write FXML;
    property arquivo: String read Farquivo write Farquivo;
    property schema: String read Fschema write Fschema;
    property dhEvento: TDateTime read FdhEvento write FdhEvento;
    property tpEvento: string read FtpEvento write FtpEvento;
    property xEvento: string read FxEvento write FxEvento;
    property nSeqEvento: Integer read FnSeqEvento write FnSeqEvento;
    property cOrgao: integer read FcOrgao write FcOrgao;
    property CNPJ: string read FCNPJ write FCNPJ;
    property Id: string read FId write FId;
    property verEvento: string read FverEvento write FverEvento;
    property descEvento: string read FdescEvento write FdescEvento;
    property xJust: string read FxJust write FxJust;
    property xMotivo: string read FxMotivo write FxMotivo;
    property EmiCNPJ: string read FEmiCNPJ write FEmiCNPJ;
    property EmiIE: string read FEmiIE write FEmiIE;
    property EmixNome: string read FEmixNome write FEmixNome;
    property cteNProt: string read FcteNProt write FcteNProt;
    property cteChvCte: String read FcteChvCte write FcteChvCte;
    property cteDhemi: TDateTime read FcteDhemi write FcteDhemi;
    property cteDhRebcto: TDateTime read FcteDhRebcto write FcteDhRebcto;
    property cteModal: string read FcteModal write FcteModal;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property cOrgaoAutor: integer read FcOrgaoAutor write FcOrgaoAutor;
    property dhRegEvento: TDateTime read FdhRegEvento write FdhRegEvento;
    property emailDest: string read FemailDest write FemailDest;

    end;

  { TDistribuicaoDFeResposta }

  TDistribuicaoDFeResposta = class(TLibNFeServiceResposta)
  private
    Farquivo: string;
    FdhResp: TDateTime;
    FindCont: string;
    FmaxNSU: string;
    FultNSU: string;
    FItems: TObjectList;

    function GetItem(Index: Integer): TDistribuicaoDFeItemResposta;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;
    destructor Destroy; override;

    procedure ProcessarResposta(const ACBrNFe: TACBrNFe); override;
    function Gerar: String; override;

    property Items[Index: Integer]: TDistribuicaoDFeItemResposta read GetItem;

  published
    property dhResp: TDateTime read FdhResp write FdhResp;
    property ultNSU: string read FultNSU write FultNSU;
    property maxNSU: string read FmaxNSU write FmaxNSU;
    property arquivo: string read Farquivo write Farquivo;
    property indCont: string read FindCont write FindCont;
  end;

  { TConsultaNFeInfCanResposta }
  TConsultaNFeInfCanResposta  = class(TLibNFeServiceResposta)
  private
    FChNFe: String;
    FNProt: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

    procedure ProcessarResposta(const ARetCancNFe: TRetCancNFe); reintroduce;

  published
    property ChNFe: String read FChNFe write FChNFe;
    property NProt: String read FNProt write FNProt;
  end;

  { TConsultaNFeItemPedidoResposta }
  TConsultaNFeItemPedidoResposta  = class(TACBrLibResposta)
  private
    FnumItem: Integer;
    FqtdeItem: Double;
  public
    constructor Create(const AId, AIndex: Integer; const ATipo: TACBrLibRespostaTipo); reintroduce;

    procedure ProcessarResposta(const AItem: TitemPedidoCollectionItem);

  published
    property numItem: Integer         read FnumItem        write FnumItem;
    property qtdeItem: Double         read FqtdeItem       write FqtdeItem;

  end;

  { TConsultaNFeChNFePendResposta }
  TConsultaNFeChNFePendResposta  = class(TACBrLibResposta)
  private
    FchNFePend : String;
  public
    constructor Create(const AId, AIndex, ASIndex: Integer; const ATipo: TACBrLibRespostaTipo); reintroduce;

    procedure ProcessarResposta(const ARetchNFePend: TRetchNFePendCollectionItem);

  published
    property chNFePend : String       read FchNFePend      write FchNFePend;

  end;

  { TConsultaNFeRetEventoResposta }
   TConsultaNFeRetEventoResposta  = class(TACBrLibResposta)
   private
     FAId: Integer;
     FAIndex: Integer;
     FId: String;
     Fversao: String;
     FtpAmb: String;
     FverAplic: String;
     FcStat: Integer;
     FxMotivo: String;
     FNomeArquivo: String;
     FcOrgao: String;
     FchNFe: String;
     FtpEvento: String;
     FxEvento: String;
     FnSeqEvento: Integer;
     FCNPJDest: String;
     FemailDest: String;
     FcOrgaoAutor: String;
     FdhRegEvento: TDateTime;
     FnProt: String;
     FXML: String;
     FItems: TObjectList;

     function GetItem(Index: Integer): TConsultaNFeChNFePendResposta;

   public
     constructor Create(const AId, AIndex: Integer; const ATipo: TACBrLibRespostaTipo); reintroduce;
     destructor Destroy; override;

     procedure ProcessarResposta(const ARetInfEvento: TRetInfEvento);
     function Gerar: String; override;

     property Items[Index: Integer]: TConsultaNFeChNFePendResposta read GetItem;

   published
     property Id: String read FId;
     property Versao: string read Fversao write Fversao;
     property tpAmb: string read FtpAmb write FtpAmb;
     property VerAplic: string read FverAplic write FverAplic;
     property CStat: integer read FcStat write FcStat;
     property XMotivo: string read FxMotivo write FxMotivo;
     property NomeArquivo: String read FNomeArquivo write FNomeArquivo;
     property cOrgao: String read FcOrgao write FcOrgao;
     property chNFe: String read FchNFe write FchNFe;
     property tpEvento: String read FtpEvento write FtpEvento;
     property xEvento: String read FxEvento write FxEvento;
     property nSeqEvento: Integer read FnSeqEvento write FnSeqEvento;
     property CNPJDest: String read FCNPJDest write FCNPJDest;
     property emailDest: String read FemailDest write FemailDest;
     property cOrgaoAutor: String read FcOrgaoAutor write FcOrgaoAutor;
     property dhRegEvento: TDateTime read FdhRegEvento write FdhRegEvento;
     property nProt: String read FnProt write FnProt;
     property XML: String read FXML write FXML;

   end;

  { TConsultaNFeDetEventoResposta }
  TConsultaNFeDetEventoResposta  = class(TACBrLibResposta)
  private
    FId: Integer;
    Fversao : String;
    FverAplic : String;
    FdescEvento : String;
    FxCorrecao : String;
    FxCondUso : String;
    FnProt : String;
    FxJust : String;
    FcOrgaoAutor : String;
    FtpAutor : String;
    FdhEmi : TDateTime;
    FtpNF : String;
    FIE : String;
    FDESTCNPJCPF : String;
    FDESTidEstrangeiro : String;
    FDESTIE : String;
    FDESTUF : String;
    FvNF : Double;
    FvICMS : Double;
    FvST : Double;
    FidPedidoCancelado : String;
    FItems: TObjectList;

    function GetItem(Index: Integer): TConsultaNFeItemPedidoResposta;

  public
    constructor Create(const AId: Integer; const ATipo: TACBrLibRespostaTipo); reintroduce;
    destructor Destroy; override;

    procedure ProcessarResposta(const AEvento: TDetEvento);
    function Gerar: String; override;

    property Items[Index: Integer]: TConsultaNFeItemPedidoResposta read GetItem;

  published
    property Versao: string read Fversao write Fversao;
    property VerAplic: string read FverAplic write FverAplic;
    property descEvento: String read FdescEvento write FdescEvento;
    property xCorrecao: String read FxCorrecao write FxCorrecao;
    property xCondUso: String read FxCondUso write FxCondUso;
    property nProt: String read FnProt write FnProt;
    property xJust: String read FxJust write FxJust;
    property cOrgaoAutor: String read FcOrgaoAutor write FcOrgaoAutor;
    property tpAutor: String read FtpAutor write FtpAutor;
    property dhEmi: TDateTime read FdhEmi write FdhEmi;
    property tpNF: String read FtpNF write FtpNF;
    property IE: String read FIE write FIE;
    property DESTCNPJCPF: String read FDESTCNPJCPF write FDESTCNPJCPF;
    property DESTidEstrangeiro: String read FDESTidEstrangeiro write FDESTidEstrangeiro;
    property DESTIE: String read FDESTIE write FDESTIE;
    property DESTUF: String read FDESTUF write FDESTUF;
    property vNF: Double read FvNF write FvNF;
    property vICMS: Double read FvICMS write FvICMS;
    property vST: Double read FvST write FvST;
    property idPedidoCancelado: String read FidPedidoCancelado write FidPedidoCancelado;

  end;

  { TConsultaNFeProcEventoResposta }
  TConsultaNFeProcEventoResposta  = class(TACBrLibResposta)
  private
    FID: Integer;
    FtpAmb: String;
    FcOrgao: String;
    FCNPJ: String;
    FchNFe: String;
    FdhEvento: TDateTime;
    FtpEvento: String;
    FnSeqEvento: Integer;
    FverEvento: String;
    FDetEvento: TConsultaNFeDetEventoResposta;
    FRetEventos: TObjectList;

    function GetEventos(Index: Integer): TConsultaNFeRetEventoResposta;

  public
    constructor Create(const AId: Integer; const ATipo: TACBrLibRespostaTipo); reintroduce;
    destructor Destroy; override;

    procedure ProcessarResposta(const AEvento: TRetEventoNFeCollectionItem); reintroduce;
    function Gerar: String; override;

    property detEvento: TConsultaNFeDetEventoResposta read FDetEvento write FDetEvento;
    property RetEventos[Index: Integer]: TConsultaNFeRetEventoResposta read GetEventos;

  published
    property ID: Integer read FID;
    property tpAmb: string read FtpAmb write FtpAmb;
    property cOrgao: String read FcOrgao write FcOrgao;
    property CNPJ: String read FCNPJ write FCNPJ;
    property chNFe: String read FchNFe write FchNFe;
    property dhEvento: TDateTime read FdhEvento write FdhEvento;
    property tpEvento: String read FtpEvento write FtpEvento;
    property nSeqEvento: Integer read FnSeqEvento write FnSeqEvento;
    property verEvento: String read FverEvento write FverEvento;

  end;

  { TConsultaNFeResposta }
  TConsultaNFeResposta = class(TLibNFeServiceResposta)
  private
    FChNFe: String;
    FNProt: String;
    FDigVal: String;
    FcMsg: Integer;
    FxMsg: String;
    FInfCan: TConsultaNFeInfCanResposta;
    FEventos: TObjectList;

    function GetEvento(Index: Integer): TConsultaNFeProcEventoResposta;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;
    destructor Destroy; override;

    procedure ProcessarResposta(const ACBrNFe: TACBrNFe); override;
    function Gerar: String; override;

    property InfCan: TConsultaNFeInfCanResposta read FInfCan write FInfCan;
    property Eventos[Index: Integer]: TConsultaNFeProcEventoResposta read GetEvento;

  published
    property ChNFe: String read FChNFe write FChNFe;
    property NProt: String read FNProt write FNProt;
    property DigVal: String read FDigVal write FDigVal;
    property cMsg: Integer read FcMsg write FcMsg;
    property xMsg: String read FxMsg write FxMsg;

  end;

  { TLibImpressaoResposta }
  TLibImpressaoResposta = class(TLibNFeResposta)
  public
    constructor Create(const QtdImpresso: Integer; const ATipo: TACBrLibRespostaTipo); reintroduce;

  end;


implementation

uses
  pcnAuxiliar,
  ACBrUtil, ACBrLibNFeConsts;

{ TLibNFeResposta }

constructor TLibNFeResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

{ TLibNFeServiceResposta }

constructor TLibNFeServiceResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

{ TLibImpressaoResposta }
constructor TLibImpressaoResposta.Create(const QtdImpresso: Integer; const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create('Impressao', ATipo);
  Msg := Format('%d NFe(s) impressa(s) com sucesso', [QtdImpresso]);
end;

{ TConsultaNFeChNFePendResposta }

constructor TConsultaNFeChNFePendResposta.Create(const AId, AIndex, ASIndex: Integer;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create('ChNFePend' + Trim(IntToStrZero(AId +1, 3)) + Trim(IntToStrZero(AIndex +1, 3))
                + Trim(IntToStrZero(ASIndex +1, 3)), ATipo);
end;

procedure TConsultaNFeChNFePendResposta.ProcessarResposta(const ARetchNFePend: TRetchNFePendCollectionItem);
begin
  chNFePend:= ARetchNFePend.ChavePend;
end;

{ TConsultaNFeRetEventoResposta }

constructor TConsultaNFeRetEventoResposta.Create(const AId, AIndex: Integer;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create('RetEvento' + Trim(IntToStrZero(AId, 3)) + Trim(IntToStrZero(AIndex, 3)), ATipo);
  FItems := TObjectList.Create;

  FAId := AId;
  FAIndex := AIndex;

  FId := IntToStr(AIndex);
end;

destructor TConsultaNFeRetEventoResposta.Destroy;
begin
  FItems.Clear;
  FItems.Free;

  inherited Destroy;
end;

function TConsultaNFeRetEventoResposta.Gerar: String;
Var
  i: Integer;
begin
  Result := Inherited Gerar;

  for i := 0 to FItems.Count - 1  do
  begin
    Result := Result + sLineBreak + TConsultaNFeChNFePendResposta(FItems.Items[i]).Gerar;
  end;
end;

function TConsultaNFeRetEventoResposta.GetItem(Index: Integer): TConsultaNFeChNFePendResposta;
begin
  Result := TConsultaNFeChNFePendResposta(FItems.Items[0]);
end;

procedure TConsultaNFeRetEventoResposta.ProcessarResposta(const ARetInfEvento: TRetInfEvento);
Var
  i: Integer;
  Item: TConsultaNFeChNFePendResposta;
begin
  NomeArquivo := ARetInfEvento.NomeArquivo;
  tpAmb := TpAmbToStr(ARetInfEvento.tpAmb);
  verAplic := ARetInfEvento.verAplic;
  cOrgao := IntToStr(ARetInfEvento.cOrgao);
  cStat := ARetInfEvento.cStat;
  xMotivo:= ARetInfEvento.xMotivo;
  chNFe := ARetInfEvento.chNFe;
  tpEvento := TpEventoToStr(ARetInfEvento.tpEvento);
  xEvento := ARetInfEvento.xEvento;
  nSeqEvento := ARetInfEvento.nSeqEvento;
  CNPJDest := ARetInfEvento.CNPJDest;
  emailDest := ARetInfEvento.emailDest;
  cOrgaoAutor := IntToStr(ARetInfEvento.cOrgaoAutor);
  dhRegEvento := ARetInfEvento.dhRegEvento;
  nProt := ARetInfEvento.nProt;
  XML := ARetInfEvento.XML;

  for i := 0 to ARetInfEvento.chNFePend.Count - 1  do
   begin
     Item := TConsultaNFeChNFePendResposta.Create(FAId, FAIndex, i+1, FTipo);
     Item.ProcessarResposta(ARetInfEvento.chNFePend.Items[i]);
     FItems.Add(Item);
   end;
end;

{ TConsultaNFeItemPedidoResposta }

constructor TConsultaNFeItemPedidoResposta.Create(const AId, AIndex: Integer;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create('ItemPedido' + Trim(IntToStrZero(AId, 3)) + Trim(IntToStrZero(AIndex, 3)), ATipo);
end;

procedure TConsultaNFeItemPedidoResposta.ProcessarResposta(const AItem: TitemPedidoCollectionItem);
begin
  numItem := AItem.numItem;
  qtdeItem := AItem.qtdeItem;
end;

{ TConsultaNFeDetEventoResposta }

constructor TConsultaNFeDetEventoResposta.Create(const AId: Integer;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create('DetEvento' + Trim(IntToStrZero(AId, 3)), ATipo);
  FItems := TObjectList.Create;
  FId := AID;
end;

destructor TConsultaNFeDetEventoResposta.Destroy;
begin
  FItems.Clear;
  FItems.Free;

  inherited Destroy;
end;

function TConsultaNFeDetEventoResposta.Gerar: String;
Var
  i: Integer;
begin
  Result := Inherited Gerar;

  for i := 0 to FItems.Count - 1  do
  begin
    Result := Result + sLineBreak + TConsultaNFeItemPedidoResposta(FItems.Items[i]).Gerar;
  end;
end;

function TConsultaNFeDetEventoResposta.GetItem(Index: Integer): TConsultaNFeItemPedidoResposta;
begin
  Result := TConsultaNFeItemPedidoResposta(FItems.Items[0]);
end;

procedure TConsultaNFeDetEventoResposta.ProcessarResposta(const AEvento: TDetEvento);
Var
  i: Integer;
  Item: TConsultaNFeItemPedidoResposta;
begin
   versao := AEvento.versao;
   descEvento:= AEvento.descEvento;
   xCorrecao := AEvento.xCorrecao;
   xCondUso := AEvento.xCondUso;
   nProt := AEvento.nProt;
   xJust := AEvento.xJust;
   cOrgaoAutor:= IntToStr(AEvento.cOrgaoAutor);
   tpAutor := TipoAutorToStr(AEvento.tpAutor);
   verAplic := AEvento.verAplic;
   dhEmi := AEvento.dhEmi;
   tpNF := tpNFToStr(AEvento.tpNF);
   IE := AEvento.IE;
   DESTCNPJCPF := AEvento.dest.CNPJCPF;
   DESTidEstrangeiro := AEvento.dest.idEstrangeiro;
   DESTIE := AEvento.dest.IE;
   DESTUF := AEvento.dest.UF;
   vNF := AEvento.vNF;
   vICMS := AEvento.vICMS;
   vST := AEvento.vST;
   idPedidoCancelado :=  AEvento.idPedidoCancelado;

   for i := 0 to AEvento.itemPedido.Count - 1  do
   begin
     Item := TConsultaNFeItemPedidoResposta.Create(FId, i+1, FTipo);
     Item.ProcessarResposta(AEvento.itemPedido.Items[i]);
     FItems.Add(Item);
   end;

end;

{ TConsultaNFeProcEventoResposta }

constructor TConsultaNFeProcEventoResposta.Create(const AId: Integer;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create('ProcEventoNFe' + Trim(IntToStrZero(AId, 3)), ATipo);
  FDetEvento := TConsultaNFeDetEventoResposta.Create(AId, ATipo);
  FRetEventos := TObjectList.Create;
  FID := AId;
end;

destructor TConsultaNFeProcEventoResposta.Destroy;
begin
  FDetEvento.Free;

  FRetEventos.Clear;
  FRetEventos.Free;

  inherited Destroy;
end;

function TConsultaNFeProcEventoResposta.GetEventos(Index: Integer): TConsultaNFeRetEventoResposta;
begin
  Result := TConsultaNFeRetEventoResposta(FRetEventos.Items[0]);
end;

function TConsultaNFeProcEventoResposta.Gerar: String;
Var
  i: Integer;
begin
  Result := Inherited Gerar;
  Result := Result + sLineBreak + FDetEvento.Gerar;

  for i := 0 to FRetEventos.Count - 1  do
  begin
    Result := Result + sLineBreak + TConsultaNFeRetEventoResposta(FRetEventos.Items[i]).Gerar;
  end;
end;

procedure TConsultaNFeProcEventoResposta.ProcessarResposta(const AEvento: TRetEventoNFeCollectionItem);
Var
  i: Integer;
  RetEvento: TConsultaNFeRetEventoResposta;
begin
   cOrgao := IntToStr(AEvento.RetEventoNFe.InfEvento.cOrgao);
   tpAmb := TpAmbToStr(AEvento.RetEventoNFe.InfEvento.tpAmb);
   CNPJ := AEvento.RetEventoNFe.InfEvento.CNPJ;
   chNFe := AEvento.RetEventoNFe.InfEvento.chNFe;
   dhEvento := AEvento.RetEventoNFe.InfEvento.dhEvento;
   tpEvento := TpEventoToStr(AEvento.RetEventoNFe.InfEvento.tpEvento);
   nSeqEvento := AEvento.RetEventoNFe.InfEvento.nSeqEvento;
   verEvento := AEvento.RetEventoNFe.InfEvento.versaoEvento;

   FDetEvento.ProcessarResposta(AEvento.RetEventoNFe.InfEvento.detEvento);

   for i := 0 to AEvento.RetEventoNFe.retEvento.Count - 1  do
   begin
     RetEvento := TConsultaNFeRetEventoResposta.Create(FId, i+1, FTipo);
     RetEvento.ProcessarResposta(AEvento.RetEventoNFe.retEvento.Items[i].RetInfEvento);
     FRetEventos.Add(RetEvento);
   end;
end;

{ TConsultaNFeInfCanResposta }

constructor TConsultaNFeInfCanResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespConsultaInfCan, ATipo);
end;

procedure TConsultaNFeInfCanResposta.ProcessarResposta(const ARetCancNFe: TRetCancNFe);
begin
  tpAmb := TpAmbToStr(ARetCancNFe.tpAmb);
  VerAplic := ARetCancNFe.verAplic;
  CStat := ARetCancNFe.cStat;
  XMotivo := ARetCancNFe.xMotivo;
  CUF := ARetCancNFe.cUF;
  ChNFe := ARetCancNFe.chNFE;
  DhRecbto := ARetCancNFe.dhRecbto;
  NProt := ARetCancNFe.nProt;
end;

{ TDistribuicaoDFeItemResposta }

constructor TDistribuicaoDFeItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

procedure TDistribuicaoDFeItemResposta.ProcessarResposta(const AresDFe: TresDFe; const ANSU, AArquivo, AXml: String;
  const  ASchema: TSchemaDFe);
begin
  NSU := ANSU;
  XML := AXml;
  Arquivo := AArquivo;
  schema := SchemaDFeToStr(ASchema);
  chNFe := AresDFe.chDFe;
  CNPJCPF := AresDFe.CNPJCPF;
  xNome := AresDFe.xNome;
  IE := AresDFe.IE;
  dhEmi := AresDFe.dhEmi;
  tpNF := tpNFToStr(AresDFe.tpNF);
  vNF := AresDFe.vNF;
  digVal := AresDFe.digVal;
  dhRecbto := AresDFe.dhRecbto;
  cSitNFe := SituacaoDFeToStr(AresDFe.cSitDFe);
  nProt := AresDFe.nProt;
end;

procedure TDistribuicaoDFeItemResposta.ProcessarResposta(const AresEvento: TresEvento; const ANSU, AArquivo, AXml: String;
  const  ASchema: TSchemaDFe);
begin
  NSU := ANSU;
  XML := AXml;
  Arquivo := AArquivo;
  schema := SchemaDFeToStr(ASchema);
  chNFe := AresEvento.chDFe;
  CNPJCPF := AresEvento.CNPJCPF;
  dhEvento := AresEvento.dhEvento;
  tpEvento := TpEventoToStr(AresEvento.tpEvento);
  xEvento := AresEvento.xEvento;
  nSeqEvento := AresEvento.nSeqEvento;
  cOrgao := AresEvento.cOrgao;
  dhRecbto := AresEvento.dhRecbto;
  nProt := AresEvento.nProt;
end;

procedure TDistribuicaoDFeItemResposta.ProcessarResposta(const AprocEvento: TprocEvento; const ANSU, AArquivo, AXml: String;
  const  ASchema: TSchemaDFe);
begin
  NSU := ANSU;
  XML := AXml;
  Arquivo := AArquivo;
  schema := SchemaDFeToStr(ASchema);
  chNFe := AprocEvento.chDFe;
  cOrgao := AprocEvento.cOrgao;
  CNPJ := AprocEvento.CNPJ;
  Id := AprocEvento.Id;
  dhEvento := AprocEvento.dhEvento;
  nSeqEvento := AprocEvento.nSeqEvento;
  tpAmb := TpAmbToStr(AprocEvento.tpAmb);
  tpEvento := TpEventoToStr(AprocEvento.tpEvento);
  verEvento := AprocEvento.verEvento;

  with AprocEvento.detEvento do
  begin
    descEvento := descEvento;
    xJust := xJust;
    xMotivo := xCorrecao;
    EmiCnpj := emit.CNPJ;
    EmiIE := emit.IE;
    EmixNome := emit.xNome;
    cteNProt := CTe.nProt;
    cteChvCte := CTe.chCTe;
    cteDhemi := CTe.dhEmi;
    cteModal := TpModalToStr(CTe.modal);
    cteDhRebcto := CTe.dhRecbto;
  end;
end;

procedure TDistribuicaoDFeItemResposta.ProcessarResposta(const ARetInfEvento: TprocEvento_RetInfEvento; const ANSU, AArquivo,
  AXml: String; const  ASchema: TSchemaDFe);
begin
  NSU := ANSU;
  XML := AXml;
  Arquivo := AArquivo;
  schema := SchemaDFeToStr(ASchema);

  Id := ARetInfEvento.Id;
  VerAplic := ARetInfEvento.VerAplic;
  tpAmb := TpAmbToStr(ARetInfEvento.tpAmb);
  cOrgao := ARetInfEvento.cOrgao;
  chNFe := ARetInfEvento.chDFe;
  CStat := ARetInfEvento.cStat;
  CNPJDest := ARetInfEvento.CNPJDest;
  cOrgaoAutor := ARetInfEvento.cOrgaoAutor;
  tpEvento := TpEventoToStr(ARetInfEvento.tpEvento);
  nSeqEvento := ARetInfEvento.nSeqEvento;
  xEvento := ARetInfEvento.xEvento;
  XMotivo := ARetInfEvento.XMotivo;
  dhRegEvento := ARetInfEvento.dhRegEvento;
  emailDest :=ARetInfEvento. emailDest;
  nProt := ARetInfEvento.nProt;
end;

{ TDistribuicaoDFeResposta }

constructor TDistribuicaoDFeResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespDistribuicaoDFe, ATipo);

  FItems := TObjectList.Create;
end;

destructor TDistribuicaoDFeResposta.Destroy;
begin
  FItems.Clear;
  FItems.Free;

  inherited Destroy;
end;

function TDistribuicaoDFeResposta.Gerar: String;
Var
  i: Integer;
begin
  Result := Inherited Gerar;

  for i := 0 to FItems.Count - 1  do
  begin
    Result := Result + sLineBreak + TDistribuicaoDFeItemResposta(FItems.Items[i]).Gerar;
  end;
end;

function TDistribuicaoDFeResposta.GetItem(Index: Integer): TDistribuicaoDFeItemResposta;
begin
  Result := TDistribuicaoDFeItemResposta(FItems.Items[0]);
end;

procedure TDistribuicaoDFeResposta.ProcessarResposta(const ACBrNFe: TACBrNFe);
var
  I, J: Integer;
  Item: TDistribuicaoDFeItemResposta;
begin
   with ACBrNFe.WebServices.DistribuicaoDFe do
   begin
     self.Versao := retDistDFeInt.versao;
     self.Msg := ACBrNFe.WebServices.DistribuicaoDFe.Msg;
     self.VerAplic := retDistDFeInt.VerAplic;
     self.tpAmb := TpAmbToStr(retDistDFeInt.tpAmb);
     self.CStat := retDistDFeInt.cStat;
     self.XMotivo := retDistDFeInt.XMotivo;
     self.dhResp := retDistDFeInt.dhResp;
     self.ultNSU := retDistDFeInt.ultNSU;
     self.maxNSU := retDistDFeInt.maxNSU;
     self.arquivo := ACBrNFe.WebServices.DistribuicaoDFe.NomeArq;

     if cStat = 137 then
       indCont := '1'  // Sim
     else
       indCont := '0'; // Não

     J := 1;
     for I := 0 to retDistDFeInt.docZip.Count - 1 do
     begin
       if (Trim(retDistDFeInt.docZip[I].resDFe.chDFe) <> '') then
       begin
         Item := TDistribuicaoDFeItemResposta.Create('ResNFe' + Trim(IntToStrZero(J, 3)), FTipo);
         Item.ProcessarResposta(retDistDFeInt.docZip.Items[I].resDFe,
                                retDistDFeInt.docZip.Items[I].NSU, listaArqs[I],
                                retDistDFeInt.docZip.Items[I].XML, retDistDFeInt.docZip.Items[I].schema);
         FItems.Add(Item);
         inc(J);
       end;
     end;

     J := 1;
     for I := 0 to retDistDFeInt.docZip.Count - 1 do
     begin
       if (Trim(retDistDFeInt.docZip[I].resEvento.chDFe) <> '') then
       begin
         Item := TDistribuicaoDFeItemResposta.Create('ResEve' + Trim(IntToStrZero(J, 3)), FTipo);
         Item.ProcessarResposta(retDistDFeInt.docZip.Items[I].resEvento,
                                retDistDFeInt.docZip.Items[I].NSU, listaArqs[I],
                                retDistDFeInt.docZip.Items[I].XML, retDistDFeInt.docZip.Items[I].schema);
         FItems.Add(Item);
         inc(J);
       end;
     end;

     J := 1;
     for I := 0 to retDistDFeInt.docZip.Count - 1 do
     begin
       if (Trim(retDistDFeInt.docZip[I].procEvento.detEvento.versao) <> '' ) then
       begin
         Item := TDistribuicaoDFeItemResposta.Create('ProEve' + Trim(IntToStrZero(J, 3)), FTipo);
         Item.ProcessarResposta(retDistDFeInt.docZip.Items[I].procEvento,
                                retDistDFeInt.docZip.Items[I].NSU, listaArqs[I],
                                retDistDFeInt.docZip.Items[I].XML, retDistDFeInt.docZip.Items[I].schema);
         FItems.Add(Item);
         inc(J);
       end;
     end;

     J := 1;
     for I := 0 to retDistDFeInt.docZip.Count - 1 do
     begin
       if (Trim(retDistDFeInt.docZip[I].procEvento.RetinfEvento.Id) <> '' ) then
       begin
         Item := TDistribuicaoDFeItemResposta.Create('InfEve' + Trim(IntToStrZero(J, 3)), FTipo);
         Item.ProcessarResposta(retDistDFeInt.docZip.Items[I].procEvento.RetinfEvento,
                                retDistDFeInt.docZip.Items[I].NSU, listaArqs[I],
                                retDistDFeInt.docZip.Items[I].XML, retDistDFeInt.docZip.Items[I].schema);
         FItems.Add(Item);
         inc(J);
       end;
     end;
   end;
end;

{ TEventoItemResposta }

constructor TEventoItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

procedure TEventoItemResposta.ProcessarResposta(const AInfEvento: TRetInfEvento);
begin
  Id := AInfEvento.Id;
  tpAmb := TpAmbToStr(AInfEvento.tpAmb);
  verAplic := AInfEvento.verAplic;
  cOrgao := AInfEvento.cOrgao;
  cStat := AInfEvento.cStat;
  xMotivo := AInfEvento.xMotivo;
  chNFe := AInfEvento.chNFe;
  tpEvento := TpEventoToStr(AInfEvento.tpEvento);
  xEvento := AInfEvento.xEvento;
  nSeqEvento := AInfEvento.nSeqEvento;
  CNPJDest := AInfEvento.CNPJDest;
  emailDest := AInfEvento.emailDest;
  dhRegEvento := AInfEvento.dhRegEvento;
  nProt := AInfEvento.nProt;
  Arquivo := AInfEvento.NomeArquivo;
  XML := AInfEvento.XML;
end;

{ TEventoResposta }

constructor TEventoResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespEvento, ATipo);

  FItems := TObjectList.Create;
end;

destructor TEventoResposta.Destroy;
begin
  FItems.Clear;
  FItems.Free;

  inherited Destroy;
end;

function TEventoResposta.GetItem(Index: Integer): TEventoItemResposta;
begin
  Result := TEventoItemResposta(FItems.Items[0]);
end;

function TEventoResposta.Gerar: String;
Var
  i: Integer;
begin
  Result := Inherited Gerar;

  for i := 0 to FItems.Count - 1  do
  begin
    Result := Result + sLineBreak + TEventoItemResposta(FItems.Items[i]).Gerar;
  end;
end;

procedure TEventoResposta.ProcessarResposta(const ACBrNFe: TACBrNFe);
Var
  I: Integer;
  Item: TEventoItemResposta;
begin
  with ACBrNFe.WebServices.EnvEvento do
  begin
    Self.VerAplic := EventoRetorno.VerAplic;
    Self.tpAmb := TpAmbToStr(EventoRetorno.tpAmb);
    Self.CStat := EventoRetorno.cStat;
    Self.XMotivo := EventoRetorno.XMotivo;
    Self.idLote := EventoRetorno.IdLote;
    Self.cOrgao := EventoRetorno.cOrgao;

    for I := 0 to EventoRetorno.retEvento.Count - 1 do
    begin
      Item := TEventoItemResposta.Create('Evento' + Trim(IntToStrZero(I +1, 3)), FTipo);
      Item.ProcessarResposta(EventoRetorno.retEvento.Items[i].RetInfEvento);
      FItems.Add(Item);
    end;
  end;
end;

{ TConsultaCadastroItemResposta }

constructor TConsultaCadastroItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

procedure TConsultaCadastroItemResposta.ProcessarResposta(const Item: TInfCadCollectionItem);
begin
  IE := Item.IE;
  CNPJ := Item.CNPJ;
  CPF := Item.CPF;
  UF := Item.UF;
  cSit := Item.cSit;
  xNome := Item.xNome;
  xFant := Item.xFant;
  xRegApur := Item.xRegApur;
  CNAE := Item.CNAE;
  dIniAtiv := Item.dIniAtiv;
  dUltSit := Item.dUltSit;
  dBaixa := Item.dBaixa;
  IEUnica := Item.IEUnica;
  IEAtual := Item.IEAtual;
  xLgr := Item.xLgr;
  nro := Item.nro;
  xCpl := Item.xCpl;
  xBairro := Item.xBairro;
  cMun := Item.cMun;
  xMun := Item.xMun;
  CEP := Item.CEP;
end;

{ TConsultaCadastroResposta }

constructor TConsultaCadastroResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespConsultaCadastro, ATipo);
  FItens := TObjectList.Create(True);
end;

destructor TConsultaCadastroResposta.Destroy;
begin
  FItens.Free;

  Inherited Destroy;
end;

function TConsultaCadastroResposta.GetItem(Index: Integer): TConsultaCadastroItemResposta;
begin
  Result := TConsultaCadastroItemResposta(FItens[Index]);
end;

function TConsultaCadastroResposta.Gerar: String;
Var
  i: Integer;
begin
  Result := Inherited Gerar;
  for i := 0 to FItens.Count - 1 do
  begin
    Result := Result + sLineBreak + TConsultaCadastroItemResposta(FItens[i]).Gerar;
  end;
end;

procedure TConsultaCadastroResposta.ProcessarResposta(const ACBrNFe: TACBrNFe);
var
  i: Integer;
  Item: TConsultaCadastroItemResposta;
begin
  with ACBrNFe.WebServices do
  begin
    Versao := ConsultaCadastro.versao;
    VerAplic := ConsultaCadastro.VerAplic;
    CStat := ConsultaCadastro.cStat;
    XMotivo := ConsultaCadastro.XMotivo;
    CUF := ConsultaCadastro.cUF;
    dhCons := ConsultaCadastro.dhCons;
    IE  := ConsultaCadastro.RetConsCad.IE;
    CNPJ := ConsultaCadastro.RetConsCad.CNPJ;
    CPF := ConsultaCadastro.RetConsCad.CPF;
    UF := ConsultaCadastro.REtConsCad.UF;

    for i := 0 to ConsultaCadastro.RetConsCad.InfCad.Count - 1 do
    begin
      Item := TConsultaCadastroItemResposta.Create('INFCAD' + Trim(IntToStrZero(i + 1, 3)), FTipo);
      Item.ProcessarResposta(ConsultaCadastro.RetConsCad.InfCad.Items[i]);
      FItens.Add(Item);
    end;
  end;
end;

{ TCancelamentoResposta }

constructor TCancelamentoResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespCancelamento, ATipo);
end;

procedure TCancelamentoResposta.ProcessarResposta(const ACBrNFe: TACBrNFe);
begin
  with ACBrNFe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0] do
  begin
    Arquivo:= RetInfEvento.NomeArquivo;
    Versao := RetInfevento.verAplic;
    TpAmb := TpAmbToStr(RetInfevento.TpAmb);
    VerAplic := RetInfevento.VerAplic;
    CStat := RetInfevento.cStat;
    XMotivo := RetInfevento.XMotivo;
    CUF := RetInfevento.cOrgao;
    ChNFe := RetInfevento.chNFe;
    DhRecbto := RetInfevento.dhRegEvento;
    NProt := RetInfevento.nProt;
    TpEvento := TpEventoToStr(RetInfevento.tpEvento);
    xEvento := RetInfevento.xEvento;
    nSeqEvento := RetInfevento.nSeqEvento;
    CNPJDest := RetInfevento.CNPJDest;
    emailDest := RetInfevento.emailDest;
    XML := RetInfevento.XML;
    XMotivo := RetInfevento.XMotivo;
  end;
end;

{ TRetornoItemResposta }

constructor TRetornoItemResposta.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

procedure TRetornoItemResposta.ProcessarResposta(const Item: TProtDFeCollectionItem);
begin
  FId := Item.Id;
  FtpAmb := TpAmbToStr(Item.tpAmb);
  FverAplic := Item.verAplic;
  FchNFe := Item.chDFe;
  FdhRecbto := Item.dhRecbto;
  FnProt := Item.nProt;
  FdigVal := Item.digVal;
  FcStat := Item.cStat;
  FxMotivo := Item.xMotivo;
  FXML := Item.XMLprotDFe;
end;

{ TRetornoResposta }

constructor TRetornoResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespRetorno, ATipo);

  FItens := TObjectList.Create(True);
end;

destructor TRetornoResposta.Destroy;
begin
  FItens.Free;

  Inherited Destroy;
end;

function TRetornoResposta.GetItem(Index: Integer): TRetornoItemResposta;
begin
  Result := TRetornoItemResposta(FItens[Index]);
end;

function TRetornoResposta.Gerar: String;
Var
  i: Integer;
begin
  Result := Inherited Gerar;
  for i := 0 to FItens.Count - 1 do
  begin
    Result := Result + sLineBreak + TRetornoItemResposta(FItens.Items[i]).Gerar;
  end;
end;

procedure TRetornoResposta.ProcessarResposta(const ACBrNFe: TACBrNFe);
Var
  i: Integer;
  Item: TRetornoItemResposta;
begin
  with ACBrNFe.WebServices do
  begin
    Versao := Retorno.versao;
    TpAmb := TpAmbToStr(Retorno.TpAmb);
    verAplic := Retorno.verAplic;
    CStat := Retorno.cStat;
    XMotivo := Retorno.xMotivo;
    CUF := Retorno.cUF;
    nRec := Retorno.Recibo;
    cMsg := Retorno.cMsg;
    xMsg := Retorno.xMsg;
    Msg := Retorno.Msg;
    Protocolo := Retorno.Protocolo;
    ChaveNFe := Retorno.ChaveNFe;

    with Retorno.NFeRetorno do
    begin
      for i := 0 to ProtDFe.Count - 1 do
      begin
        Item := TRetornoItemResposta.Create('NFe' + Trim(IntToStr(StrToInt(copy(ProtDFe.Items[i].chDFe, 26, 9)))), FTipo);
        Item.ProcessarResposta(ProtDFe.Items[i]);
        FItens.Add(Item);
      end;
    end;
  end;
end;

{ TReciboResposta }

procedure TReciboResposta.ProcessarResposta(const ACBrNFe: TACBrNFe);
Var
  i: Integer;
  Item: TRetornoItemResposta;
begin
  with ACBrNFe.WebServices do
  begin
    Versao := Recibo.Versao;
    TpAmb := TpAmbToStr(Recibo.TpAmb);
    VerAplic := Recibo.VerAplic;
    nRec := Recibo.Recibo;
    CStat := Recibo.cStat;
    XMotivo := Recibo.XMotivo;
    CUF := Recibo.cUF;

    with Recibo.NFeRetorno do
    begin
      for i := 0 to ProtDFe.Count - 1 do
      begin
        Item := TRetornoItemResposta.Create('NFe' + Trim(IntToStr(StrToInt(copy(ProtDFe.Items[i].chDFe, 26, 9)))), FTipo);
        Item.ProcessarResposta(ProtDFe.Items[i]);
        FItens.Add(Item);
      end;
    end;
  end;
end;

{ TEnvioResposta }

constructor TEnvioResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespEnvio, ATipo);
end;

procedure TEnvioResposta.ProcessarResposta(const ACBrNFe: TACBrNFe);
begin
  with ACBrNFe.WebServices do
  begin
    Versao := Enviar.versao;
    TpAmb := TpAmbToStr(Enviar.TpAmb);
    verAplic := Enviar.verAplic;
    CStat := Enviar.cStat;
    XMotivo := Enviar.xMotivo;
    CUF := Enviar.cUF;
    nRec := Enviar.Recibo;
    DhRecbto := Enviar.dhRecbto;
    Tmed := Enviar.TMed;
    Msg := Enviar.Msg;
  end;
end;

{ TStatusServicoResposta }

constructor TStatusServicoResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespStatus, ATipo);
end;

procedure TStatusServicoResposta.ProcessarResposta(const ACBrNFe: TACBrNFe);
begin
  with ACBrNFe.WebServices do
  begin
    Msg := StatusServico.Msg;
    Versao := StatusServico.versao;
    TpAmb := TpAmbToStr(StatusServico.TpAmb);
    VerAplic := StatusServico.VerAplic;
    CStat := StatusServico.CStat;
    XMotivo := StatusServico.XMotivo;
    CUF := StatusServico.CUF;
    DhRecbto := StatusServico.DhRecbto;
    TMed := StatusServico.TMed;
    DhRetorno := StatusServico.DhRetorno;
    XObs := StatusServico.XObs;
  end;
end;

{ TInutilizarNFeResposta }

constructor TInutilizarNFeResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespInutilizacao, ATipo);
end;

procedure TInutilizarNFeResposta.ProcessarResposta(const ACBrNFe: TACBrNFe);
begin
  with ACBrNFe.WebServices do
  begin
    Msg := Inutilizacao.Msg;
    Versao := Inutilizacao.versao;
    TpAmb := TpAmbToStr(Inutilizacao.TpAmb);
    VerAplic := Inutilizacao.VerAplic;
    CStat := Inutilizacao.CStat;
    XMotivo := Inutilizacao.XMotivo;
    CUF := Inutilizacao.cUF;
    DhRecbto := Inutilizacao.DhRecbto;
    NomeArquivo := Inutilizacao.NomeArquivo;
    NProt := Inutilizacao.Protocolo;
    Xml := Inutilizacao.XML_ProcInutNFe;

  end;
end;

{ TConsultaNFeResposta }

constructor TConsultaNFeResposta.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoRespConsulta, ATipo);

  FInfCan := TConsultaNFeInfCanResposta.Create(ATipo);
  FEventos := TObjectList.Create;
end;

destructor TConsultaNFeResposta.Destroy;
begin
  FInfCan.Free;

  FEventos.Clear;
  FEventos.Free;

  inherited Destroy;
end;

function TConsultaNFeResposta.GetEvento(Index: Integer): TConsultaNFeProcEventoResposta;
begin
  Result := TConsultaNFeProcEventoResposta(FEventos.Items[Index]);
end;

function TConsultaNFeResposta.Gerar: String;
Var
  i: Integer;
begin
  Result := Inherited Gerar;

  if NaoEstaVazio(Trim(InfCan.nProt)) then
  begin
    Result := Result + sLineBreak + InfCan.Gerar;
  end;

  for i := 0 to FEventos.Count - 1 do
  begin
    Result := Result + sLineBreak + TConsultaNFeProcEventoResposta(FEventos.Items[i]).Gerar;
  end;
end;

procedure TConsultaNFeResposta.ProcessarResposta(const ACBrNFe: TACBrNFe);
Var
  I: Integer;
  ProcEvento: TConsultaNFeProcEventoResposta;
begin
  with ACBrNFe.WebServices do
  begin
    Msg := Consulta.Msg;
    Versao := Consulta.versao;
    TpAmb := TpAmbToStr(Consulta.TpAmb);
    VerAplic := Consulta.VerAplic;
    CStat := Consulta.CStat;
    XMotivo := Consulta.XMotivo;
    CUF := Consulta.CUF;
    DhRecbto := Consulta.DhRecbto;
    ChNFe := Consulta.NFeChave;
    NProt := Consulta.Protocolo;
    DigVal := Consulta.protNFe.digVal;
    cMsg := Consulta.protNFe.cMsg;
    xMsg := Consulta.ProtNFe.xMsg;

    if NaoEstaVazio(Trim(Consulta.retCancNFe.nProt)) then
    begin
      InfCan.ProcessarResposta(Consulta.retCancNFe);
    end;

    for I:= 0 to Consulta.procEventoNFe.Count-1 do
    begin
      ProcEvento := TConsultaNFeProcEventoResposta.Create(I + 1, FTipo);
      ProcEvento.ProcessarResposta(Consulta.procEventoNFe.Items[I]);
      FEventos.Add(ProcEvento);
    end;
  end;
end;

end.

