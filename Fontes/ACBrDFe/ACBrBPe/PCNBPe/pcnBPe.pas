////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar BPe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml do BPe          //
//                                                                            //
//        site: www.projetocooperar.org                                       //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
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

{*******************************************************************************
|* Historico
|*
|* 20/06/2017: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pcnBPe;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnConversaoBPe, pcnSignature, pcnProcBPe, pcnGerador;

type

  TBPe = class;
  TInfBPe = class;
  TIde = class;

  TEmit = class;
  TenderEmit = class;

  TComp = class;
  TenderComp = class;

  TAgencia = class;

  TInfBPeSub = class;
  TInfPassagem = class;
  TInfPassageiro = class;

  TInfViagemCollection = class;
  TInfViagemCollectionItem = class;
  TinfTravessia = class;

  TinfValorBPe = class;
  TCompCollection = class;
  TCompCollectionItem = class;

  TImp = class;
  TICMS = class;
  TICMSUFFim = class;

  TpagCollection = class;
  TpagCollectionItem = class;

  TautXMLCollection     = class;
  TautXMLCollectionItem = class;

  TInfAdic = class;
  TinfBPeSupl = class;

  { TinfRespTec }

  TinfRespTec = class(TObject)
  private
    FCNPJ: String;
    FxContato: String;
    Femail: String;
    Ffone: String;
    FidCSRT: Integer;
    FhashCSRT: String;
  public
    procedure Assign(Source: TinfRespTec);

    property CNPJ: String     read FCNPJ     write FCNPJ;
    property xContato: String read FxContato write FxContato;
    property email: String    read Femail    write Femail;
    property fone: String     read Ffone     write Ffone;
    property idCSRT: Integer  read FidCSRT   write FidCSRT;
    property hashCSRT: String read FhashCSRT write FhashCSRT;
  end;

  { TBPe }

  TBPe = class(TObject)
  private
    FinfBPe: TinfBPe;
    FIde: TIde;
    FEmit: TEmit;
    FComp: TComp;
    FAgencia: TAgencia;
    FinfBPeSub: TinfBPeSub;
    FinfPassagem: TinfPassagem;
    FinfViagem: TInfViagemCollection;
    FinfValorBPe: TinfValorBPe;
    FImp: TImp;
    FPag: TpagCollection;
    FautXML: TautXMLCollection;
    FInfAdic: TInfAdic;
    FinfBPeSupl: TinfBPeSupl;
    FSignature: TSignature;
    FProcBPe: TProcBPe;
    FinfRespTec: TinfRespTec;

    procedure SetInfViagem(const Value: TInfViagemCollection);
    procedure SetPag(Value: TpagCollection);
    procedure SetautXML(const Value: TautXMLCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TBPe);
    procedure SetXMLString(const AValue : AnsiString) ;
    property infBPe: TinfBPe read FinfBPe write FinfBPe;
    property Ide: TIde read FIde write FIde;
    property Emit: TEmit read FEmit write FEmit;
    property Comp: TComp read FComp write FComp;
    property Agencia: TAgencia read FAgencia write FAgencia;
    property infBPeSub: TinfBPeSub read FinfBPeSub write FinfBPeSub;
    property infPassagem: TinfPassagem read FinfPassagem write FinfPassagem;
    property infViagem: TInfViagemCollection read FinfViagem write SetInfViagem;
    property infValorBPe: TinfValorBPe read FinfValorBPe write FinfValorBPe;
    property Imp: TImp read FImp write FImp;
    property Pag: TpagCollection read Fpag write SetPag;
    property autXML: TautXMLCollection read FautXML write SetautXML;
    property InfAdic: TInfAdic read FInfAdic write FInfAdic;
    property infBPeSupl: TinfBPeSupl read FinfBPeSupl write FinfBPeSupl;
    property Signature: TSignature read FSignature write FSignature;
    property procBPe: TProcBPe read FProcBPe write FProcBPe;
    property infRespTec: TinfRespTec read FinfRespTec write FinfRespTec;
  end;

  TinfBPe = class(TPersistent)
  private
    FVersao: Real;
    FID: String;

    function GetVersaoStr: String;
    function GetVersao: Real;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Versao: Real read GetVersao write FVersao;
    property VersaoStr: String read GetVersaoStr;
    property ID: String read FID write FID;
  end;

  TIde = class(TPersistent)
  private
    FcUF: Integer;
    FtpAmb: TpcnTipoAmbiente;
    Fmodelo: Integer;
    Fserie: Integer;
    FnBP: Integer;
    FcBP: Integer;
    FcDV: Integer;
    Fmodal: TModalBPe;
    FdhEmi: TDateTime;
    FtpEmis: TpcnTipoEmissao;
    FverProc: String;
    FtpBPe: TTipoBPe;
    FindPres: TpcnPresencaComprador;
    FUFIni: String;
    FcMunIni: Integer;
    FUFFim: String;
    FcMunFim: Integer;
    FdhCont : TDateTime;
    FxJust  : String;

  public
    constructor Create(AOwner: TBPe);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property cUF: Integer read FcUF write FcUF;
    property tpAmb: TpcnTipoAmbiente read FtpAmb write FtpAmb default taHomologacao;
    property modelo: Integer read Fmodelo write Fmodelo;
    property serie: Integer read Fserie write Fserie;
    property nBP: Integer read FnBP write FnBP;
    property cBP: Integer read FcBP write FcBP;
    property cDV: Integer read FcDV write FcDV;
    property modal: TModalBPe read Fmodal write Fmodal;
    property dhEmi: TDateTime read FdhEmi write FdhEmi;
    property tpEmis: TpcnTipoEmissao read FtpEmis write FtpEmis default teNormal;
    property verProc: String read FverProc write FverProc;
    property tpBPe: TTipoBPe read FtpBPe write FtpBPe default tbNormal;
    property indPres: TpcnPresencaComprador read FindPres write FindPres;
    property UFIni: String read FUFIni write FUFIni;
    property cMunIni: Integer read FcMunIni write FcMunIni;
    property UFFim: String read FUFFim write FUFFim;
    property cMunFim: Integer read FcMunFim write FcMunFim;
    property dhCont: TDateTime read FdhCont write FdhCont;
    property xJust: String read FxJust write FxJust;
  end;

  TEmit = class(TPersistent)
  private
    FCNPJ: String;
    FIE: String;
    FIEST: String;
    FxNome: String;
    FxFant: String;
    FIM: String;
    FCNAE: String;
    FCRT: TpcnCRT;
    FenderEmit: TenderEmit;
    FTAR: String;
  public
    constructor Create(AOwner: TBPe);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property CNPJ: String read FCNPJ write FCNPJ;
    property IE: String read FIE write FIE;
    property IEST: String read FIEST write FIEST;
    property xNome: String read FxNome write FxNome;
    property xFant: String read FxFant write FxFant;
    property IM: String read FIM write FIM;
    property CNAE: String read FCNAE write FCNAE;
    property CRT: TpcnCRT read FCRT write FCRT;
    property EnderEmit: TEnderEmit read FEnderEmit write FEnderEmit;
    property TAR: String read FTAR write FTAR;
  end;

  TEnderEmit = class(TPersistent)
  private
    FxLgr: String;
    Fnro: String;
    fxCpl: String;
    FxBairro: String;
    FcMun: Integer;
    FxMun: String;
    FCEP: Integer;
    FUF: String;
    Ffone: String;
    FEmail: String;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property xLgr: String read FxLgr write FxLgr;
    property nro: String read Fnro write Fnro;
    property xCpl: String read FxCpl write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: String read FxMun write FxMun;
    property CEP: Integer read FCEP write FCEP;
    property UF: String read FUF write FUF;
    property Fone: String read Ffone write Ffone;
    property Email: String read FEmail write FEmail;
  end;

  TComp = class(TPersistent)
  private
    FxNome: String;
    FCNPJCPF: String;
    FidEstrangeiro: String;
    FIE: String;
    FenderComp: TenderComp;
  public
    constructor Create(AOwner: TBPe);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property xNome: String read FxNome write FxNome;
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property idEstrangeiro: String read FidEstrangeiro write FidEstrangeiro;
    property IE: String read FIE write FIE;
    property EnderComp: TEnderComp read FEnderComp write FEnderComp;
  end;

  TEnderComp = class(TPersistent)
  private
    FxLgr: String;
    Fnro: String;
    fxCpl: String;
    FxBairro: String;
    FcMun: Integer;
    FxMun: String;
    FCEP: Integer;
    FUF: String;
    FcPais: Integer;
    FxPais: String;
    Ffone: String;
    FEmail: String;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property xLgr: String read FxLgr write FxLgr;
    property nro: String read Fnro write Fnro;
    property xCpl: String read FxCpl write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: String read FxMun write FxMun;
    property CEP: Integer read FCEP write FCEP;
    property UF: String read FUF write FUF;
    property cPais: Integer read FcPais write FcPais;
    property xPais: String read FxPais write FxPais;
    property Fone: String read Ffone write Ffone;
    property Email: String read FEmail write FEmail;
  end;

  TAgencia = class(TPersistent)
  private
    FxNome: String;
    FCNPJ: String;
    FenderAgencia: TenderComp;
  public
    constructor Create(AOwner: TBPe);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property xNome: String read FxNome write FxNome;
    property CNPJ: String read FCNPJ write FCNPJ;
    property EnderAgencia: TEnderComp read FEnderAgencia write FEnderAgencia;
  end;

  TinfBPeSub = class(TPersistent)
  private
    FchBPe: String;
    FtpSub: TTipoSubstituicao;
  public
    constructor Create(AOwner: TBPe);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property chBPe: String read FchBPe write FchBPe;
    property tpSub: TTipoSubstituicao read FtpSub write FtpSub;
  end;

  TinfPassagem = class(TPersistent)
  private
    FcLocOrig: String;
    FxLocOrig: String;
    FcLocDest: String;
    FxLocDest: String;
    FdhEmb: TDateTime;
    FdhValidade: TDateTime;
    FinfPassageiro: TinfPassageiro;
  public
    constructor Create(AOwner: TBPe);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property cLocOrig: String read FcLocOrig write FcLocOrig;
    property xLocOrig: String read FxLocOrig write FxLocOrig;
    property cLocDest: String read FcLocDest write FcLocDest;
    property xLocDest: String read FxLocDest write FxLocDest;
    property dhEmb: TDateTime read FdhEmb write FdhEmb;
    property dhValidade: TDateTime read FdhValidade write FdhValidade;
    property infPassageiro: TinfPassageiro read FinfPassageiro write FinfPassageiro;
  end;

  TinfPassageiro = class(TPersistent)
  private
    FxNome: String;
    FCPF: String;
    FtpDoc: TTipoDocumento;
    FnDoc: String;
    FxDoc: String;
    FdNasc: TDateTime;
    FFone: String;
    FEmail: String;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property xNome: String read FxNome write FxNome;
    property CPF: String read FCPF write FCPF;
    property tpDoc: TTipoDocumento read FtpDoc write FtpDoc;
    property nDoc: String read FnDoc write FnDoc;
    property xDoc: String read FxDoc write FxDoc;
    property dNasc: TDateTime read FdNasc write FdNasc;
    property Fone: String read FFone write FFone;
    property Email: String read FEmail write FEmail;
  end;

  TInfViagemCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfViagemCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfViagemCollectionItem);
  public
    constructor Create(AOwner: TBPe);
    destructor Destroy; override;
    function Add: TInfViagemCollectionItem;
    property Items[Index: Integer]: TInfViagemCollectionItem read GetItem write SetItem; default;
  end;

  TInfViagemCollectionItem = class(TCollectionItem)
  private
    FcPercurso: String;
    FxPercurso: String;
    FtpViagem: TTipoViagem;
    FtpServ: TTipoServico;
    FtpAcomodacao: TTipoAcomodacao;
    FtpTrecho: TTipoTrecho;
    FdhViagem: TDateTime;
    FdhConexao: TDateTime;
    FPrefixo: String;
    FPoltrona: Integer;
    FPlataforma: String;
    FinfTravessia: TinfTravessia;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property cPercurso: String read FcPercurso write FcPercurso;
    property xPercurso: String read FxPercurso write FxPercurso;
    property tpViagem: TTipoViagem read FtpViagem write FtpViagem;
    property tpServ: TTipoServico read FtpServ write FtpServ;
    property tpAcomodacao: TTipoAcomodacao read FtpAcomodacao write FtpAcomodacao;
    property tpTrecho: TTipoTrecho read FtpTrecho write FtpTrecho;
    property dhViagem: TDateTime read FdhViagem write FdhViagem;
    property dhConexao: TDateTime read FdhConexao write FdhConexao;
    property Prefixo: String read FPrefixo write FPrefixo;
    property Poltrona: Integer read FPoltrona write FPoltrona;
    property Plataforma: String read FPlataforma write FPlataforma;
    property infTravessia: TinfTravessia read FinfTravessia write FinfTravessia;
  end;

  TinfTravessia = class(TPersistent)
  private
    FtpVeiculo: TTipoVeiculo;
    FsitVeiculo: TSitVeiculo;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property tpVeiculo: TTipoVeiculo read FtpVeiculo write FtpVeiculo;
    property sitVeiculo: TSitVeiculo read FsitVeiculo write FsitVeiculo;
  end;

  TinfValorBPe = class(TObject)
  private
    FvBP: Currency;
    FvDesconto: Currency;
    FvPgto: Currency;
    FvTroco: Currency;
    FtpDesconto: TTipoDesconto;
    FxDesconto: String;
    FcDesconto: String;
    FComp: TCompCollection;

    procedure SetComp(const Value: TCompCollection);
  public
    constructor Create(AOwner: TBPe);
    destructor Destroy; override;

    procedure Assign(Source: TinfValorBPe);
    property vBP: Currency read FvBP write FvBP;
    property vDesconto: Currency read FvDesconto write FvDesconto;
    property vPgto: Currency read FvPgto write FvPgto;
    property vTroco: Currency read FvTroco write FvTroco;
    property tpDesconto: TTipoDesconto read FtpDesconto write FtpDesconto;
    property xDesconto: String read FxDesconto write FxDesconto;
    property cDesconto: String read FcDesconto write FcDesconto;
    property Comp: TCompCollection read FComp write SetComp;
  end;

  TCompCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TCompCollectionItem;
    procedure SetItem(Index: Integer; Value: TCompCollectionItem);
  public
    constructor Create(AOwner: TinfValorBPe);
    function Add: TCompCollectionItem;
    property Items[Index: Integer]: TCompCollectionItem read GetItem write SetItem; default;
  end;

  TCompCollectionItem = class(TCollectionItem)
  private
    FtpComp: TTipoComponente;
    FvComp: Currency;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property tpComp: TTipoComponente read FtpComp write FtpComp;
    property vComp: Currency read FvComp write FvComp;
  end;

  TImp = class(TPersistent)
  private
    FICMS: TICMS;
    FvTotTrib: Currency;
    FinfAdFisco: String;
    FICMSUFFim: TICMSUFFim;
  public
    constructor Create(AOwner: TBPe);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property ICMS: TICMS read FICMS write FICMS;
    property vTotTrib: Currency read FvTotTrib write FvTotTrib;
    property infAdFisco: String read FinfAdFisco write FinfAdFisco;
    property ICMSUFFim: TICMSUFFim read FICMSUFFim write FICMSUFFim;
  end;

  TICMS = class(TPersistent)
  private
    FCST: TpcnCSTIcms;
    FvBC: Currency;
    FpICMS: Currency;
    FvICMS: Currency;
    FpRedBC: Currency;
    FvCred: Currency;
    FpRedBCOutraUF: Currency;
    FvBCOutraUF: Currency;
    FpICMSOutraUF: Currency;
    FvICMSOutraUF: Currency;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property CST: TpcnCSTIcms read FCST write FCST default cst00;
    property vBC: Currency read FvBC write FvBC;
    property pICMS: Currency read FpICMS write FpICMS;
    property vICMS: Currency read FvICMS write FvICMS;
    property pRedBC: Currency read FpRedBC write FpRedBC;
    property vCred: Currency read FvCred write FvCred;
    property pRedBCOutraUF: Currency read FpRedBCOutraUF write FpRedBCOutraUF;
    property vBCOutraUF: Currency read FvBCOutraUF write FvBCOutraUF;
    property pICMSOutraUF: Currency read FpICMSOutraUF write FpICMSOutraUF;
    property vICMSOutraUF: Currency read FvICMSOutraUF write FvICMSOutraUF;
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
  public
    procedure Assign(Source: TPersistent); override;
  published
    property vBCUFFim: Currency read FvBCUFFim write FvBCUFFim;
    property pFCPUFFim: Currency read FpFCPUFFim write FpFCPUFFim;
    property pICMSUFFim: Currency read FpICMSUFFim write FpICMSUFFim;
    property pICMSInter: Currency read FpICMSInter write FpICMSInter;
    property pICMSInterPart: Currency read FpICMSInterPart write FpICMSInterPart;
    property vFCPUFFim: Currency read FvFCPUFFim write FvFCPUFFim;
    property vICMSUFFim: Currency read FvICMSUFFim write FvICMSUFFim;
    property vICMSUFIni: Currency read FvICMSUFIni write FvICMSUFIni;
  end;

  { TpagCollection}

  TpagCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TpagCollectionItem;
    procedure SetItem(Index: Integer; Value: TpagCollectionItem);
  public
    constructor Create(AOwner: TBPe);
    function Add: TpagCollectionItem;
    property Items[Index: Integer]: TpagCollectionItem read GetItem write SetItem; default;
  end;

  TpagCollectionItem = class(TCollectionItem)
  private
    FtPag: TpcnFormaPagamento;
    FxPag: String;
    FnDocPag: String;
    FvPag: Currency;
    FtpIntegra: TtpIntegra;
    FCNPJ: String;
    FtBand: TBandeiraCard;
    FxBand: String;
    FcAut: String;
    FnsuTrans: String;
    FnsuHost: String;
    FnParcelas: Integer;
    FinfAdCard: String;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property tPag: TpcnFormaPagamento read FtPag write FtPag;
    property xPag: String read FxPag write FxPag;
    property nDocPag: String read FnDocPag write FnDocPag;
    property vPag: Currency read FvPag write FvPag;
    property tpIntegra: TtpIntegra read FtpIntegra write FtpIntegra;
    property CNPJ: String read FCNPJ write FCNPJ;
    property tBand: TBandeiraCard read FtBand write FtBand;
    property xBand: String read FxBand write FxBand;
    property cAut: String read FcAut write FcAut;
    property nsuTrans: String read FnsuTrans write FnsuTrans;
    property nsuHost: String read FnsuHost write FnsuHost;
    property nParcelas: Integer read FnParcelas write FnParcelas;
    property infAdCard: String read FinfAdCard write FinfAdCard;
  end;

  TautXMLCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TautXMLCollectionItem;
    procedure SetItem(Index: Integer; Value: TautXMLCollectionItem);
  public
    constructor Create(AOwner: TBPe);
    function Add: TautXMLCollectionItem;
    property Items[Index: Integer]: TautXMLCollectionItem read GetItem write SetItem; default;
  end;

  TautXMLCollectionItem = class(TCollectionItem)
  private
    FCNPJCPF: String;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
  end;

  TInfAdic = class(TPersistent)
  private
    FinfAdFisco: String;
    FinfCpl: String;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property infAdFisco: String read FinfAdFisco write FinfAdFisco;
    property infCpl: String read FinfCpl write FinfCpl;
  end;

  TinfBPeSupl = class(TPersistent)
  private
    FqrCodBPe: String;
    FboardPassBPe: String;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property qrCodBPe: String read FqrCodBPe write FqrCodBPe;
    property boardPassBPe: String read FboardPassBPe write FboardPassBPe;
  end;

const
  CMUN_EXTERIOR: Integer = 9999999;
  XMUN_EXTERIOR: String = 'EXTERIOR';
  UF_EXTERIOR: String = 'EX';

implementation

uses
  ACBrUtil, pcnBPeR;

procedure TBPe.Assign(Source: TBPe);
begin
  infBPe.Assign(Source.infBPe);
  Ide.Assign(Source.Ide);
  Emit.Assign(Source.Emit);
  Comp.Assign(Source.Comp);
  Agencia.Assign(Source.Agencia);
  infBPeSub.Assign(Source.infBPeSub);
  infPassagem.Assign(Source.infPassagem);
  infViagem.Assign(Source.infViagem);
  infValorBPe.Assign(Source.infValorBPe);
  Imp.Assign(Source.Imp);
  infBPeSupl.Assign(Source.infBPeSupl);
  Signature.Assign(Source.Signature);
  procBPe.Assign(Source.procBPe);
end;

procedure TBPe.SetXMLString(const AValue: AnsiString);
var
 LocBPeR : TBPeR;
begin
  LocBPeR := TBPeR.Create(Self);
  try
    LocBPeR.Leitor.Arquivo := AValue;
    LocBPeR.LerXml;
  finally
    LocBPeR.Free
  end;
end;

constructor TBPe.Create;
begin
  inherited Create;
  FinfBPe      := TinfBPe.Create;
  FIde         := TIde.Create(Self);
  FEmit        := TEmit.Create(Self);
  FComp        := TComp.Create(Self);
  FAgencia     := TAgencia.Create(Self);
  FinfBPeSub   := TinfBPeSub.Create(Self);
  FinfPassagem := TinfPassagem.Create(Self);
  FinfViagem   := TInfViagemCollection.Create(Self);
  FinfValorBPe := TinfValorBPe.Create(Self);
  FImp         := TImp.Create(Self);
  Fpag         := TpagCollection.Create(Self);
  FautXML      := TautXMLCollection.Create(Self);
  FinfAdic     := TinfAdic.Create;
  FinfBPeSupl  := TinfBPeSupl.Create;
  FSignature   := TSignature.create;
  FProcBPe     := TProcBPe.create;
  FinfRespTec  := TinfRespTec.Create;

  FinfBPe.Versao := 0;
end;

destructor TBPe.Destroy;
begin
  FinfBPe.Free;
  FIde.Free;
  FEmit.Free;
  FComp.Free;
  FAgencia.Free;
  FinfBPeSub.Free;
  FinfPassagem.Free;
  FinfViagem.Free;
  FinfValorBPe.Free;
  FImp.Free;
  Fpag.Free;
  FautXML.Free;
  FinfAdic.Free;
  FinfBPeSupl.Free;
  FSignature.Free;
  FProcBPe.Free;
  FinfRespTec.Free;

  inherited Destroy;
end;

procedure TBPe.SetInfViagem(const Value: TInfViagemCollection);
begin
  FinfViagem := Value;
end;

procedure TBPe.SetPag(Value: TpagCollection);
begin
  Fpag.Assign(Value);
end;

procedure TBPe.SetautXML(const Value: TautXMLCollection);
begin
  FautXML := Value;
end;

{ TinfBPe }

procedure TinfBPe.Assign(Source: TPersistent);
begin
  if Source is TinfBPe then
  begin
    ID := TinfBPe(Source).ID;
    Versao := TinfBPe(Source).Versao;
  end
  else
    inherited;
end;

function TinfBPe.GetVersao: Real;
begin
  if FVersao <= 0 then
     Result := 1
  else
     Result := FVersao;
end;

function TinfBPe.GetVersaoStr: String;
begin
  if FVersao <= 0 then
     Result := V1_00
  else
     Result := 'versao="' + FloatToString(FVersao, '.', '#0.00') + '"';
end;

{Ide}

procedure TIde.Assign(Source: TPersistent);
begin
  if Source is TIde then
  begin
    cUF := TIde(Source).cUF;
    tpAmb := TIde(Source).tpAmb;
    modelo := TIde(Source).modelo;
    serie := TIde(Source).serie;
    nBP := TIde(Source).nBP;
    cBP := TIde(Source).cBP;
    cDV := TIde(Source).cDV;
    Modal := TIde(Source).modal;
    dhEmi := TIde(Source).dhEmi;
    tpEmis := TIde(Source).tpEmis;
    verProc := TIde(Source).verProc;
    tpBPe := TIde(Source).tpBPe;
    indPres := TIde(Source).indPres;
    UFIni := TIde(Source).UFIni;
    cMunIni := TIde(Source).cMunIni;
    UFFim := TIde(Source).UFFim;
    cMunFim := TIde(Source).cMunFim;
    dhCont := TIde(Source).dhCont;
    xJust := TIde(Source).xJust;
  end
  else
    inherited;
end;

constructor TIde.Create(AOwner: TBPe);
begin
  inherited Create;
end;

destructor TIde.Destroy;
begin
  inherited;
end;

{Emit}

procedure TEmit.Assign(Source: TPersistent);
begin
  if Source is TEmit then
  begin
    CNPJ := TEmit(Source).CNPJ;
    IE := TEmit(Source).IE;
    IEST := TEmit(Source).IEST;
    xNome := TEmit(Source).xNome;
    xFant := TEmit(Source).xFant;
    IM := TEmit(Source).IM;
    CNAE := TEmit(Source).CNAE;
    CRT := TEmit(Source).CRT;
    EnderEmit.Assign(TEmit(Source).EnderEmit);
    TAR := TEmit(Source).TAR;
  end
  else
    inherited;
end;

constructor TEmit.Create(AOwner: TBPe);
begin
  inherited Create;
  FEnderEmit := TEnderEmit.Create;
  FCRT:= crtRegimeNormal;
end;

destructor TEmit.Destroy;
begin
  FEnderEmit.Free;
  inherited;
end;

{ TenderEmit }

procedure TenderEmit.Assign(Source: TPersistent);
begin
  if Source is TenderEmit then
  begin
    xLgr := TenderEmit(Source).xLgr;
    nro := TenderEmit(Source).nro;
    xCpl := TenderEmit(Source).xCpl;
    xBairro := TenderEmit(Source).xBairro;
    cMun := TenderEmit(Source).cMun;
    xMun := TenderEmit(Source).xMun;
    CEP := TenderEmit(Source).CEP;
    UF := TenderEmit(Source).UF;
    fone := TenderEmit(Source).fone;
    Email := TenderEmit(Source).Email;
  end
  else
    inherited;
end;

{ TComp }

procedure TComp.Assign(Source: TPersistent);
begin
  if Source is TComp then
  begin
    xNome := TComp(Source).xNome;
    CNPJCPF := TComp(Source).CNPJCPF;
    idEstrangeiro := TComp(Source).idEstrangeiro;
    IE := TComp(Source).IE;
    EnderComp.Assign(TComp(Source).EnderComp);
  end
  else
    inherited;
end;

constructor TComp.Create(AOwner: TBPe);
begin
  inherited Create;
  FEnderComp := TEnderComp.Create;
end;

destructor TComp.Destroy;
begin
  FEnderComp.Free;
  inherited;
end;

{ TEnderComp }

procedure TEnderComp.Assign(Source: TPersistent);
begin
  if Source is TEnderComp then
  begin
    xLgr := TEnderComp(Source).xLgr;
    nro := TEnderComp(Source).nro;
    xCpl := TEnderComp(Source).xCpl;
    xBairro := TEnderComp(Source).xBairro;
    cMun := TEnderComp(Source).cMun;
    xMun := TEnderComp(Source).xMun;
    UF := TEnderComp(Source).UF;
    CEP := TEnderComp(Source).CEP;
    cPais := TEnderComp(Source).cPais;
    xPais := TEnderComp(Source).xPais;
    fone := TEnderComp(Source).fone;
    Email := TEnderComp(Source).Email;
  end
  else
    inherited;
end;

{ TAgencia }

procedure TAgencia.Assign(Source: TPersistent);
begin
  if Source is TAgencia then
  begin
    xNome := TAgencia(Source).xNome;
    CNPJ := TEmit(Source).CNPJ;
    EnderAgencia.Assign(TAgencia(Source).EnderAgencia);
  end
  else
    inherited;
end;

constructor TAgencia.Create(AOwner: TBPe);
begin
  inherited Create;
  FEnderAgencia := TEnderComp.Create;
end;

destructor TAgencia.Destroy;
begin
  FEnderAgencia.Free;
  inherited;
end;

{ TinfBPeSub }

procedure TinfBPeSub.Assign(Source: TPersistent);
begin
  if Source is TinfBPeSub then
  begin
    chBPe := TinfBPeSub(Source).chBPe;
    tpSub := TinfBPeSub(Source).tpSub;
  end
  else
    inherited;
end;

constructor TinfBPeSub.Create(AOwner: TBPe);
begin
  inherited Create;
end;

destructor TinfBPeSub.Destroy;
begin

  inherited;
end;

{ TinfPassagem }

procedure TinfPassagem.Assign(Source: TPersistent);
begin
  if Source is TinfPassagem then
  begin
    cLocOrig := TinfPassagem(Source).cLocOrig;
    xLocOrig := TinfPassagem(Source).xLocOrig;
    cLocDest := TinfPassagem(Source).cLocDest;
    xLocDest := TinfPassagem(Source).xLocDest;
    dhEmb := TinfPassagem(Source).dhEmb;
    dhValidade := TinfPassagem(Source).dhValidade;
    infPassageiro.Assign(TinfPassagem(Source).infPassageiro);
  end
  else
    inherited;
end;

constructor TinfPassagem.Create(AOwner: TBPe);
begin
  inherited Create;
  FinfPassageiro := TinfPassageiro.Create;
end;

destructor TinfPassagem.Destroy;
begin
  FinfPassageiro.Free;
  inherited;
end;

{ TinfPassageiro }

procedure TinfPassageiro.Assign(Source: TPersistent);
begin
  if Source is TinfPassageiro then
  begin
    xNome := TinfPassageiro(Source).xNome;
    CPF := TinfPassageiro(Source).CPF;
    tpDoc := TinfPassageiro(Source).tpDoc;
    nDoc := TinfPassageiro(Source).nDoc;
    xDoc := TinfPassageiro(Source).xDoc;
    dNasc := TinfPassageiro(Source).dNasc;
    Fone := TinfPassageiro(Source).Fone;
    Email := TinfPassageiro(Source).Email;
  end
  else
    inherited;
end;

{ TInfViagemCollection }

function TInfViagemCollection.GetItem(Index: Integer): TInfViagemCollectionItem;
begin
  Result := TInfViagemCollectionItem(inherited GetItem(Index));
end;

procedure TInfViagemCollection.SetItem(Index: Integer; Value: TInfViagemCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

constructor TInfViagemCollection.Create(AOwner: TBPe);
begin
  inherited Create(TInfViagemCollectionItem);
end;

destructor TInfViagemCollection.Destroy;
begin
  inherited;
end;

function TInfViagemCollection.Add: TInfViagemCollectionItem;
begin
  Result := TInfViagemCollectionItem(inherited Add)
end;

{ TInfViagemCollectionItem }

procedure TInfViagemCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TInfViagemCollectionItem then
  begin
    cPercurso := TInfViagemCollectionItem(Source).cPercurso;
    xPercurso := TInfViagemCollectionItem(Source).xPercurso;
    tpViagem := TInfViagemCollectionItem(Source).tpViagem;
    tpServ := TInfViagemCollectionItem(Source).tpServ;
    tpAcomodacao := TInfViagemCollectionItem(Source).tpAcomodacao;
    tpTrecho := TInfViagemCollectionItem(Source).tpTrecho;
    dhViagem := TInfViagemCollectionItem(Source).dhViagem;
    dhConexao := TInfViagemCollectionItem(Source).dhConexao;
    Prefixo := TInfViagemCollectionItem(Source).Prefixo;
    Poltrona := TInfViagemCollectionItem(Source).Poltrona;
    Plataforma := TInfViagemCollectionItem(Source).Plataforma;
    infTravessia.Assign(TInfViagemCollectionItem(Source).infTravessia);
  end
  else
    inherited;
end;

constructor TInfViagemCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  FinfTravessia := TinfTravessia.Create;
end;

destructor TInfViagemCollectionItem.Destroy;
begin
  FinfTravessia.Free;
  inherited;
end;

{ TinfTravessia }

procedure TinfTravessia.Assign(Source: TPersistent);
begin
  if Source is TInfTravessia then
  begin
    tpVeiculo := TInfTravessia(Source).tpVeiculo;
    sitVeiculo := TInfTravessia(Source).sitVeiculo;
  end
  else
    inherited;
end;

{ TinfValorBPe }

procedure TinfValorBPe.Assign(Source: TinfValorBPe);
begin
  vBP        := Source.vBP;
  vDesconto  := Source.vDesconto;
  vPgto      := Source.vPgto;
  vTroco     := Source.vTroco;
  tpDesconto := Source.tpDesconto;
  xDesconto  := Source.xDesconto;
  cDesconto  := Source.cDesconto;
  Comp       := Source.Comp;
end;

constructor TinfValorBPe.Create(AOwner: TBPe);
begin
  inherited Create;
  FComp := TCompCollection.Create(Self);
end;

destructor TinfValorBPe.Destroy;
begin
  FComp.Free;

  inherited Destroy;
end;

procedure TinfValorBPe.SetComp(const Value: TCompCollection);
begin
  FComp := Value;
end;

{ TCompCollection }

function TCompCollection.Add: TCompCollectionItem;
begin
  Result := TCompCollectionItem(inherited Add);
end;

constructor TCompCollection.Create(AOwner: TinfValorBPe);
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

procedure TCompCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TCompCollectionItem then
  begin
    tpComp := TCompCollectionItem(Source).FtpComp;
    vComp := TCompCollectionItem(Source).vComp;
  end
  else
    inherited;
end;

constructor TCompCollectionItem.Create(Collection: TCollection);
begin
  inherited;

end;

destructor TCompCollectionItem.Destroy;
begin

  inherited;
end;

{ TImp }

procedure TImp.Assign(Source: TPersistent);
begin
  if Source is TImp then
  begin
    ICMS := TImp(Source).ICMS;
    vTotTrib := TImp(Source).vTotTrib;
    infAdFisco := TImp(Source).infAdFisco;
    ICMSUFFim := TImp(Source).ICMSUFFim;
  end
  else
    inherited;
end;

constructor TImp.Create(AOwner: TBPe);
begin
  inherited Create;
  FICMS := TICMS.Create;
  FICMSUFFim := TICMSUFFim.Create;
end;

destructor TImp.Destroy;
begin
  FICMS.Free;
  FICMSUFFim.Free;
  inherited;
end;

{ TICMS }

procedure TICMS.Assign(Source: TPersistent);
begin
  if Source is TICMS then
  begin
    CST := TICMS(Source).CST;
    vBC := TICMS(Source).vBC;
    pICMS := TICMS(Source).pICMS;
    vICMS := TICMS(Source).vICMS;
    pRedBC := TICMS(Source).pRedBC;
    vCred := TICMS(Source).vCred;
    pRedBCOutraUF := TICMS(Source).pRedBCOutraUF;
    vBCOutraUF := TICMS(Source).vBCOutraUF;
    pICMSOutraUF := TICMS(Source).pICMSOutraUF;
    vICMSOutraUF := TICMS(Source).vICMSOutraUF;
  end
  else
    inherited;
end;

{ TICMSUFFim }

procedure TICMSUFFim.Assign(Source: TPersistent);
begin
  if Source is TICMSUFFim then
  begin
    vBCUFFim := TICMSUFFim(Source).vBCUFFim;
    pFCPUFFim := TICMSUFFim(Source).pFCPUFFim;
    pICMSUFFim := TICMSUFFim(Source).pICMSUFFim;
    pICMSInter := TICMSUFFim(Source).pICMSInter;
    pICMSInterPart := TICMSUFFim(Source).pICMSInterPart;
    vFCPUFFim := TICMSUFFim(Source).vFCPUFFim;
    vICMSUFFim := TICMSUFFim(Source).vICMSUFFim;
    vICMSUFIni := TICMSUFFim(Source).vICMSUFIni;
  end
  else
    inherited;
end;

{ TpagCollection }

function TpagCollection.Add: TpagCollectionItem;
begin
  Result := TpagCollectionItem(inherited Add);
end;

constructor TpagCollection.Create(AOwner: TBPe);
begin
  inherited Create(TpagCollectionItem);
end;

function TpagCollection.GetItem(Index: Integer): TpagCollectionItem;
begin
  Result := TpagCollectionItem(inherited GetItem(Index));
end;

procedure TpagCollection.SetItem(Index: Integer;
  Value: TpagCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TpagCollectionItem }

procedure TpagCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TpagCollectionItem then
  begin
    tPag := TpagCollectionItem(Source).tPag;
    xPag := TpagCollectionItem(Source).xPag;
    nDocPag := TpagCollectionItem(Source).nDocPag;
    vPag := TpagCollectionItem(Source).vPag;
    tpIntegra := TpagCollectionItem(Source).tpIntegra;
    CNPJ := TpagCollectionItem(Source).CNPJ;
    tBand := TpagCollectionItem(Source).tBand;
    xBand := TpagCollectionItem(Source).xBand;
    cAut := TpagCollectionItem(Source).cAut;
    nsuTrans := TpagCollectionItem(Source).nsuTrans;
    nsuHost := TpagCollectionItem(Source).nsuHost;
    nParcelas := TpagCollectionItem(Source).nParcelas;
    infAdCard := TpagCollectionItem(Source).infAdCard;
  end
  else
    inherited;
end;

{ TautXMLCollection }

function TautXMLCollection.Add: TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem(inherited Add);
////  Result.create;
end;

constructor TautXMLCollection.Create(AOwner: TBPe);
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

procedure TautXMLCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TautXMLCollectionItem then
  begin
    CNPJCPF := TautXMLCollectionItem(Source).CNPJCPF;
  end
  else
    inherited;
end;

constructor TautXMLCollectionItem.Create(Collection: TCollection);
begin
    inherited;
end;

destructor TautXMLCollectionItem.Destroy;
begin

  inherited;
end;

{infAdic}

procedure TInfAdic.Assign(Source: TPersistent);
begin
  if Source is TInfAdic then
  begin
    infAdFisco := TInfAdic(Source).infAdFisco;
    infCpl := TInfAdic(Source).infCpl;
  end
  else
    inherited;
end;

{ TinfBPeSupl }

procedure TinfBPeSupl.Assign(Source: TPersistent);
begin
  if Source is TinfBPeSupl then
  begin
    qrCodBPe := TinfBPeSupl(Source).qrCodBPe;
    boardPassBPe := TinfBPeSupl(Source).boardPassBPe;
  end
  else
    inherited;
end;

{ TinfRespTec }

procedure TinfRespTec.Assign(Source: TinfRespTec);
begin
  CNPJ     := Source.CNPJ;
  xContato := Source.xContato;
  email    := Source.email;
  fone     := Source.fone;
  idCSRT   := Source.idCSRT;
  hashCSRT := Source.hashCSRT;
end;

end.

