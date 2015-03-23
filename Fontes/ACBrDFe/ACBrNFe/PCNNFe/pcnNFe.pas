////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
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

{******************************************************************************
|* Historico
|*
|* 24/09/2012: Italo Jurisato Junior
|*  - Alterações para funcionamento com NFC-e
******************************************************************************}

{$I ACBr.inc}

unit pcnNFe;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnConversaoNFe, pcnSignature, pcnProcNFe, pcnGerador;

type

  TNFe = class;
  TInfNFe = class;
  TIde = class;
  TNFrefCollection = class;
  TNFrefCollectionItem = class;
  TRefNF = class;
  TRefNFP = class;
  TRefECF = class;  
  TEmit = class;
  TAvulsa = class;
  TenderEmit = class;
  TDest = class;
  TenderDest = class;
  TRetirada = class;
  TEntrega = class;
  TDetCollection = class;
  TDetCollectionItem = class;
  TProd = class;
  TveicProd = class;
  TmedCollection = class;
  TmedCollectionItem = class;
  TarmaCollection = class;
  TarmaCollectionItem = class;
  TComb = class;
  TCIDE = class;
  TICMSComb = class;
  TICMSInter = class;
  TICMSCons = class;
  TDICollection = class;
  TDICollectionItem = class;
  TAdiCollection = class;
  TAdiCollectionItem = class;
  TNVECollection = class;
  TNVECollectionItem = class;

  TdetExportCollection = class;
  TdetExportCollectionItem = class;

  TImposto = class;
  TICMS = class;
  TIPI = class;
  TII = class;
  TPIS = class;
  TPISST = class;
  TCOFINS = class;
  TCOFINSST = class;
  TISSQN = class;
  TTotal = class;
  TICMSTot = class;
  TISSQNtot = class;
  TretTrib = class;
  TTransp = class;
  TTransporta = class;
  TveicTransp = class;
  TretTransp = class;
  TreboqueCollection = class;
  TreboqueCollectionItem = class;
  TVolCollection = class;
  TVolCollectionItem = class;
  TLacresCollection = class;
  TLacresCollectionItem = class;
  TCobr = class;
  TFat = class;
  TDupCollection = class;
  TDupCollectionItem = class;
  TInfAdic = class;
  TobsContCollection = class;
  TobsContCollectionItem = class;
  TobsFiscoCollection = class;
  TobsFiscoCollectionItem = class;
  TprocRefCollection = class;
  TprocRefCollectionItem = class;
  TExporta = class;
  TCompra = class;
  TCana = class;
  TForDiaCollection = class;
  TForDiaCollectionItem = class;
  TDeducCollection = class;
  TDeducCollectionItem = class;
  TpagCollection = class;
  TpagCollectionItem = class;

  TautXMLCollection     = class;
  TautXMLCollectionItem = class;

  TNFe = class(TPersistent)
  private
    FinfNFe: TinfNFe;
    FIde: TIde;
    FEmit: TEmit;
    FAvulsa: TAvulsa;
    FDest: TDest;
    FRetirada: TRetirada;
    FEntrega: TEntrega;
    FDet: TDetCollection;
    FTotal: TTotal;
    FTransp: TTransp;
    FCobr: TCobr;
    Fpag: TpagCollection;
    FInfAdic: TInfAdic;
    Fexporta: Texporta;
    Fcompra: Tcompra;
    Fcana: Tcana;
    FSignature: TSignature;
    FProcNFe: TProcNFe;
    FautXML: TautXMLCollection;

    procedure SetDet(Value: TDetCollection);
    procedure Setpag(Value: TpagCollection);
    procedure SetautXML(const Value: TautXMLCollection);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property infNFe: TinfNFe read FinfNFe write FinfNFe;
    property Ide: TIde read FIde write FIde;
    property Emit: TEmit read FEmit write FEmit;
    property Avulsa: TAvulsa read FAvulsa write FAvulsa;
    property Dest: TDest read FDest write FDest;
    property Retirada: TRetirada read FRetirada write FRetirada;
    property Entrega: TEntrega read FEntrega write FEntrega;
    property autXML: TautXMLCollection read FautXML write SetautXML;
    property Det: TDetCollection read FDet write SetDet;
    property Total: TTotal read FTotal write FTotal;
    property Transp: TTransp read FTransp write FTransp;
    property Cobr: TCobr read FCobr write FCobr;
    property pag: TpagCollection read Fpag write Setpag;
    property InfAdic: TInfAdic read FInfAdic write FInfAdic;
    property exporta: Texporta read Fexporta write Fexporta;
    property compra: Tcompra read Fcompra write Fcompra;
    property cana: Tcana read Fcana write Fcana;
    property signature: Tsignature read Fsignature write Fsignature;
    property procNFe: TProcNFe read FProcNFe write FProcNFe;    
  end;

  TinfNFe = class(TPersistent)
  private
    FID: String;
    FVersao: Real;
    function GetVersaoStr: String;
    function GetVersao: Real;
  published
    property ID: String read FID write FID;
    property Versao: Real read GetVersao write FVersao;
    property VersaoStr: String read GetVersaoStr;
  end;

  TIde = class(TPersistent)
  private
    FcUF: Integer;
    FcNF: Integer;
    FnatOp: String;
    FindPag: TpcnIndicadorPagamento;
    Fmodelo: Integer;
    Fserie: Integer;
    FnNF: Integer;
    FdEmi: TDateTime;
    FdSaiEnt: TDateTime;
    FhSaiEnt: TDateTime;
    FtpNF: TpcnTipoNFe;
    FidDest: TpcnDestinoOperacao;
    FcMunFG: Integer;
    FNFref: TNFrefCollection;
    FrefNFP: TRefNFP;    
    FtpImp: TpcnTipoImpressao;
    FtpEmis: TpcnTipoEmissao;
    FcDV: Integer;
    FtpAmb: TpcnTipoAmbiente;
    FfinNFe : TpcnFinalidadeNFe;
    FindFinal: TpcnConsumidorFinal;
    FindPres: TpcnPresencaComprador;
    FprocEmi: TpcnProcessoEmissao;
    FverProc: String;
    FdhCont : TDateTime;
    FxJust  : String;

    procedure SetNFref(Value: TNFrefCollection);
  public
    constructor Create(AOwner: TNFe);
    destructor Destroy; override;
  published
    property cUF: Integer read FcUF write FcUF;
    property cNF: Integer read FcNF write FcNF;
    property natOp: String read FnatOp write FnatOp;
    property indPag: TpcnIndicadorPagamento read FindPag write FindPag default ipPrazo;
    property modelo: Integer read Fmodelo write Fmodelo;
    property serie: Integer read Fserie write Fserie;
    property nNF: Integer read FnNF write FnNF;
    property dEmi: TDateTime read FdEmi write FdEmi;
    property dSaiEnt: TDateTime read FdSaiEnt write FdSaiEnt;
    property hSaiEnt: TDateTime read FhSaiEnt write FhSaiEnt;
    property tpNF: TpcnTipoNFe read FtpNF write FtpNF default tnSaida;
    property idDest: TpcnDestinoOperacao read FidDest write FidDest;
    property cMunFG: Integer read FcMunFG write FcMunFG;
    property NFref: TNFrefCollection read FNFref write SetNFref;
    property refNFP: TRefNFP read FrefNFP write FrefNFP;
    property tpImp: TpcnTipoImpressao read FtpImp write FtpImp default tiPaisagem;
    property tpEmis: TpcnTipoEmissao read FtpEmis write FtpEmis default teNormal;
    property cDV: Integer read FcDV write FcDV;
    property tpAmb: TpcnTipoAmbiente read FtpAmb write FtpAmb default taHomologacao;
    property finNFe: TpcnFinalidadeNFe read FfinNFe write FfinNFe default fnNormal;
    property indFinal: TpcnConsumidorFinal read FindFinal write FindFinal;
    property indPres: TpcnPresencaComprador read FindPres write FindPres;
    property procEmi: TpcnProcessoEmissao read FprocEmi write FprocEmi default peAplicativoContribuinte;
    property verProc: String read FverProc write FverProc;
    property dhCont: TDateTime read FdhCont write FdhCont;
    property xJust: String read FxJust write FxJust;
  end;

  TNFrefCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TNFrefCollectionItem;
    procedure SetItem(Index: Integer; Value: TNFrefCollectionItem);
  public
    constructor Create(AOwner: TIde); reintroduce;
    function Add: TNFrefCollectionItem;
    property Items[Index: Integer]: TNFrefCollectionItem read GetItem write SetItem; default;
  end;

  TNFrefCollectionItem = class(TCollectionItem)
  private
    FrefNFe: String;
    FrefCTe: String;
    FRefNF: TRefNF;
    FRefECF: TRefECF;
    FRefNFP: TRefNFP;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property refNFe: String read FrefNFe write FrefNFe;
    property refCTe: String read FrefCTe write FrefCTe;
    property RefNF: TRefNF read FRefNF write FRefNF;
    property RefNFP: TRefNFP read FRefNFP write FRefNFP;
    property RefECF: TRefECF read FRefECF write FRefECF;
  end;

  TRefNF = class(TPersistent)
  private
    FcUF: Integer;
    FAAMM: String;
    FCNPJ: String;
    Fmodelo: Integer;
    Fserie: Integer;
    FnNF: Integer;
  published
    property cUF: Integer read FcUF write FcUF;
    property AAMM: String read FAAMM write FAAMM;
    property CNPJ: String read FCNPJ write FCNPJ;
    property modelo: Integer read FModelo write Fmodelo;
    property serie: Integer read FSerie write Fserie;
    property nNF: Integer read FnNF write FnNF;
  end;

  TRefNFP = class(TPersistent)
  private
    FcUF: Integer;
    FAAMM: String;
    FCNPJCPF: String;
    FIE: String;
    Fmodelo: String;
    Fserie: Integer;
    FnNF: Integer;
  published
    property cUF: Integer read FcUF write FcUF;
    property AAMM: String read FAAMM write FAAMM;
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property IE: String read FIE write FIE;
    property modelo: String read FModelo write Fmodelo;
    property serie: Integer read FSerie write Fserie;
    property nNF: Integer read FnNF write FnNF;
  end;

  TRefECF = class(TPersistent)
  private
    Fmodelo: TpcnECFModRef;
    FnECF: String;
    FnCOO: String;
  published
    property modelo:TpcnECFModRef read FModelo write Fmodelo default ECFModRefVazio;
    property nECF: String read FnECF write FnECF;
    property nCOO: String read FnCOO write FnCOO;
  end;


  TEmit = class(TPersistent)
  private
    FCNPJCPF: String;
    FxNome: String;
    FxFant: String;
    FenderEmit: TenderEmit;
    FIE: String;
    FIEST: String;
    FIM: String;
    FCNAE: String;
    FCRT: TpcnCRT;
  public
    constructor Create(AOwner: TNFe);
    destructor Destroy; override;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property xNome: String read FxNome write FxNome;
    property xFant: String read FxFant write FxFant;
    property EnderEmit: TEnderEmit read FEnderEmit write FEnderEmit;
    property IE: String read FIE write FIE;
    property IEST: String read FIEST write FIEST;
    property IM: String read FIM write FIM;
    property CNAE: String read FCNAE write FCNAE;
    property CRT: TpcnCRT read FCRT write FCRT;
  end;

  TenderEmit = class(TPersistent)
  private
    FxLgr: String;
    Fnro: String;
    fxCpl: String;
    FxBairro: String;
    FcMun: Integer;
    FxMun: String;
    FUF: String;
    FCEP: Integer;
    FcPais: Integer;
    FxPais: String;
    Ffone: String;
  published
    property xLgr: String read FxLgr write FxLgr;
    property nro: String read Fnro write Fnro;
    property xCpl: String read FxCpl write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: String read FxMun write FxMun;
    property UF: String read FUF write FUF;
    property CEP: Integer read FCEP write FCEP;
    property cPais: Integer read FcPais write FcPais ;
    property xPais: String read FxPais write FxPais ;
    property fone: String read Ffone write Ffone;
  end;

  TAvulsa = class(TPersistent)
  private
    FCNPJ: String;
    FxOrgao: String;
    Fmatr: String;
    FxAgente: String;
    Ffone: String;
    FUF: String;
    FnDAR: String;
    FdEmi: TDateTime;
    FvDAR: Currency;
    FrepEmi: String;
    FdPag: TDateTime;
  published
    property CNPJ: String read FCNPJ write FCNPJ;
    property xOrgao: String read FxOrgao write FxOrgao;
    property matr: String read Fmatr write Fmatr;
    property xAgente: String read FxAgente write FxAgente;
    property fone: String read Ffone write Ffone;
    property UF: String read FUF write FUF;
    property nDAR: String read FnDAR write FnDAR;
    property dEmi: TDateTime read FdEmi write FdEmi;
    property vDAR: Currency read FvDAR write FvDAR;
    property repEmi: String read FrepEmi write FrepEmi;
    property dPag: TDateTime read FdPag write FdPag;
  end;

  TDest = class(TPersistent)
  private
    FCNPJCPF: String;
    FidEstrangeiro: String;
    FxNome: String;
    FEnderDest: TEnderDest;
    FindIEDest: TpcnindIEDest;
    FIE: String;
    FISUF: String;
    FIM: String;
    Femail: String;
  public
    constructor Create(AOwner: TNFe);
    destructor Destroy; override;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property idEstrangeiro: String read FidEstrangeiro write FidEstrangeiro;
    property xNome: String read FxNome write FxNome;
    property EnderDest: TEnderDest read FEnderDest write FEnderDest;
    property indIEDest: TpcnindIEDest read FindIEDest write FindIEDest;
    property IE: String read FIE write FIE;
    property ISUF: String read FISUF write FISUF;
    property IM: String read FIM write FIM;
    property Email: String read Femail write Femail;
  end;

  TEnderDest = class(TPersistent)
  private
    FxLgr: String;
    Fnro: String;
    fxCpl: String;
    FxBairro: String;
    FcMun: Integer;
    FxMun: String;
    FUF: String;
    FCEP: Integer;
    FcPais: Integer;
    FxPais: String;
    Ffone: String;
  published
    property xLgr: String read FxLgr write FxLgr;
    property nro: String read Fnro write Fnro;
    property xCpl: String read FxCpl write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: String read FxMun write FxMun;
    property UF: String read FUF write FUF;
    property CEP: Integer read FCEP write FCEP;
    property cPais: Integer read FcPais write FcPais ;
    property xPais: String read FxPais write FxPais ;
    property fone: String read Ffone write Ffone;
  end;

  TRetirada = class(TPersistent)
  private
    FCNPJCPF: String;
    FxLgr: String;
    Fnro: String;
    fxCpl: String;
    FxBairro: String;
    FcMun: Integer;
    FxMun: String;
    FUF: String;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property xLgr: String read FxLgr write FxLgr;
    property nro: String read Fnro write Fnro;
    property xCpl: String read FxCpl write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: String read FxMun write FxMun;
    property UF: String read FUF write FUF;
  end;

  TEntrega = class(TPersistent)
  private
    FCNPJCPF: String;
    FxLgr: String;
    Fnro: String;
    fxCpl: String;
    FxBairro: String;
    FcMun: Integer;
    FxMun: String;
    FUF: String;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property xLgr: String read FxLgr write FxLgr;
    property nro: String read Fnro write Fnro;
    property xCpl: String read FxCpl write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: String read FxMun write FxMun;
    property UF: String read FUF write FUF;
  end;

  TDetCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDetCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetCollectionItem);
  public
    constructor Create(AOwner: TNFe);
    function Add: TDetCollectionItem;
    property Items[Index: Integer]: TDetCollectionItem read GetItem write SetItem; default;
  end;

  TDetCollectionItem = class(TCollectionItem)
  private
    FProd: TProd;
    FImposto: TImposto;
    FpDevol: Currency;
    FvIPIDevol: Currency;
    FinfAdProd: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property Prod: TProd read FProd write FProd;
    property Imposto: TImposto read FImposto write FImposto;
    property pDevol: Currency read FpDevol write FpDevol;
    property vIPIDevol: Currency read FvIPIDevol write FvIPIDevol;
    property infAdProd: String read FinfAdProd write FinfAdProd;
  end;

  TProd = class(TPersistent)
  private
    FcProd: String;
    FnItem: Integer;
    FcEAN: String;
    FxProd: String;
    FNCM: String;
    FEXTIPI: String;
    //Fgenero: Integer;
    FCFOP: String;
    FuCom: String;
    FqCom: Currency;
    FvUnCom: Double;
    FvProd: Currency;
    FcEANTrib: String;
    FuTrib: String;
    FqTrib: Currency;
    FvUnTrib: Double;
    FvFrete: Currency;
    FvSeg: Currency;
    FvDesc: Currency;
    FvOutro: Currency;
    FIndTot: TpcnIndicadorTotal;
    FDI: TDICollection;
    FxPed: String;
    FnItemPed: Integer;
    FdetExport: TdetExportCollection;
    FveicProd: TveicProd;
    Fmed: TMedCollection;
    Farma: TarmaCollection;
    Fcomb: Tcomb;
    FnRECOPI: String;
    FnFCI: String;
    FNVE: TNVECollection;

    procedure SetDI(Value: TDICollection);
    procedure SetMed(Value: TmedCollection);
    procedure SetArma(Value: TarmaCollection);
    procedure SetdetExport(const Value: TdetExportCollection);
    procedure SetNVE(Value : TNVeCollection);
    procedure getCFOP(const Value: String);
  public
    constructor Create(AOwner: TDetcollectionItem);
    destructor Destroy; override;
  published
    property cProd: String read FcProd write FcProd;
    property nItem: Integer read FnItem write FnItem;
    property cEAN: String read FcEAN write FcEAN;
    property xProd: String read FxProd write FxProd;
    property NCM: String read FNCM write FNCM;
    property NVE : TNVECollection read FNVE write FNVE;
    property EXTIPI: String read FEXTIPI write FEXTIPI;
    //property genero: Integer read Fgenero write Fgenero;
    property CFOP: String read FCFOP write getCFOP;
    property uCom: String read FuCom write FuCom;
    property qCom: Currency read FqCom write FqCom;
    property vUnCom: Double read FvUnCom write FvUnCom;
    property vProd: Currency read FvProd write FvProd;
    property cEANTrib: String read FcEANTrib write FcEANTrib;
    property uTrib: String read FuTrib write FuTrib;
    property qTrib: Currency read FqTrib write FqTrib;
    property vUnTrib: Double read FvUnTrib write FvUnTrib;
    property vFrete: Currency read FvFrete write FvFrete;
    property vSeg: Currency read FvSeg write FvSeg;
    property vDesc: Currency read FvDesc write FvDesc;
    property vOutro: Currency read FvOutro write FvOutro;
    property IndTot: TpcnIndicadorTotal read FIndTot write FIndTot default itSomaTotalNFe;
    property DI: TDICollection read FDI write SetDI;
    property xPed: String read FxPed write FxPed;
    property nItemPed : Integer read FnItemPed write FnItemPed;
    property detExport: TdetExportCollection read FdetExport write SetdetExport;
    property veicProd: TveicProd read FveicProd write FveicProd;
    property med: TMedCollection read Fmed write SetMed;
    property arma: TarmaCollection read Farma write SetArma;
    property comb: Tcomb read Fcomb write Fcomb;
    property nRECOPI: String read FnRECOPI write FnRECOPI;
    property nFCI: String read FnFCI write FnFCI;
  end;

  TveicProd = class(TPersistent)
  private
    FtpOP: TpcnTipoOperacao;
    Fchassi: String;
    FcCor: String;
    FxCor: String;
    Fpot: String;
    FCilin: String;
    FpesoL: String;
    FpesoB: String;
    FnSerie: String;
    FtpComb: String;
    FnMotor: String;
    FCMT: String;
    Fdist: String;
    //FRENAVAM: String;
    FanoMod: Integer;
    FanoFab: Integer;
    FtpPint: String;
    FtpVeic: Integer;
    FespVeic: Integer;
    FVIN: String;
    FcondVeic: TpcnCondicaoVeiculo;
    FcMod: String;
    FcCorDENATRAN: String;
    Flota: Integer;
    FtpRest: Integer;

    function getCombDescricao: String;
  published
    property tpOP: TpcnTipoOperacao read FtpOP write FtpOP;
    property chassi: String read Fchassi write Fchassi;
    property cCor: String read FcCor write FcCor;
    property xCor: String read FxCor write FxCor;
    property pot: String read Fpot write Fpot;
    property Cilin: String read FCilin write FCilin;
    property pesoL: String read FpesoL write FpesoL;
    property pesoB: String read FpesoB write FpesoB;
    property nSerie: String read FnSerie write FnSerie;
    property tpComb: String read FtpComb write FtpComb;
    property CombDescricao: String read getCombDescricao;
    property nMotor: String read FnMotor write FnMotor;
    property CMT: String read FCMT write FCMT;
    property dist: String read Fdist write Fdist;
    //property RENAVAM: String read FRENAVAM write FRENAVAM;
    property anoMod: Integer read FanoMod write FanoMod;
    property anoFab: Integer read FanoFab write FanoFab;
    property tpPint: String read FtpPint write FtpPint;
    property tpVeic: Integer read FtpVeic write FtpVeic;
    property espVeic: Integer read FespVeic write FespVeic;
    property VIN: String read FVIN write FVIN;
    property condVeic: TpcnCondicaoVeiculo read FcondVeic write FcondVeic;
    property cMod: String read FcMod write FcMod;
    property cCorDENATRAN: String read FcCorDENATRAN write FcCorDENATRAN;
    property lota: Integer read Flota write Flota;
    property tpRest: Integer read FtpRest write FtpRest;
  end;

  TMedCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TMedCollectionItem;
    procedure SetItem(Index: Integer; Value: TMedCollectionItem);
  public
    constructor Create(AOwner: TProd);
    destructor Destroy; override;
    function Add: TMedCollectionItem;
    property Items[Index: Integer]: TMedCollectionItem read GetItem write SetItem; default;
  end;

  TMedCollectionItem = class(TCollectionItem)
  private
    FnLote: String;
    FqLote: Currency;
    FdFab: TDateTime;
    FdVal: TDateTime;
    FvPMC: Currency;
  published
    property nLote: String read FnLote write FnLote;
    property qLote: Currency read FqLote write FqLote;
    property dFab: TDateTime read FdFab write FdFab;
    property dVal: TDateTime read FdVal write FdVal;
    property vPMC: Currency read FvPMC write FvPMC;
  end;

  TArmaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TArmaCollectionItem;
    procedure SetItem(Index: Integer; Value: TArmaCollectionItem);
  public
    constructor Create(AOwner: TProd);
    destructor Destroy; override;
    function Add: TArmaCollectionItem;
    property Items[Index: Integer]: TArmaCollectionItem read GetItem write SetItem; default;
  end;

  TArmaCollectionItem = class(TCollectionItem)
  private
    FtpArma: TpcnTipoArma;
    FnSerie: String;
    FnCano: String;
    Fdescr: String;
  published
    property tpArma: TpcnTipoArma read FtpArma write FtpArma default taUsoPermitido;
    property nSerie: String read FnSerie write FnSerie;
    property nCano: String read FnCano write FnCano;
    property descr: String read Fdescr write Fdescr;
  end;

  Tcomb = class(TPersistent)
  private
    FcProdANP: Integer;
    FpMixGN: Currency;
    FCODIF: String;
    FqTemp: Currency;
    FUFcons: String;
    FCIDE: TCIDE;
    FICMS: TICMSComb;
    FICMSInter: TICMSInter;
    FICMSCons: TICMSCons;
  public
    constructor Create(AOwner: TProd);
    destructor Destroy; override;
  published
    property cProdANP: Integer read FcProdANP write FcProdANP;
    property pMixGN: Currency read FpMixGN write FpMixGN;
    property CODIF: String read FCODIF write FCODIF;
    property qTemp: Currency read FqTemp write FqTemp;
    property UFcons: String read FUFcons write FUFcons;
    property CIDE: TCIDE read FCIDE write FCIDE;
    property ICMS: TICMSComb read FICMS write FICMS;
    property ICMSInter: TICMSInter read FICMSInter write FICMSInter;
    property ICMSCons: TICMSCons read FICMSCons write FICMSCons;
  end;

  TCIDE = class(TPersistent)
  private
    FqBCProd: Currency;
    FvAliqProd: Currency;
    FvCIDE: Currency;
  published
    property qBCProd: Currency read FqBCProd write FqBCProd;
    property vAliqProd: Currency read FvAliqProd write FvAliqProd;
    property vCIDE: Currency read FvCIDE write FvCIDE;
  end;

  TICMSComb = class(TPersistent)
  private
    FvBCICMS: Currency;
    FvICMS: Currency;
    FvBCICMSST: Currency;
    FvICMSST: Currency;
  published
    property vBCICMS: Currency read FvBCICMS write FvBCICMS;
    property vICMS: Currency read FvICMS write FvICMS;
    property vBCICMSST: Currency read FvBCICMSST write FvBCICMSST;
    property vICMSST: Currency read FvICMSST write FvICMSST;
  end;

  TICMSInter = class(TPersistent)
  private
    FvBCICMSSTDest: Currency;
    FvICMSSTDest: Currency;
  published
    property vBCICMSSTDest: Currency read FvBCICMSSTDest write FvBCICMSSTDest;
    property vICMSSTDest: Currency read FvICMSSTDest write FvICMSSTDest;
  end;

  TICMSCons = class(TPersistent)
  private
    FvBCICMSSTCons: Currency;
    FvICMSSTCons: Currency;
    FUFcons: String;
  published
    property vBCICMSSTCons: Currency read FvBCICMSSTCons write FvBCICMSSTCons;
    property vICMSSTCons: Currency read FvICMSSTCons write FvICMSSTCons;
    property UFcons: String read FUFcons write FUFcons;
  end;

  TDICollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDICollectionItem;
    procedure SetItem(Index: Integer; Value: TDICollectionItem);
  public
    constructor Create(AOwner: TProd);
    function Add: TDICollectionItem;
    property Items[Index: Integer]: TDICollectionItem read GetItem write SetItem; default;
  end;

  TDICollectionItem = class(TCollectionItem)
  private
    FnDi: String;
    FdDi: TDateTime;
    FxLocDesemb: String;
    FUFDesemb: String;
    FdDesemb: TDateTime;
    FtpViaTransp: TpcnTipoViaTransp;
    FvAFRMM: Currency;
    FtpIntermedio: TpcnTipoIntermedio;
    FCNPJ: String;
    FUFTerceiro: String;
    FcExportador: String;
    Fadi: TadiCollection;

    procedure SetAdi(Value: TAdiCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nDi: String read FnDi write FnDi;
    property dDi: TDateTime read FdDi write FdDi;
    property xLocDesemb: String read FxLocDesemb write FxLocDesemb;
    property UFDesemb: String read FUFDesemb write FUFDesemb;
    property dDesemb: TDateTime read FdDesemb write FdDesemb;
    property tpViaTransp: TpcnTipoViaTransp read FtpViaTransp write FtpViaTransp;
    property vAFRMM: Currency read FvAFRMM write FvAFRMM;
    property tpIntermedio: TpcnTipoIntermedio read FtpIntermedio write FtpIntermedio;
    property CNPJ: String read FCNPJ write FCNPJ;
    property UFTerceiro: String read FUFTerceiro write FUFTerceiro;
    property cExportador: String read FcExportador write FcExportador;
    property adi: TAdiCollection read Fadi write SetAdi;
  end;

  TAdiCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TAdiCollectionItem;
    procedure SetItem(Index: Integer; Value: TAdiCollectionItem);
  public
    constructor Create(AOwner: TDICollectionItem);
    function Add: TAdiCollectionItem;
    property Items[Index: Integer]: TAdiCollectionItem read GetItem write SetItem; default;
  end;

  TAdiCollectionItem = class(TCollectionItem)
  private
    FnAdicao: Integer;
    FnSeqAdi: Integer;
    FcFabricante: String;
    FvDescDI: Currency;
    FnDraw: String;
  published
    property nAdicao: Integer read FnAdicao write FnAdicao;
    property nSeqAdi: Integer read FnSeqAdi write FnSeqAdi;
    property cFabricante: String read FcFabricante write FcFabricante;
    property vDescDI: Currency read FvDescDI write FvDescDI;
    property nDraw: String read FnDraw write FnDraw;
  end;

  TNVECollection = class(TCollection)
  private
    function GetItem(Index: Integer): TNVECollectionItem;
    procedure SetItem(Index: Integer; Value: TNVECollectionItem);
  public
    constructor Create(AOwner: TProd);
    function Add: TNVECollectionItem;
    property Items[Index: Integer]: TNVECollectionItem read GetItem write SetItem; default;
  end;

  TNVECollectionItem = class(TCollectionItem)
  private
    FNve: String;
  published
    property NVE: String read FNve write FNve;
  end;

  TdetExportCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TdetExportCollectionItem;
    procedure SetItem(Index: Integer; Value: TdetExportCollectionItem);
  public
    constructor Create(AOwner: TProd);
    function Add: TdetExportCollectionItem;
    property Items[Index: Integer]: TdetExportCollectionItem read GetItem write SetItem; default;
  end;

  TdetExportCollectionItem = class(TCollectionItem)
  private
    FnDraw: String;
    FnRE: String;
    FchNFe: String;
    FqExport: Currency;
  published
    property nDraw: String read FnDraw write FnDraw;
    property nRE: String read FnRE write FnRE;
    property chNFe: String read FchNFe write FchNFe;
    property qExport: Currency read FqExport write FqExport;
  end;

  TImposto = class(TPersistent)
  private
    FvTotTrib: Currency;
    FICMS: TICMS;
    FIPI: TIPI;
    FII: TII;
    FPIS: TPIS;
    FPISST: TPISST;
    FCOFINS: TCOFINS;
    FCOFINSST: TCOFINSST;
    FISSQN: TISSQN;
  public
    constructor Create(AOwner: TDetcollectionItem);
    destructor Destroy; override;
  published
    property vTotTrib: Currency read FvTotTrib write FvTotTrib;
    property ICMS: TICMS read FICMS write FICMS;
    property IPI: TIPI read FIPI write FIPI;
    property II: TII read FII write FII;
    property PIS: TPIS read FPIS write FPIS;
    property PISST: TPISST read FPISST write FPISST;
    property COFINS: TCOFINS read FCOFINS write FCOFINS;
    property COFINSST: TCOFINSST read FCOFINSST write FCOFINSST;
    property ISSQN: TISSQN read FISSQN write FISSQN;
  end;

  TICMS = class(TPersistent)
  private
    Forig: TpcnOrigemMercadoria;          //N11
    FCST: TpcnCSTIcms;                    //N12
    FCSOSN: TpcnCSOSNIcms;                //N12a
    FmodBC: TpcnDeterminacaoBaseIcms;     //N13
    FpRedBC: Currency;                    //N14
    FvBC: Currency;                       //N15
    FpICMS: Currency;                     //N16
    FvICMS: Currency;                     //N17
    FmodBCST: TpcnDeterminacaoBaseIcmsST; //N18
    FpMVAST: Currency;                    //N19
    FpRedBCST: Currency;                  //N20
    FvBCST: Currency;                     //N21
    FpICMSST: Currency;                   //N22
    FvICMSST: Currency;                   //N23
    FUFST: String;                        //N24
    FpBCOp: Currency;                     //N25
    FvBCSTRet: Currency;                  //N26
    FvICMSSTRet: Currency;                //N27
    FmotDesICMS: TpcnMotivoDesoneracaoICMS; //N28
    FpCredSN: Currency;                   //N29
    FvCredICMSSN: Currency;               //N30
    FvBCSTDest: Currency;                 //N31
    FvICMSSTDest: Currency;               //N32
    FvICMSDeson: Currency;
    FvICMSOp: Currency;
    FpDif: Currency;
    FvICMSDif: Currency;
  published
    property orig: TpcnOrigemMercadoria read Forig write Forig default oeNacional;
    property CST: TpcnCSTIcms read FCST write FCST default cst00;
    property CSOSN: TpcnCSOSNIcms read FCSOSN write FCSOSN;
    property modBC: TpcnDeterminacaoBaseIcms read FmodBC write FmodBC default dbiMargemValorAgregado;
    property pRedBC: Currency read FpRedBC write FpRedBC;
    property vBC: Currency read FvBC write FvBC;
    property pICMS: Currency read FpICMS write FpICMS;
    property vICMS: Currency read FvICMS write FvICMS;
    property modBCST: TpcnDeterminacaoBaseIcmsST read FmodBCST write FmodBCST default dbisPrecoTabelado;
    property pMVAST: Currency read FpMVAST write FpMVAST;
    property pRedBCST: Currency read FpRedBCST write FpRedBCST;
    property vBCST: Currency read FvBCST write FvBCST;
    property pICMSST: Currency read FpICMSST write FpICMSST;
    property vICMSST: Currency read FvICMSST write FvICMSST;
    property UFST: String read FUFST write FUFST;
    property pBCOp: Currency read FpBCOp write FpBCOp;
    property vBCSTRet: Currency read FvBCSTRet write FvBCSTRet;
    property vICMSSTRet: Currency read FvICMSSTRet write FvICMSSTRet;
    property motDesICMS: TpcnMotivoDesoneracaoICMS read FmotDesICMS write FmotDesICMS;
    property pCredSN: Currency read FpCredSN write FpCredSN;
    property vCredICMSSN: Currency read FvCredICMSSN write FvCredICMSSN;
    property vBCSTDest: Currency read FvBCSTDest write FvBCSTDest;
    property vICMSSTDest: Currency read FvICMSSTDest write FvICMSSTDest;
    property vICMSDeson: Currency read FvICMSDeson write FvICMSDeson;
    property vICMSOp: Currency read FvICMSOp write FvICMSOp;
    property pDif: Currency read FpDif write FpDif;
    property vICMSDif: Currency read FvICMSDif write FvICMSDif;
  end;

  TIPI = class(TPersistent)
  private
    FclEnq: String;
    FCNPJProd: String;
    FcSelo: String;
    FqSelo: Integer;
    FcEnq: String;
    FCST: TpcnCstIpi;
    FvBC: Currency;
    FqUnid: Currency;
    FvUnid: Currency;
    FpIPI: Currency;
    FvIPI: Currency;
  published
    property clEnq: String read FclEnq write FclEnq;
    property CNPJProd: String read FCNPJProd write FCNPJProd;
    property cSelo: String read FcSelo write FcSelo;
    property qSelo: Integer read FqSelo write FqSelo;
    property cEnq: String read FcEnq write FcEnq;
    property CST: TpcnCstIpi read FCST write FCST default ipi00;
    property vBC: Currency read FvBC write FvBC;
    property qUnid: Currency read FqUnid write FqUnid;
    property vUnid: Currency read FvUnid write FvUnid;
    property pIPI: Currency read FpIPI write FpIPI;
    property vIPI: Currency read FvIPI write FvIPI;
  end;

  TII = class(TPersistent)
  private
    FvBc: Currency;
    FvDespAdu: Currency;
    FvII: Currency;
    FvIOF: Currency;
  published
    property vBc: Currency read FvBC write FvBC;
    property vDespAdu: Currency read FvDespAdu write FvDespAdu;
    property vII: Currency read FvII write FvII;
    property vIOF: Currency read FvIOF write FvIOF;
  end;

  TPIS = class(TPersistent)
  private
    FCST: TpcnCstPis;
    FvBC: Currency;
    FpPIS: Currency;
    FvPIS: Currency;
    FqBCProd: Currency;
    FvAliqProd: Currency;
  published
    property CST: TpcnCstPis read FCST write FCST default pis01;
    property vBC: Currency read FvBC write FvBC;
    property pPIS: Currency read FpPIS write FpPIS;
    property vPIS: Currency read FvPIS write FvPIS;
    property qBCProd: Currency read FqBCProd write FqBCProd;
    property vAliqProd: Currency read FvAliqProd write FvAliqProd;
  end;

  TPISST = class(TPersistent)
  private
    FvBc: Currency;
    FpPis: Currency;
    FqBCProd: Currency;
    FvAliqProd: Currency;
    FvPIS: Currency;
  published
    property vBc: Currency read FvBc write FvBc;
    property pPis: Currency read FpPis write FpPis;
    property qBCProd: Currency read FqBCProd write FqBCProd;
    property vAliqProd: Currency read FvAliqProd write FvAliqProd;
    property vPIS: Currency read FvPIS write FvPIS;
  end;

  TCOFINS = class(TPersistent)
  private
    FCST: TpcnCstCofins;
    FvBC: Currency;
    FpCOFINS: Currency;
    FvCOFINS: Currency;
    FvBCProd: Currency;
    FvAliqProd: Currency;
    FqBCProd: Currency;
  published
    property CST: TpcnCstCofins read FCST write FCST default cof01;
    property vBC: Currency read FvBC write FvBC;
    property pCOFINS: Currency read FpCOFINS write FpCOFINS;
    property vCOFINS: Currency read FvCOFINS write FvCOFINS;
    property vBCProd: Currency read FvBCProd write FvBCProd;
    property vAliqProd: Currency read FvAliqProd write FvAliqProd;
    property qBCProd: Currency read FqBCProd write FqBCProd;
  end;

  TTotal = class(TPersistent)
  private
    FICMSTot: TICMSTot;
    FISSQNtot: TISSQNtot;
    FretTrib: TretTrib;
  public
    constructor Create(AOwner: TNFe);
    destructor Destroy; override;
  published
    property ICMSTot: TICMSTot read FICMSTot write FICMSTot;
    property ISSQNtot: TISSQNtot read FISSQNtot write FISSQNtot;
    property retTrib: TretTrib read FretTrib write FretTrib;
  end;

  TICMSTot = class(TPersistent)
  private
    FvBC: Currency;
    FvICMS: Currency;
    FvICMSDeson: Currency;
    FvBCST: Currency;
    FvST: Currency;
    FvProd: Currency;
    FvFrete: Currency;
    FvSeg: Currency;
    FvDesc: Currency;
    FvII: Currency;
    FvIPI: Currency;
    FvPIS: Currency;
    FvCOFINS: Currency;
    FvOutro: Currency;
    FvNF: Currency;
    FvTotTrib: Currency;
  published
    property vBC: Currency read FvBC write FvBC;
    property vICMS: Currency read FvICMS write FvICMS;
    property vICMSDeson: Currency read FvICMSDeson write FvICMSDeson;
    property vBCST: Currency read FvBCST write FvBCST;
    property vST: Currency read FvST write FvST;
    property vProd: Currency read FvProd write FvProd;
    property vFrete: Currency read FvFrete write FvFrete;
    property vSeg: Currency read FvSeg write FvSeg;
    property vDesc: Currency read FvDesc write FvDesc;
    property vII: Currency read FvII write FvII;
    property vIPI: Currency read FvIPI write FvIPI;
    property vPIS: Currency read FvPIS write FvPIS;
    property vCOFINS: Currency read FvCOFINS write FvCOFINS;
    property vOutro: Currency read FvOutro write FvOutro;
    property vNF: Currency read FvNF write FvNF;
    property vTotTrib: Currency read FvTotTrib write FvTotTrib;
  end;

  TISSQNtot = class(TPersistent)
  private
    FvServ: Currency;
    FvBC: Currency;
    FvISS: Currency;
    FvPIS: Currency;
    FvCOFINS: Currency;
    FdCompet: TDateTime;
    FvDeducao: Currency;
//    FvINSS: Currency;
//    FvIR: Currency;
//    FvCSLL: Currency;
    FvOutro: Currency;
    FvDescIncond: Currency;
    FvDescCond: Currency;
//    FindISSRet: TpcnindISSRet;
//    FindISS: TpcnindISS;
//    FcServico: String;
//    FcMun: Integer;
//    FcPais: Integer;
//    FnProcesso: String;
    FvISSRet: Currency;
    FcRegTrib: TpcnRegTribISSQN;
//    FindIncentivo: TpcnindIncentivo;
  published
    property vServ: Currency read FvServ write FvServ;
    property vBC: Currency read FvBC write FvBC;
    property vISS: Currency read FvISS write FvISS;
    property vPIS: Currency read FvPIS write FvPIS;
    property vCOFINS: Currency read FvCOFINS write FvCOFINS;
    property dCompet: TDateTime read FdCompet write FdCompet;
    property vDeducao: Currency read FvDeducao write FvDeducao;
//    property vINSS: Currency read FvINSS write FvINSS;
//    property vIR: Currency read FvIR write FvIR;
//    property vCSLL: Currency read FvCSLL write FvCSLL;
    property vOutro: Currency read FvOutro write FvOutro;
    property vDescIncond: Currency read FvDescIncond write FvDescIncond;
    property vDescCond: Currency read FvDescCond write FvDescCond;
//    property indISSRet: TpcnindISSRet read FindISSRet write FindISSRet;
//    property indISS: TpcnindISS read FindISS write FindISS;
//    property cServico: String read FcServico write FcServico;
//    property cMun: Integer read FcMun write FcMun;
//    property cPais: Integer read FcPais write FcPais;
//    property nProcesso: String read FnProcesso write FnProcesso;
    property vISSRet: Currency read FvISSRet write FvISSRet;
    property cRegTrib: TpcnRegTribISSQN read FcRegTrib write FcRegTrib;
//    property indIncentivo: TpcnindIncentivo read FindIncentivo write FindIncentivo;
  end;

  TretTrib = class(TPersistent)
  private
    FvRetPIS: Currency;
    FvRetCOFINS: Currency;
    FvRetCSLL: Currency;
    FvBCIRRF: Currency;
    FvIRRF: Currency;
    FvBCRetPrev: Currency;
    FvRetPrev: Currency;
  public
  published
    property vRetPIS: Currency read FvRetPIS write FvRetPIS;
    property vRetCOFINS: Currency read FvRetCOFINS write FvRetCOFINS;
    property vRetCSLL: Currency read FvRetCSLL write FvRetCSLL;
    property vBCIRRF: Currency read FvBCIRRF write FvBCIRRF;
    property vIRRF: Currency read FvIRRF write FvIRRF;
    property vBCRetPrev: Currency read FvBCRetPrev write FvBCRetPrev;
    property vRetPrev: Currency read FvRetPrev write FvRetPrev;
  end;

  TCOFINSST = class(TPersistent)
  private
    FvBC: Currency;
    FpCOFINS: Currency;
    FqBCProd: Currency;
    FvAliqProd: Currency;
    FvCOFINS: Currency;
  published
    property vBC: Currency read FvBC write FvBC;
    property pCOFINS: Currency read FpCOFINS write FpCOFINS;
    property qBCProd: Currency read FqBCProd write FqBCProd;
    property vAliqProd: Currency read FvAliqProd write FvAliqProd;
    property vCOFINS: Currency read FvCOFINS write FvCOFINS;
  end;

  TISSQN = class(TPersistent)
  private
    FvBC: Currency;
    FvAliq: Currency;
    FvISSQN: Currency;
    FcMunFG: Integer;
    FcListServ: String;
    FcSitTrib: TpcnISSQNcSitTrib;
    FvDeducao: Currency;
    FvOutro: Currency;
    FvDescIncond: Currency;
    FvDescCond: Currency;
    FindISSRet: TpcnindISSRet;
    FvISSRet: Currency;
    FindISS: TpcnindISS;
    FcServico: String;
    FcMun: Integer;
    FcPais: Integer;
    FnProcesso: String;
    FindIncentivo: TpcnindIncentivo;
  public
  published
    property vBC: Currency read FvBC write FvBC;
    property vAliq: Currency read FvAliq write FvAliq;
    property vISSQN: Currency read FvISSQN write FvISSQN;
    property cMunFG: Integer read FcMunFG write FcMunFG;
    property cListServ: String read FcListServ write FcListServ;
    property cSitTrib: TpcnISSQNcSitTrib read FcSitTrib write FcSitTrib default ISSQNcSitTribVazio;
    property vDeducao: Currency read FvDeducao write FvDeducao;
    property vOutro: Currency read FvOutro write FvOutro;
    property vDescIncond: Currency read FvDescIncond write FvDescIncond;
    property vDescCond: Currency read FvDescCond write FvDescCond;
    property indISSRet: TpcnindISSRet read FindISSRet write FindISSRet;
    property vISSRet: Currency read FvISSRet write FvISSRet;
    property indISS: TpcnindISS read FindISS write FindISS;
    property cServico: String read FcServico write FcServico;
    property cMun: Integer read FcMun write FcMun;
    property cPais: Integer read FcPais write FcPais;
    property nProcesso: String read FnProcesso write FnProcesso;
    property indIncentivo: TpcnindIncentivo read FindIncentivo write FindIncentivo;
  end;

  TTransp = class(TPersistent)
  private
    FmodFrete: TpcnModalidadeFrete;
    FTransporta: TTransporta;
    FretTransp: TretTransp;
    FveicTransp: TveicTransp;
    FVol: TVolCollection;
    FReboque: TReboqueCollection;
    Fvagao: String;
    Fbalsa: String;

    procedure SetVol(Value: TVolCollection);
    procedure SetReboque(Value: TReboqueCollection);
  public
    constructor Create(AOwner: TNFe);
    destructor Destroy; override;
  published
    property modFrete: TpcnModalidadeFrete read FmodFrete write FmodFrete;
    property Transporta: TTransporta read FTransporta write FTransporta;
    property retTransp: TretTransp read FretTransp write FretTransp;
    property veicTransp: TveicTransp read FveicTransp write FveicTransp;
    property Vol: TVolCollection read FVol write SetVol;
    property Reboque: TReboqueCollection read FReboque write SetReboque;
    property vagao: String read Fvagao write Fvagao;
    property balsa: String read Fbalsa write Fbalsa;
  end;

  TTransporta = class(TPersistent)
  private
    FCNPJCPF: String;
    FxNome: String;
    FIE: String;
    FxEnder: String;
    FxMun: String;
    FUF: String;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property xNome: String read FxNome write FxNome;
    property IE: String read FIE write FIE;
    property xEnder: String read FxEnder write FxEnder;
    property xMun: String read FxMun write FxMun;
    property UF: String read FUF write FUF;
  end;

  TretTransp = class(TPersistent)
  private
    FvServ: Currency;
    FvBCRet: Currency;
    FpICMSRet: Currency;
    FvICMSRet: Currency;
    FCFOP: String;
    FcMunFG: Integer;
  public
    constructor Create(AOwner: TTransp);
  published
    property vServ: Currency read FvServ write FvServ;
    property vBCRet: Currency read FvBCRet write FvBCRet;
    property pICMSRet: Currency read FpICMSRet write FpICMSRet;
    property vICMSRet: Currency read FvICMSRet write FvICMSRet;
    property CFOP: String read FCFOP write FCFOP;
    property cMunFG: Integer read FcMunFG write FcMunFG;
  end;

  TveicTransp = class(TPersistent)
  private
    Fplaca: String;
    FUF: String;
    FRNTC: String;
  published
    property placa: String read Fplaca write Fplaca;
    property UF: String read FUF write FUF;
    property RNTC: String read FRNTC write FRNTC;
  end;

  TReboqueCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TReboqueCollectionItem;
    procedure SetItem(Index: Integer; Value: TReboqueCollectionItem);
  public
    constructor Create(AOwner: TTransp);
    function Add: TReboqueCollectionItem;
    property Items[Index: Integer]: TReboqueCollectionItem read GetItem write SetItem; default;
  end;

  TReboqueCollectionItem = class(TCollectionItem)
  private
    Fplaca: String;
    FUF: String;
    FRNTC: String;
  published
    property placa: String read Fplaca write Fplaca;
    property UF: String read FUF write FUF;
    property RNTC: String read FRNTC write FRNTC;
  end;

  TVolCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TVolCollectionItem;
    procedure SetItem(Index: Integer; Value: TVolCollectionItem);
  public
    constructor Create(AOwner: TTransp);
    function Add: TVolCollectionItem;
    property Items[Index: Integer]: TVolCollectionItem read GetItem write SetItem; default;
  end;

  TVolCollectionItem = class(TCollectionItem)
  private
    FqVol: Integer;
    Fesp: String;
    Fmarca: String;
    FnVol: String;
    FpesoL: Currency;
    FpesoB: Currency;
    FLacres: TLacresCollection;

    procedure SetLacres(Value: TLacresCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property qVol: Integer read FqVol write FqVol;
    property esp: String read Fesp write Fesp;
    property marca: String read Fmarca write Fmarca;
    property nVol: String read FnVol write FnVol;
    property pesoL: Currency read FpesoL write FpesoL;
    property pesoB: Currency read FpesoB write FpesoB;
    property Lacres: TLacresCollection read FLacres write SetLacres;
  end;

  TLacresCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TLacresCollectionItem;
    procedure SetItem(Index: Integer; Value: TLacresCollectionItem);
  public
    constructor Create(AOwner: TVolCollectionItem);
    function Add: TLacresCollectionItem;
    property Items[Index: Integer]: TLacresCollectionItem read GetItem write SetItem; default;
  end;

  TLacresCollectionItem = class(TCollectionItem)
  private
    FnLacre: String;
  published
    property nLacre: String read FnLacre write FnLacre;
  end;

  TCobr = class(TPersistent)
  private
    FFat: TFat;
    FDup: TDupCollection;

    procedure SetDup(Value: TDupCollection);
  public
    constructor Create(AOwner: TNFe);
    destructor Destroy; override;
  published
    property Fat: TFat read FFat write FFat;
    property Dup: TDupCollection read FDup write SetDup;
  end;

  TFat = class(TPersistent)
  private
    FnFat: String;
    FvOrig: Currency;
    FvDesc: Currency;
    FvLiq: Currency;
  published
    property nFat: String read FnFat write FnFat;
    property vOrig: Currency read FvOrig write FvOrig;
    property vDesc: Currency read FvDesc write FvDesc;
    property vLiq: Currency read FvLiq write FvLiq;
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
    FnDup: String;
    FdVenc: TDateTime;
    FvDup: Currency;
  published
    property nDup: String read FnDup write FnDup;
    property dVenc: TDateTime read FdVenc write FdVenc;
    property vDup: Currency read FvDup write FvDup;
  end;

  TpagCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TpagCollectionItem;
    procedure SetItem(Index: Integer; Value: TpagCollectionItem);
  public
    constructor Create(AOwner: TNFe);
    function Add: TpagCollectionItem;
    property Items[Index: Integer]: TpagCollectionItem read GetItem write SetItem; default;
  end;

  TpagCollectionItem = class(TCollectionItem)
  private
    FtPag: TpcnFormaPagamento;
    FvPag: Currency;
    FCNPJ: String;
    FtBand: TpcnBandeiraCartao;
    FcAut: String;
  published
    property tPag: TpcnFormaPagamento read FtPag write FtPag;
    property vPag: Currency read FvPag write FvPag;
    property CNPJ: String read FCNPJ write FCNPJ;
    property tBand: TpcnBandeiraCartao read FtBand write FtBand;
    property cAut: String read FcAut write FcAut;
  end;

  TInfAdic = class(TPersistent)
  private
    FinfAdFisco: String;
    FinfCpl: String;
    FobsCont: TobsContCollection;
    FobsFisco: TobsFiscoCollection;
    FprocRef: TprocRefCollection;

    procedure SetobsCont(Value: TobsContCollection);
    procedure SetobsFisco(Value: TobsFiscoCollection);
    procedure SetprocRef(Value: TprocRefCollection);
  public
    constructor Create(AOwner: TNFe);
    destructor Destroy; override;
  published
    property infAdFisco: String read FinfAdFisco write FinfAdFisco;
    property infCpl: String read FinfCpl write FinfCpl;
    property obsCont: TobsContCollection read FobsCont write SetobsCont;
    property obsFisco: TobsFiscoCollection read FobsFisco write SetobsFisco;
    property procRef: TprocRefCollection read FprocRef write SetprocRef;
  end;

  TobsContCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TobsContCollectionItem;
    procedure SetItem(Index: Integer; Value: TobsContCollectionItem);
  public
    constructor Create(AOwner: TinfAdic);
    function Add: TobsContCollectionItem;
    property Items[Index: Integer]: TobsContCollectionItem read GetItem write SetItem; default;
  end;

  TobsContCollectionItem = class(TCollectionItem)
  private
    FxCampo: String;
    FxTexto: String;
  published
    property xCampo: String read FxCampo write FxCampo;
    property xTexto: String read FxTexto write FxTexto;
  end;

  TobsFiscoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TobsFiscoCollectionItem;
    procedure SetItem(Index: Integer; Value: TobsFiscoCollectionItem);
  public
    constructor Create(AOwner: TinfAdic);
    function Add: TobsFiscoCollectionItem;
    property Items[Index: Integer]: TobsFiscoCollectionItem read GetItem write SetItem; default;
  end;

  TobsFiscoCollectionItem = class(TCollectionItem)
  private
    FxCampo: String;
    FxTexto: String;
  published
    property xCampo: String read FxCampo write FxCampo;
    property xTexto: String read FxTexto write FxTexto;
  end;

  TprocRefCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TprocRefCollectionItem;
    procedure SetItem(Index: Integer; Value: TprocRefCollectionItem);
  public
    constructor Create(AOwner: TinfAdic);
    function Add: TprocRefCollectionItem;
    property Items[Index: Integer]: TprocRefCollectionItem read GetItem write SetItem; default;
  end;

  TprocRefCollectionItem = class(TCollectionItem)
  private
    FnProc: String;
    FindProc: TpcnIndicadorProcesso;
  published
    property nProc: String read FnProc write FnProc;
    property indProc: TpcnIndicadorProcesso read FindProc write FindProc default ipSEFAZ;
  end;

  TExporta = class(TPersistent)
  private
    FUFembarq: String;
    FxLocEmbarq: String;
    // Versao 3.10
    FUFSaidaPais: String;
    FxLocExporta: String;
    FxLocDespacho: String;
  published
    property UFembarq: String read FUFembarq write FUFembarq;
    property xLocEmbarq: String read FxLocEmbarq write FxLocEmbarq;
    // Versao 3.10
    property UFSaidaPais: String read FUFSaidaPais write FUFSaidaPais;
    property xLocExporta: String read FxLocExporta write FxLocExporta;
    property xLocDespacho: String read FxLocDespacho write FxLocDespacho;
  end;

  TCompra = class(TPersistent)
  private
    FxNEmp: String;
    FxPed: String;
    FxCont: String;
  published
    property xNEmp: String read FxNEmp write FxNEmp;
    property xPed: String read FxPed write FxPed;
    property xCont: String read FxCont write FxCont;
  end;

  TCana = class(TPersistent)
  private
    Fsafra: String;
    Fref: String;
    Ffordia : TForDiaCollection;
    FqTotMes: Double;
    FqTotAnt: Double;
    FqTotGer: Double;
    Fdeduc : TDeducCollection;
    FvFor: Currency;
    FvTotDed: Currency;
    FvLiqFor: Currency;

    procedure SetDeduc(const Value: TDeducCollection);
    procedure SetForDia(const Value: TForDiaCollection);
  public
    constructor Create(AOwner: TNFe);
    destructor Destroy; override;
  published
    property safra: String read Fsafra write Fsafra;
    property ref: String read Fref write Fref;
    property fordia: TForDiaCollection read Ffordia write SetForDia;
    property qTotMes: Double read FqTotMes write FqTotMes;
    property qTotAnt: Double read FqTotAnt write FqTotAnt;
    property qTotGer: Double read FqTotGer write FqTotGer;
    property deduc: TDeducCollection read Fdeduc write SetDeduc;
    property vFor: Currency read FvFor write FvFor;
    property vTotDed: Currency read FvTotDed write FvTotDed;
    property vLiqFor: Currency read FvLiqFor write FvLiqFor;
  end;

  TForDiaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TForDiaCollectionItem;
    procedure SetItem(Index: Integer; Value: TForDiaCollectionItem);
  public
    constructor Create(AOwner: TCana);
    function Add: TForDiaCollectionItem;
    property Items[Index: Integer]: TForDiaCollectionItem read GetItem write SetItem; default;
  end;

  TForDiaCollectionItem = class(TCollectionItem)
  private
    Fdia: Integer;
    Fqtde: Currency;
  published
    property dia: Integer read Fdia write Fdia;
    property qtde: Currency read Fqtde write Fqtde;
  end;

  TDeducCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDeducCollectionItem;
    procedure SetItem(Index: Integer; Value: TDeducCollectionItem);
  public
    constructor Create(AOwner: TCana);
    function Add: TDeducCollectionItem;
    property Items[Index: Integer]: TDeducCollectionItem read GetItem write SetItem; default;
  end;

  TDeducCollectionItem = class(TCollectionItem)
  private
    FxDed: String;
    FvDed: Currency;
  published
    property xDed: String read FxDed write FxDed;
    property vDed: Currency read FvDed write FvDed;
  end;

  TautXMLCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TautXMLCollectionItem;
    procedure SetItem(Index: Integer; Value: TautXMLCollectionItem);
  public
    constructor Create(AOwner: TNFe);
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

  CMUN_EXTERIOR: Integer = 9999999;
  XMUN_EXTERIOR: String = 'EXTERIOR';
  UF_EXTERIOR: String = 'EX';

implementation

{ TNFe }

constructor TNFe.Create;
begin
  FinfNFe  := TinfNFe.Create;
  FIde     := TIde.Create(Self);
  FEmit    := TEmit.Create(Self);
  FAvulsa  := TAvulsa.Create;
  FDest    := TDest.Create(Self);
  FRetirada := TRetirada.Create;
  FEntrega := TEntrega.Create;
  FautXML  := TautXMLCollection.Create(Self);
  FDet     := TDetCollection.Create(Self);
  FTotal   := TTotal.Create(self);
  FCobr    := TCobr.Create(Self);
  Fpag     := TpagCollection.Create(Self);
  FTransp  := TTransp.Create(Self);
  FinfAdic := TinfAdic.Create(self);
  FExporta := TExporta.Create;
  FCompra  := TCompra.Create;
  FCana    := TCana.Create(Self);
  Fsignature := Tsignature.create;
  FProcNFe := TProcNFe.create;

  FinfNFe.Versao := 0;

  FEmit.EnderEmit.xPais := 'BRASIL';
  FEmit.EnderEmit.cPais := 1058;
  FEmit.EnderEmit.nro   := 'SEM NUMERO';

  FDest.EnderDest.xPais := 'BRASIL';
  FDest.EnderDest.cPais := 1058;
  FDest.EnderDest.nro   := 'SEM NUMERO';
end;

destructor TNFe.Destroy;
begin
  FinfNFe.Free;
  FIde.Free;
  FEmit.Free;
  FAvulsa.Free;
  FDest.Free;
  FRetirada.Free;
  FEntrega.Free;
  FautXML.Free;
  FDet.Free;
  FTotal.Free;
  FCobr.Free;
  Fpag.Free;
  FTransp.Free;
  FinfAdic.Free;
  FExporta.Free;
  FCompra.Free;
  FCana.Free;
  Fsignature.Free;
  FProcNFe.Free;
  inherited Destroy;
end;

procedure TNFe.SetautXML(const Value: TautXMLCollection);
begin
  FautXML := Value;
end;

procedure TNFe.SetDet(Value: TDetCollection);
begin
  FDet.Assign(Value);
end;

procedure TNFe.Setpag(Value: TpagCollection);
begin
  Fpag.Assign(Value);
end;

{ TDetCollection }

constructor TDetCollection.Create(AOwner: TNFe);
begin
  inherited Create(TDetCollectionItem);
end;

function TDetCollection.Add: TDetCollectionItem;
begin
  Result := TDetCollectionItem(inherited Add);
  Result.create;
end;

function TDetCollection.GetItem(Index: Integer): TDetCollectionItem;
begin
  Result := TDetCollectionItem(inherited GetItem(Index));
end;

procedure TDetCollection.SetItem(Index: Integer; Value: TDetCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDetCollectionItem }

constructor TDetCollectionItem.Create;
begin
  FProd := TProd.Create(self);
  FImposto := TImposto.Create(self);
end;

{Ide}

constructor TIde.Create(AOwner: TNFe);
begin
  inherited Create;
  FNFref := TNFrefCollection.Create(Self);
end;

destructor TIde.Destroy;
begin
  FNFref.Free;
  inherited;
end;

procedure TIde.SetNFref(Value: TNFrefCollection);
begin
  FNFref.Assign(Value);
end;

{NFrefCollection}

constructor TNFrefCollection.Create(AOwner: TIde);
begin
  inherited Create(TNFrefCollectionItem);
end;

function TNFrefCollection.Add: TNFrefCollectionItem;
begin
  Result := TNFrefCollectionItem(inherited Add);
  Result.create;
end;

function TNFrefCollection.GetItem(Index: Integer): TNFrefCollectionItem;
begin
  Result := TNFrefCollectionItem(inherited GetItem(Index));
end;

procedure TNFrefCollection.SetItem(Index: Integer; Value: TNFrefCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

destructor TDetCollectionItem.Destroy;
begin
  FProd.Free;
  FImposto.Free;
  inherited;
end;

{ TNFrefCollectionItem }

constructor TNFrefCollectionItem.Create;
begin
  FRefNF := TRefNF.Create;
  FRefNFP := TRefNFP.Create;
  FRefECF := TRefECF.Create;
end;


destructor TNFrefCollectionItem.destroy;
begin
  FreeAndNil(FRefNF);
  FreeAndNil(FRefNFP);
  FreeAndNil(FRefECF);
  inherited;
end;

{Emit}

constructor TEmit.Create(AOwner: TNFe);
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

{Dest}

constructor TDest.Create(AOwner: TNFe);
begin
  inherited Create;
  FEnderDest := TEnderDest.Create;
end;

destructor TDest.Destroy;
begin
  FEnderDest.Free;
  inherited;
end;

{Prod}

constructor TProd.Create(AOwner: TDetcollectionItem);
begin
  inherited Create;
  FDI := TDICollection.Create(Self);
  FNVE := TNVECollection.Create(self);
  FdetExport := TdetExportCollection.Create(Self);
  FveicProd := TveicProd.Create;
  FMed := TMedCollection.Create(Self);
  Farma := TArmaCollection.Create(Self);
  Fcomb := TComb.Create(Self);
end;

destructor TProd.Destroy;
begin
  FDI.Free;
  FNVE.Free;
  FdetExport.Free;
  FveicProd.Free;
  FMed.Free;
  FArma.Free;
  Fcomb.Free;
  inherited;
end;

procedure TProd.getCFOP(const Value: String);
begin
  FCFOP := Value;
end;

procedure TProd.SetDI(Value: TDICollection);
begin
  FDI.Assign(Value);
end;

procedure TProd.SetdetExport(const Value: TdetExportCollection);
begin
  FdetExport := Value;
end;

procedure TProd.SetMed(Value: TMedCollection);
begin
  FMed.Assign(Value);
end;

procedure TProd.SetNVE(Value: TNVeCollection);
begin
  FNVE.Assign(Value);
end;

procedure TProd.SetArma(Value: TArmaCollection);
begin
  FArma.Assign(Value);
end;

{MedCollection}

constructor TMedCollection.Create(AOwner: TProd);
begin
  inherited Create(TMedCollectionItem);
end;

function TMedCollection.Add: TMedCollectionItem;
begin
  Result := TMedCollectionItem(inherited Add);
end;

function TMedCollection.GetItem(Index: Integer): TMedCollectionItem;
begin
  Result := TMedCollectionItem(inherited GetItem(Index));
end;

procedure TMedCollection.SetItem(Index: Integer; Value: TMedCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ArmaCollection}

constructor TArmaCollection.Create(AOwner: TProd);
begin
  inherited Create(TArmaCollectionItem);
end;

function TArmaCollection.Add: TArmaCollectionItem;
begin
  Result := TArmaCollectionItem(inherited Add);
end;

function TArmaCollection.GetItem(Index: Integer): TArmaCollectionItem;
begin
  Result := TArmaCollectionItem(inherited GetItem(Index));
end;

procedure TArmaCollection.SetItem(Index: Integer; Value: TArmaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{comb}

constructor Tcomb.Create(AOwner: TProd);
begin
  inherited Create;
  FCIDE := TCIDE.Create;
  FICMS := TICMSComb.Create;
  FICMSInter := TICMSInter.Create;
  FICMScons := TICMScons.Create;
end;

destructor Tcomb.Destroy;
begin
  FCIDE.Free;
  FICMS.Free;
  FICMSInter.Free;
  FICMScons.Free;
  inherited;
end;

{DICollection}

constructor TDICollection.Create(AOwner: TProd);
begin
  inherited Create(TDICollectionItem);
end;

function TDICollection.Add: TDICollectionItem;
begin
  Result := TDICollectionItem(inherited Add);
  Result.create;
end;

function TDICollection.GetItem(Index: Integer): TDICollectionItem;
begin
  Result := TDICollectionItem(inherited GetItem(Index));
end;

procedure TDICollection.SetItem(Index: Integer; Value: TDICollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{DICollectionItem}

constructor TDICollectionItem.Create;
begin
  FAdi := TadiCollection.Create(self);
end;

destructor TDICollectionItem.Destroy;
begin
  FAdi.Free;
  inherited;
end;

procedure TDICollectionItem.SetAdi(Value: TAdiCollection);
begin
  FAdi.Assign(Value);
end;

{AdiCollection}

constructor TAdiCollection.Create(AOwner: TDiCollectionItem);
begin
  inherited Create(TAdiCollectionItem);
end;

function TAdiCollection.Add: TAdiCollectionItem;
begin
  Result := TAdiCollectionItem(inherited Add);
end;

function TAdiCollection.GetItem(Index: Integer): TAdiCollectionItem;
begin
  Result := TAdiCollectionItem(inherited GetItem(Index));
end;

procedure TAdiCollection.SetItem(Index: Integer; Value: TAdiCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{Imposto}

constructor TImposto.Create(AOwner: TDetcollectionItem);
begin
  inherited Create;
  FICMS := TICMS.Create;
  FIPI := TIPI.Create;
  FII := TII.Create;
  FPIS := TPIS.Create;
  FPISST := TPISST.Create;
  FCOFINS := TCOFINS.Create;
  FCOFINSST := TCOFINSST.Create;
  FISSQN := TISSQN.create;
end;

destructor TImposto.Destroy;
begin
  FICMS.Free;
  FIPI.Free;
  FII.Free;
  FPIS.Free;
  FPISST.Free;
  FCOFINS.Free;
  FCOFINSST.Free;
  FISSQN.Free;
  inherited;
end;

{Total}

constructor TTotal.Create(AOwner: TNFe);
begin
  inherited Create;
  FICMSTot := TICMSTot.Create;
  FISSQNtot := TISSQNtot.create;
  FretTrib := TretTrib.create;
end;

destructor TTotal.Destroy;
begin
  FICMSTot.Free;
  FISSQNtot.Free;
  FretTrib.Free;
  inherited;
end;

{Transp}

constructor TTransp.Create(AOwner: TNFe);
begin
  inherited Create;
  FTransporta := TTransporta.Create;
  FretTransp := TretTransp.Create(self);
  FveicTransp := TveicTransp.Create;
  FVol := TVolCollection.Create(self);
  Freboque := TreboqueCollection.Create(self);
end;

destructor TTransp.Destroy;
begin
  FTransporta.Free;
  FretTransp.Free;
  FveicTransp.Free;
  FVol.Free;
  Freboque.Free;
  inherited;
end;

procedure TTransp.SetVol(Value: TVolCollection);
begin
  FVol.Assign(Value);
end;

procedure TTransp.SetReboque(Value: TReboqueCollection);
begin
  FReboque.Assign(Value);
end;

{VolCollection}

constructor TVolCollection.Create(AOwner: TTransp);
begin
  inherited Create(TVolCollectionItem);
end;

function TVolCollection.Add: TVolCollectionItem;
begin
  Result := TVolCollectionItem(inherited Add);
  Result.create;
end;

function TVolCollection.GetItem(Index: Integer): TVolCollectionItem;
begin
  Result := TVolCollectionItem(inherited GetItem(Index));
end;

procedure TVolCollection.SetItem(Index: Integer; Value: TVolCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{VolCollectionItem}

constructor TVolCollectionItem.Create;
begin
  FLacres := TLacresCollection.Create(self);
end;

destructor TVolCollectionItem.Destroy;
begin
  FLacres.Free;
  inherited;
end;

procedure TVolCollectionItem.SetLacres(Value: TLacresCollection);
begin
  FLacres.Assign(Value);
end;

{LacresCollection}

constructor TLacresCollection.Create(AOwner: TVolCollectionItem);
begin
  inherited Create(TLacresCollectionItem);
end;

function TLacresCollection.Add: TLacresCollectionItem;
begin
  Result := TLacresCollectionItem(inherited Add);
end;

function TLacresCollection.GetItem(Index: Integer): TLacresCollectionItem;
begin
  Result := TLacresCollectionItem(inherited GetItem(Index));
end;

procedure TLacresCollection.SetItem(Index: Integer; Value: TLacresCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{retTransp}

constructor TretTransp.Create(AOwner: TTransp);
begin
  inherited Create;
end;

{ReboqueCollection}

constructor TReboqueCollection.Create(AOwner: TTransp);
begin
  inherited Create(TReboqueCollectionItem);
end;

function TreboqueCollection.Add: TreboqueCollectionItem;
begin
  Result := TreboqueCollectionItem(inherited Add);
end;

function TreboqueCollection.GetItem(Index: Integer): TreboqueCollectionItem;
begin
  Result := TreboqueCollectionItem(inherited GetItem(Index));
end;

procedure TreboqueCollection.SetItem(Index: Integer; Value: TreboqueCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{Cobr}

constructor TCobr.Create(AOwner: TNFe);
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

{infAdic}

constructor TinfAdic.Create(AOwner: TNFe);
begin
  inherited Create;
  FobsCont := TobsContCollection.Create(Self);
  FobsFisco := TobsFiscoCollection.Create(Self);
  FprocRef := TprocRefCollection.Create(Self);
end;

destructor TinfAdic.Destroy;
begin
  FobsCont.Free;
  FobsFisco.Free;
  FprocRef.Free;
  inherited;
end;

procedure TinfAdic.SetobsCont(Value: TobsContCollection);
begin
  FobsCont.Assign(Value);
end;

procedure TinfAdic.SetobsFisco(Value: TobsFiscoCollection);
begin
  FobsFisco.Assign(Value);
end;

procedure TinfAdic.SetprocRef(Value: TprocRefCollection);
begin
  FprocRef.Assign(Value);
end;

{obsContCollection}

constructor TobsContCollection.Create(AOwner: TinfAdic);
begin
  inherited Create(TobsContCollectionItem);
end;

function TobsContCollection.Add: TobsContCollectionItem;
begin
  Result := TobsContCollectionItem(inherited Add);
end;

function TobsContCollection.GetItem(Index: Integer): TobsContCollectionItem;
begin
  Result := TobsContCollectionItem(inherited GetItem(Index));
end;

procedure TobsContCollection.SetItem(Index: Integer; Value: TobsContCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{obsFiscoCollection}

constructor TobsFiscoCollection.Create(AOwner: TinfAdic);
begin
  inherited Create(TobsFiscoCollectionItem);
end;

function TobsFiscoCollection.Add: TobsFiscoCollectionItem;
begin
  Result := TobsFiscoCollectionItem(inherited Add);
end;

function TobsFiscoCollection.GetItem(Index: Integer): TobsFiscoCollectionItem;
begin
  Result := TobsFiscoCollectionItem(inherited GetItem(Index));
end;

procedure TobsFiscoCollection.SetItem(Index: Integer; Value: TobsFiscoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{procRefCollection}

constructor TprocRefCollection.Create(AOwner: TinfAdic);
begin
  inherited Create(TprocRefCollectionItem);
end;

function TprocRefCollection.Add: TprocRefCollectionItem;
begin
  Result := TprocRefCollectionItem(inherited Add);
end;

function TprocRefCollection.GetItem(Index: Integer): TprocRefCollectionItem;
begin
  Result := TprocRefCollectionItem(inherited GetItem(Index));
end;

procedure TprocRefCollection.SetItem(Index: Integer; Value: TprocRefCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

destructor TMedCollection.Destroy;
begin
  inherited;
end;

destructor TArmaCollection.Destroy;
begin
  inherited Destroy;
end;

destructor TDupCollection.Destroy;
begin
  inherited;
end;

{ TCana }

constructor TCana.Create(AOwner: TNFe);
begin
  inherited Create;
  Ffordia := TForDiaCollection.Create(Self);
  Fdeduc := TDeducCollection.Create(Self);
end;

destructor TCana.Destroy;
begin
  Ffordia.Free;
  Fdeduc.Free;
  inherited;
end;

procedure TCana.SetDeduc(const Value: TDeducCollection);
begin
  Fdeduc.Assign(Value);
end;

procedure TCana.SetForDia(const Value: TForDiaCollection);
begin
  Ffordia.Assign(Value);
end;

{ TForDiaCollection }

function TForDiaCollection.Add: TForDiaCollectionItem;
begin
  Result := TForDiaCollectionItem(inherited Add);
end;

constructor TForDiaCollection.Create(AOwner: TCana);
begin
  inherited Create(TForDiaCollectionItem);
end;

function TForDiaCollection.GetItem(Index: Integer): TForDiaCollectionItem;
begin
  Result := TForDiaCollectionItem(inherited GetItem(Index));
end;

procedure TForDiaCollection.SetItem(Index: Integer;
  Value: TForDiaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDeducCollection }

function TDeducCollection.Add: TDeducCollectionItem;
begin
  Result := TDeducCollectionItem(inherited Add);
end;

constructor TDeducCollection.Create(AOwner: TCana);
begin
  inherited Create(TDeducCollectionItem);
end;

function TDeducCollection.GetItem(Index: Integer): TDeducCollectionItem;
begin
  Result := TDeducCollectionItem(inherited GetItem(Index));
end;

procedure TDeducCollection.SetItem(Index: Integer;
  Value: TDeducCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfNFe }

function TinfNFe.GetVersao: Real;
begin
  if FVersao <= 0 then
     Result := 2
  else
     Result := FVersao;
end;

function TinfNFe.GetVersaoStr: String;
   function FormatFloat_Aux(AValue: Extended;
                            AFormat: String): String;
   {$IFDEF VER140} //delphi6
   {$ELSE}
   var
      vFormato: TFormatSettings;
   {$ENDIF}
   begin
      {$IFDEF VER140} //delphi6
      DecimalSeparator  := '.';
      ThousandSeparator := ',';
      Result := SysUtils.FormatFloat(AFormat, AValue);
      DecimalSeparator  := ',';
      ThousandSeparator := '.';
      {$ELSE}
      vFormato.DecimalSeparator  := '.';
      vFormato.ThousandSeparator := ',';
      Result := SysUtils.FormatFloat(AFormat, AValue, vFormato);
      {$ENDIF}
   end;
begin
  if FVersao <= 0 then
     Result := V2_00
  else
     Result := 'versao="'+FormatFloat_Aux(FVersao,'#0.00')+'"';
end;

{ TveicProd }

function TveicProd.getCombDescricao: String;
var
  wTpComb: Integer;
begin
  wTpComb := StrToIntDef(FtpComb,0);

  case wTpComb of
     1: Result:='01 - ALCOOL';
     2: Result:='02 - GASOLINA';
     3: Result:='03 - DIESEL';
     4: Result:='04 - GASOGENIO';
     5: Result:='05 - GAS METANO';
     6: Result:='06 - ELETRICO/FONTE INTERNA';
     7: Result:='07 - ELETRICO/FONTE EXTERNA';
     8: Result:='08 - GASOL/GAS NATURAL COMBUSTIVEL';
     9: Result:='09 - ALCOOL/GAS NATURAL COMBUSTIVEL';
    10: Result:='10 - DIESEL/GAS NATURAL COMBUSTIVEL';
    11: Result:='11 - VIDE/CAMPO/OBSERVACAO';
    12: Result:='12 - ALCOOL/GAS NATURAL VEICULAR';
    13: Result:='13 - GASOLINA/GAS NATURAL VEICULAR';
    14: Result:='14 - DIESEL/GAS NATURAL VEICULAR';
    15: Result:='15 - GAS NATURAL VEICULAR';
    16: Result:='16 - ALCOOL/GASOLINA';
    17: Result:='17 - GASOLINA/ALCOOL/GAS NATURAL';
    18: Result:='18 - GASOLINA/ELETRICO';
    else
      Result:=FtpComb;
  end;
end;

{ TpagCollection }

function TpagCollection.Add: TpagCollectionItem;
begin
  Result := TpagCollectionItem(inherited Add);
end;

constructor TpagCollection.Create(AOwner: TNFe);
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

{ TautXMLCollection }

function TautXMLCollection.Add: TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem(inherited Add);
  Result.create;
end;

constructor TautXMLCollection.Create(AOwner: TNFe);
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

{ TdetExportCollection }

function TdetExportCollection.Add: TdetExportCollectionItem;
begin
  Result := TdetExportCollectionItem(inherited Add);
end;

constructor TdetExportCollection.Create(AOwner: TProd);
begin
  inherited Create(TdetExportCollectionItem);
end;

function TdetExportCollection.GetItem(
  Index: Integer): TdetExportCollectionItem;
begin
  Result := TdetExportCollectionItem(inherited GetItem(Index));
end;

procedure TdetExportCollection.SetItem(Index: Integer;
  Value: TdetExportCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TNVECollection }

function TNVECollection.Add: TNVECollectionItem;
begin
  Result := TNVECollectionItem(inherited Add);
end;

constructor TNVECollection.Create(AOwner: TProd);
begin
  inherited Create(TNVECollectionItem);
end;

function TNVECollection.GetItem(Index: Integer): TNVECollectionItem;
begin
  Result := TNVECollectionItem(inherited GetItem(Index));
end;

procedure TNVECollection.SetItem(Index: Integer; Value: TNVECollectionItem);
begin
  inherited SetItem(Index, Value);
end;

end.

