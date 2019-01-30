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
  TrastroCollection = class;
  TrastroCollectionItem = class;
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
  TICMSUFDest = class;
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

  TinfNFeSupl = class;

  TinfRespTec = class;

  { TNFe }

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
    FinfNFeSupl: TinfNFeSupl;
    FinfRespTec: TinfRespTec;

    procedure SetDet(Value: TDetCollection);
    procedure Setpag(Value: TpagCollection);
    procedure SetautXML(const Value: TautXMLCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure SetXMLString(const AValue : AnsiString);
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
    property infNFeSupl: TinfNFeSupl read FinfNFeSupl write FinfNFeSupl;
    property signature: Tsignature read Fsignature write Fsignature;
    property procNFe: TProcNFe read FProcNFe write FProcNFe;
    property infRespTec: TinfRespTec read FinfRespTec write FinfRespTec;
  end;

  TinfNFeSupl = class(TPersistent)
  private
    FqrCode: String;
    FurlChave: String;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property qrCode: String read FqrCode write FqrCode;
    property urlChave: String read FurlChave write FurlChave;
  end;

  TinfNFe = class(TPersistent)
  private
    FID: String;
    FVersao: Real;
    function GetVersaoStr: String;
    function GetVersao: Real;
  public
    procedure Assign(Source: TPersistent); override;
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

    procedure Assign(Source: TPersistent); override;
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
    constructor Create(AOwner: TIde); 
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
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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

    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
  published
    property xLgr: String read FxLgr write FxLgr;
    property nro: String read Fnro write Fnro;
    property xCpl: String read FxCpl write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: String read FxMun write FxMun;
    property UF: String read FUF write FUF;
    property CEP: Integer read FCEP write FCEP;
    property cPais: Integer read FcPais write FcPais;
    property xPais: String read FxPais write FxPais;
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
  public
    procedure Assign(Source: TPersistent); override;
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

    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
  published
    property xLgr: String read FxLgr write FxLgr;
    property nro: String read Fnro write Fnro;
    property xCpl: String read FxCpl write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: String read FxMun write FxMun;
    property UF: String read FUF write FUF;
    property CEP: Integer read FCEP write FCEP;
    property cPais: Integer read FcPais write FcPais;
    property xPais: String read FxPais write FxPais;
    property fone: String read Ffone write Ffone;
  end;

  TRetirada = class(TPersistent)
  private
    FCNPJCPF: String;
    FxNome: String;
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
    Femail: String;
    FIE: String;
  public
    procedure Assign(Source: TPersistent); override;
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
    property CEP: Integer read FCEP write FCEP;
    property cPais: Integer read FcPais write FcPais;
    property xPais: String read FxPais write FxPais;
    property fone: String read Ffone write Ffone;
    property Email: String read Femail write Femail;
    property IE: String read FIE write FIE;
  end;

  TEntrega = class(TPersistent)
  private
    FCNPJCPF: String;
    FxNome: String;
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
    Femail: String;
    FIE: String;
  public
    procedure Assign(Source: TPersistent); override;
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
    property CEP: Integer read FCEP write FCEP;
    property cPais: Integer read FcPais write FcPais;
    property xPais: String read FxPais write FxPais;
    property fone: String read Ffone write Ffone;
    property Email: String read Femail write Femail;
    property IE: String read FIE write FIE;
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
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Prod: TProd read FProd write FProd;
    property Imposto: TImposto read FImposto write FImposto;
    property pDevol: Currency read FpDevol write FpDevol;
    property vIPIDevol: Currency read FvIPIDevol write FvIPIDevol;
    property infAdProd: String read FinfAdProd write FinfAdProd;
  end;

  { TProd }

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
    FnItemPed: String;
    FdetExport: TdetExportCollection;
    FRastro: TrastroCollection;
    FveicProd: TveicProd;
    Fmed: TMedCollection;
    Farma: TarmaCollection;
    Fcomb: Tcomb;
    FnRECOPI: String;
    FnFCI: String;
    FNVE: TNVECollection;
    FCEST: String;
    FindEscala: TpcnIndEscala;
    FCNPJFab: String;
    FcBenef: String;

    procedure SetDI(Value: TDICollection);
    procedure SetRastro(Value: TrastroCollection);
    procedure SetMed(Value: TmedCollection);
    procedure SetArma(Value: TarmaCollection);
    procedure SetdetExport(const Value: TdetExportCollection);
    procedure SetNVE(Value : TNVeCollection);
    procedure getCFOP(const Value: String);
  public
    constructor Create(AOwner: TDetcollectionItem);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property cProd: String read FcProd write FcProd;
    property nItem: Integer read FnItem write FnItem;
    property cEAN: String read FcEAN write FcEAN;
    property xProd: String read FxProd write FxProd;
    property NCM: String read FNCM write FNCM;
    property NVE : TNVECollection read FNVE write SetNVE; //FNVE;
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
    property nItemPed : String read FnItemPed write FnItemPed;
    property detExport: TdetExportCollection read FdetExport write SetdetExport;
    property rastro: TrastroCollection read FRastro write SetRastro;
    property veicProd: TveicProd read FveicProd write FveicProd;
    property med: TMedCollection read Fmed write SetMed;
    property arma: TarmaCollection read Farma write SetArma;
    property comb: Tcomb read Fcomb write Fcomb;
    property nRECOPI: String read FnRECOPI write FnRECOPI;
    property nFCI: String read FnFCI write FnFCI;
    property CEST: String read FCEST write FCEST;
    property indEscala: TpcnIndEscala read FindEscala write FindEscala default ieNenhum;
    property CNPJFab: String read FCNPJFab write FCNPJFab;
    property cBenef: String read FcBenef write FcBenef;
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
  public
    procedure Assign(Source: TPersistent); override;
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

  TrastroCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TrastroCollectionItem;
    procedure SetItem(Index: Integer; Value: TrastroCollectionItem);
  public
    constructor Create(AOwner: TProd);
    destructor Destroy; override;
    function Add: TrastroCollectionItem;
    property Items[Index: Integer]: TrastroCollectionItem read GetItem write SetItem; default;
  end;

  TrastroCollectionItem = class(TCollectionItem)
  private
    FnLote: String;
    FqLote: Currency;
    FdFab: TDateTime;
    FdVal: TDateTime;
    FcAgreg: String;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property nLote: String read FnLote write FnLote;
    property qLote: Currency read FqLote write FqLote;
    property dFab: TDateTime read FdFab write FdFab;
    property dVal: TDateTime read FdVal write FdVal;
    property cAgreg: String read FcAgreg write FcAgreg;
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
    FcProdANVISA: String;
    FxMotivoIsencao: String;
    FnLote: String;
    FqLote: Currency;
    FdFab: TDateTime;
    FdVal: TDateTime;
    FvPMC: Currency;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property cProdANVISA: String read FcProdANVISA write FcProdANVISA;
    property xMotivoIsencao: String read FxMotivoIsencao write FxMotivoIsencao;
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
  public
    procedure Assign(Source: TPersistent); override;
  published
    property tpArma: TpcnTipoArma read FtpArma write FtpArma default taUsoPermitido;
    property nSerie: String read FnSerie write FnSerie;
    property nCano: String read FnCano write FnCano;
    property descr: String read Fdescr write Fdescr;
  end;

  Tencerrante = class(TPersistent)
  private
    FnBico: Integer;
    FnBomba: Integer;
    FnTanque: Integer;
    FvEncIni: Currency;
    FvEncFin: Currency;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property nBico: Integer read FnBico write FnBico;
    property nBomba: Integer read FnBomba write FnBomba;
    property nTanque: Integer read FnTanque write FnTanque;
    property vEncIni: Currency read FvEncIni write FvEncIni;
    property vEncFin: Currency read FvEncFin write FvEncFin;
  end;

  Tcomb = class(TPersistent)
  private
    FcProdANP: Integer;
    FpMixGN: Currency;
    FdescANP: String;
    FpGLP: Currency;
    FpGNn: Currency;
    FpGNi: Currency;
    FvPart: Currency;
    FCODIF: String;
    FqTemp: Currency;
    FUFcons: String;
    FCIDE: TCIDE;
    FICMS: TICMSComb;
    FICMSInter: TICMSInter;
    FICMSCons: TICMSCons;
    Fencerrante: Tencerrante;
  public
    constructor Create(AOwner: TProd);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property cProdANP: Integer read FcProdANP write FcProdANP;
    property pMixGN: Currency read FpMixGN write FpMixGN;
    property descANP: String read FdescANP write FdescANP;
    property pGLP: Currency read FpGLP write FpGLP;
    property pGNn: Currency read FpGNn write FpGNn;
    property pGNi: Currency read FpGNi write FpGNi;
    property vPart: Currency read FvPart write FvPart;
    property CODIF: String read FCODIF write FCODIF;
    property qTemp: Currency read FqTemp write FqTemp;
    property UFcons: String read FUFcons write FUFcons;
    property CIDE: TCIDE read FCIDE write FCIDE;
    property ICMS: TICMSComb read FICMS write FICMS;
    property ICMSInter: TICMSInter read FICMSInter write FICMSInter;
    property ICMSCons: TICMSCons read FICMSCons write FICMSCons;
    property encerrante: Tencerrante read Fencerrante write Fencerrante;
  end;

  TCIDE = class(TPersistent)
  private
    FqBCProd: Currency;
    FvAliqProd: Currency;
    FvCIDE: Currency;
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
  published
    property vBCICMSSTDest: Currency read FvBCICMSSTDest write FvBCICMSSTDest;
    property vICMSSTDest: Currency read FvICMSSTDest write FvICMSSTDest;
  end;

  TICMSCons = class(TPersistent)
  private
    FvBCICMSSTCons: Currency;
    FvICMSSTCons: Currency;
    FUFcons: String;
  public
    procedure Assign(Source: TPersistent); override;
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
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
    FICMSUFDest: TICMSUFDest;
  public
    constructor Create(AOwner: TDetcollectionItem);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
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
    property ICMSUFDest: TICMSUFDest read FICMSUFDest write FICMSUFDest;
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
    FvBCFCP: Currency;
    FpFCP: Currency;
    FvFCP: Currency;
    FvBCFCPST: Currency;
    FpFCPST: Currency;
    FvFCPST: Currency;
    FvBCFCPSTRet: Currency;
    FpFCPSTRet: Currency;
    FvFCPSTRet: Currency;
    FpST: Currency;
    FpRedBCEfet: Currency;
    FvBCEfet: Currency;
    FpICMSEfet: Currency;
    FvICMSEfet: Currency;
  public
    procedure Assign(Source: TPersistent); override;
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
    property vBCFCP: Currency read FvBCFCP write FvBCFCP;
    property pFCP: Currency read FpFCP write FpFCP;
    property vFCP: Currency read FvFCP write FvFCP;
    property vBCFCPST: Currency read FvBCFCPST write FvBCFCPST;
    property pFCPST: Currency read FpFCPST write FpFCPST;
    property vFCPST: Currency read FvFCPST write FvFCPST;
    property vBCFCPSTRet: Currency read FvBCFCPSTRet write FvBCFCPSTRet;
    property pFCPSTRet: Currency read FpFCPSTRet write FpFCPSTRet;
    property vFCPSTRet: Currency read FvFCPSTRet write FvFCPSTRet;
    property pST: Currency read FpST write FpST;
    property pRedBCEfet: Currency read FpRedBCEfet write FpRedBCEfet;
    property vBCEfet: Currency read FvBCEfet write FvBCEfet;
    property pICMSEfet: Currency read FpICMSEfet write FpICMSEfet;
    property vICMSEfet: Currency read FvICMSEfet write FvICMSEfet;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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

    procedure Assign(Source: TPersistent); override;
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
    FvFCPUFDest: Currency;
    FvICMSUFDest: Currency;
    FvICMSUFRemet: Currency;
    FvFCP: Currency;
    FvBCST: Currency;
    FvST: Currency;
    FvFCPST: Currency;
    FvFCPSTRet: Currency;
    FvProd: Currency;
    FvFrete: Currency;
    FvSeg: Currency;
    FvDesc: Currency;
    FvII: Currency;
    FvIPI: Currency;
    FvIPIDevol: Currency;
    FvPIS: Currency;
    FvCOFINS: Currency;
    FvOutro: Currency;
    FvNF: Currency;
    FvTotTrib: Currency;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property vBC: Currency read FvBC write FvBC;
    property vICMS: Currency read FvICMS write FvICMS;
    property vICMSDeson: Currency read FvICMSDeson write FvICMSDeson;
    property vFCPUFDest: Currency read FvFCPUFDest write FvFCPUFDest;
    property vICMSUFDest: Currency read FvICMSUFDest write FvICMSUFDest;
    property vICMSUFRemet: Currency read FvICMSUFRemet write FvICMSUFRemet;
    property vFCP: Currency read FvFCP write FvFCP;
    property vBCST: Currency read FvBCST write FvBCST;
    property vST: Currency read FvST write FvST;
    property vFCPST: Currency read FvFCPST write FvFCPST;
    property vFCPSTRet: Currency read FvFCPSTRet write FvFCPSTRet;
    property vProd: Currency read FvProd write FvProd;
    property vFrete: Currency read FvFrete write FvFrete;
    property vSeg: Currency read FvSeg write FvSeg;
    property vDesc: Currency read FvDesc write FvDesc;
    property vII: Currency read FvII write FvII;
    property vIPI: Currency read FvIPI write FvIPI;
    property vIPIDevol: Currency read FvIPIDevol write FvIPIDevol;
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
  public
    procedure Assign(Source: TPersistent); override;
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
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
    procedure Assign(Source: TPersistent); override;
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

  TICMSUFDest = class(TPersistent)
  private
    FvBCUFDest: Currency;
    FvBCFCPUFDest: Currency;
    FpFCPUFDest: Currency;
    FpICMSUFDest: Currency;
    FpICMSInter: Currency;
    FpICMSInterPart: Currency;
    FvFCPUFDest: Currency;
    FvICMSUFDest: Currency;
    FvICMSUFRemet: Currency;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property vBCUFDest: Currency read FvBCUFDest write FvBCUFDest;
    property vBCFCPUFDest: Currency read FvBCFCPUFDest write FvBCFCPUFDest;
    property pFCPUFDest: Currency read FpFCPUFDest write FpFCPUFDest;
    property pICMSUFDest: Currency read FpICMSUFDest write FpICMSUFDest;
    property pICMSInter: Currency read FpICMSInter write FpICMSInter;
    property pICMSInterPart: Currency read FpICMSInterPart write FpICMSInterPart;
    property vFCPUFDest: Currency read FvFCPUFDest write FvFCPUFDest;
    property vICMSUFDest: Currency read FvICMSUFDest write FvICMSUFDest;
    property vICMSUFRemet: Currency read FvICMSUFRemet write FvICMSUFRemet;
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

    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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

    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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

    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
  published
    property nDup: String read FnDup write FnDup;
    property dVenc: TDateTime read FdVenc write FdVenc;
    property vDup: Currency read FvDup write FvDup;
  end;

  { TpagCollection }

  TpagCollection = class(TCollection)
  private
    FvTroco: Currency;

    function GetItem(Index: Integer): TpagCollectionItem;
    procedure SetItem(Index: Integer; Value: TpagCollectionItem);
  public
    constructor Create(AOwner: TNFe);
    procedure Assign(Source: TPersistent); override;

    function Add: TpagCollectionItem;

    property Items[Index: Integer]: TpagCollectionItem read GetItem write SetItem; default;
  published
    property vTroco: Currency read FvTroco write FvTroco;
  end;

  TpagCollectionItem = class(TCollectionItem)
  private
    FtPag: TpcnFormaPagamento;
    FvPag: Currency;
    FtpIntegra: TtpIntegra;
    FCNPJ: String;
    FtBand: TpcnBandeiraCartao;
    FcAut: String;
    FindPag: TpcnIndicadorPagamento;
  public
    constructor Create(AOwner: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property indPag: TpcnIndicadorPagamento read FindPag write FindPag default ipNenhum;
    property tPag: TpcnFormaPagamento read FtPag write FtPag;
    property vPag: Currency read FvPag write FvPag;
    property tpIntegra: TtpIntegra read FtpIntegra write FtpIntegra;
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

    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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

    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
  public
    procedure Assign(Source: TPersistent); override;
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
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
  end;

  TinfRespTec = class(TPersistent)
  private
    FCNPJ: String;
    FxContato: String;
    Femail: String;
    Ffone: String;
    FidCSRT: Integer;
    FhashCSRT: String;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property CNPJ: String     read FCNPJ     write FCNPJ;
    property xContato: String read FxContato write FxContato;
    property email: String    read Femail    write Femail;
    property fone: String     read Ffone     write Ffone;
    property idCSRT: Integer  read FidCSRT   write FidCSRT;
    property hashCSRT: String read FhashCSRT write FhashCSRT;
  end;

const

  CMUN_EXTERIOR: Integer = 9999999;
  XMUN_EXTERIOR: String = 'EXTERIOR';
  UF_EXTERIOR: String = 'EX';

implementation

Uses ACBrUtil, pcnNFeR;

procedure TNFe.Assign(Source: TPersistent);
begin
  if Source is TNFe then
  begin
    infNFe.Assign(TNFe(Source).infNFe);
    Ide.Assign(TNFe(Source).Ide);
    Emit.Assign(TNFe(Source).Emit);
    Avulsa.Assign(TNFe(Source).Avulsa);
    Dest.Assign(TNFe(Source).Dest);
    Retirada.Assign(TNFe(Source).Retirada);
    Entrega.Assign(TNFe(Source).Entrega);
    autXML.Assign(TNFe(Source).autXML);
    Det.Assign(TNFe(Source).Det);
    Total.Assign(TNFe(Source).Total);
    Transp.Assign(TNFe(Source).Transp);
    Cobr.Assign(TNFe(Source).Cobr);
    pag.Assign(TNFe(Source).pag);
    InfAdic.Assign(TNFe(Source).InfAdic);
    exporta.Assign(TNFe(Source).exporta);
    compra.Assign(TNFe(Source).compra);
    cana.Assign(TNFe(Source).cana);
    infNFeSupl.Assign(TNFe(Source).infNFeSupl);
    signature.Assign(TNFe(Source).signature);
    procNFe.Assign(TNFe(Source).procNFe);
    infRespTec.Assign(TNFe(Source).infRespTec);
  end
  else
    inherited; 
end;

procedure TNFe.SetXMLString(const AValue: AnsiString);
var
 LocNFeR : TNFeR;
begin
  LocNFeR := TNFeR.Create(Self);
  try
    LocNFeR.Leitor.Arquivo := AValue;
    LocNFeR.LerXml;
  finally
    LocNFeR.Free
  end;
end;

constructor TNFe.Create;
begin
  inherited Create;
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
  FinfNFeSupl := TinfNFeSupl.Create;
  Fsignature := Tsignature.create;
  FProcNFe := TProcNFe.create;
  FinfRespTec := TinfRespTec.create;

  FinfNFe.Versao := 0;

  FEmit.EnderEmit.xPais := 'BRASIL';
  FEmit.EnderEmit.cPais := 1058;

  FDest.EnderDest.xPais := 'BRASIL';
  FDest.EnderDest.cPais := 1058;

  Retirada.xPais := 'BRASIL';
  Retirada.cPais := 1058;

  Entrega.xPais := 'BRASIL';
  Entrega.cPais := 1058;
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
  FinfNFeSupl.Free;
  Fsignature.Free;
  FProcNFe.Free;
  FinfRespTec.Free;

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
////  Result.create;
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

procedure TDetCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TDetCollectionItem then
  begin
    Prod.Assign(TDetCollectionItem(Source).Prod);
    Imposto.Assign(TDetCollectionItem(Source).Imposto);
    pDevol := TDetCollectionItem(Source).pDevol;
    vIPIDevol := TDetCollectionItem(Source).vIPIDevol;
    infAdProd := TDetCollectionItem(Source).infAdProd;
  end
  else
    inherited;
end;

constructor TDetCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  FProd := TProd.Create(self);
  FImposto := TImposto.Create(self);
end;

{Ide}

procedure TIde.Assign(Source: TPersistent);
begin
  if Source is TIde then
  begin
    cUF := TIde(Source).cUF;
    cNF := TIde(Source).cNF;
    natOp := TIde(Source).natOp;
    indPag := TIde(Source).indPag;
    modelo := TIde(Source).modelo;
    serie := TIde(Source).serie;
    nNF := TIde(Source).nNF;
    dEmi := TIde(Source).dEmi;
    dSaiEnt := TIde(Source).dSaiEnt;
    hSaiEnt := TIde(Source).hSaiEnt;
    tpNF := TIde(Source).tpNF;
    idDest := TIde(Source).idDest;
    cMunFG := TIde(Source).cMunFG;
    NFref.Assign(TIde(Source).NFref);
    tpImp := TIde(Source).tpImp;
    tpEmis := TIde(Source).tpEmis;
    cDV := TIde(Source).cDV;
    tpAmb := TIde(Source).tpAmb;
    finNFe := TIde(Source).finNFe;
    indFinal := TIde(Source).indFinal;
    indPres := TIde(Source).indPres;
    procEmi := TIde(Source).procEmi;
    verProc := TIde(Source).verProc;
    dhCont := TIde(Source).dhCont;
    xJust := TIde(Source).xJust;
  end
  else
    inherited; 
end;

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
////  Result.create;
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

procedure TNFrefCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TNFrefCollectionItem then
  begin
    refNFe := TNFrefCollectionItem(Source).refNFe;
    refCTe := TNFrefCollectionItem(Source).refCTe;
    RefNF.Assign(TNFrefCollectionItem(Source).RefNF);
    RefNFP.Assign(TNFrefCollectionItem(Source).RefNFP);
    RefECF.Assign(TNFrefCollectionItem(Source).RefECF);
  end
  else
    inherited;
end;

constructor TNFrefCollectionItem.Create(Collection: TCollection);
begin
  inherited;
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

procedure TEmit.Assign(Source: TPersistent);
begin
  if Source is TEmit then
  begin
    CNPJCPF := TEmit(Source).CNPJCPF;
    xNome := TEmit(Source).xNome;
    xFant := TEmit(Source).xFant;
    EnderEmit.Assign(TEmit(Source).EnderEmit);
    IE := TEmit(Source).IE;
    IEST := TEmit(Source).IEST;
    IM := TEmit(Source).IM;
    CNAE := TEmit(Source).CNAE;
    CRT := TEmit(Source).CRT;
  end
  else
    inherited;
end;

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

procedure TDest.Assign(Source: TPersistent);
begin
  if Source is TDest then
  begin
    CNPJCPF := TDest(Source).CNPJCPF;
    idEstrangeiro := TDest(Source).idEstrangeiro;
    xNome := TDest(Source).xNome;
    EnderDest.Assign(TDest(Source).EnderDest);
    indIEDest := TDest(Source).indIEDest;
    IE := TDest(Source).IE;
    ISUF := TDest(Source).ISUF;
    IM := TDest(Source).IM;
    Email := TDest(Source).Email;
  end
  else
    inherited;
end;

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
  FindEscala := ieNenhum;

  FDI := TDICollection.Create(Self);
  FNVE := TNVECollection.Create(self);
  FdetExport := TdetExportCollection.Create(Self);
  FRastro := TrastroCollection.Create(Self);
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
  FRastro.Free;
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

procedure TProd.SetRastro(Value: TrastroCollection);
begin
  FRastro.Assign(Value);
end;

procedure TProd.SetdetExport(const Value: TdetExportCollection);
begin
  FdetExport := Value;
end;

procedure TProd.SetMed(Value: TmedCollection);
begin
  FMed.Assign(Value);
end;

procedure TProd.SetNVE(Value: TNVeCollection);
begin
  FNVE.Assign(Value);
end;

procedure TProd.SetArma(Value: TarmaCollection);
begin
  FArma.Assign(Value);
end;

{ TrastroCollection }

function TrastroCollection.GetItem(Index: Integer): TrastroCollectionItem;
begin
  Result := TrastroCollectionItem(inherited GetItem(Index));
end;

procedure TrastroCollection.SetItem(Index: Integer; Value: TrastroCollectionItem
  );
begin
  inherited SetItem(Index, Value);
end;

constructor TrastroCollection.Create(AOwner: TProd);
begin
  inherited Create(TrastroCollectionItem);
end;

destructor TrastroCollection.Destroy;
begin
  inherited;
end;

function TrastroCollection.Add: TrastroCollectionItem;
begin
  Result := TrastroCollectionItem(inherited Add)
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

procedure Tcomb.Assign(Source: TPersistent);
begin
  if Source is Tcomb then
  begin
    cProdANP := Tcomb(Source).cProdANP;
    descANP := Tcomb(Source).descANP;
    pMixGN := Tcomb(Source).pMixGN;
    CODIF := Tcomb(Source).CODIF;
    qTemp := Tcomb(Source).qTemp;
    UFcons := Tcomb(Source).UFcons;
    CIDE.Assign(Tcomb(Source).CIDE);
    ICMS.Assign(Tcomb(Source).ICMS);
    ICMSInter.Assign(Tcomb(Source).ICMSInter);
    ICMSCons.Assign(Tcomb(Source).ICMSCons);
    encerrante.Assign(Tcomb(Source).encerrante);
  end
  else
    inherited;
end;

constructor Tcomb.Create(AOwner: TProd);
begin
  inherited Create;
  FCIDE := TCIDE.Create;
  FICMS := TICMSComb.Create;
  FICMSInter := TICMSInter.Create;
  FICMScons := TICMScons.Create;
  Fencerrante := Tencerrante.Create;
end;

destructor Tcomb.Destroy;
begin
  FCIDE.Free;
  FICMS.Free;
  FICMSInter.Free;
  FICMScons.Free;
  Fencerrante.Free;
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
////  Result.create;
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

procedure TDICollectionItem.Assign(Source: TPersistent);
begin
  if Source is TDICollectionItem then
  begin
    nDi := TDICollectionItem(Source).nDi;
    dDi := TDICollectionItem(Source).dDi;
    xLocDesemb := TDICollectionItem(Source).xLocDesemb;
    UFDesemb := TDICollectionItem(Source).UFDesemb;
    dDesemb := TDICollectionItem(Source).dDesemb;
    tpViaTransp := TDICollectionItem(Source).tpViaTransp;
    vAFRMM := TDICollectionItem(Source).vAFRMM;
    tpIntermedio := TDICollectionItem(Source).tpIntermedio;
    CNPJ := TDICollectionItem(Source).CNPJ;
    UFTerceiro := TDICollectionItem(Source).UFTerceiro;
    cExportador := TDICollectionItem(Source).cExportador;
    adi.Assign(TDICollectionItem(Source).adi);
  end
  else
    inherited;
end;

constructor TDICollectionItem.Create(Collection: TCollection);
begin
  inherited;
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

procedure TImposto.Assign(Source: TPersistent);
begin
  if Source is TImposto then
  begin
    vTotTrib := TImposto(Source).vTotTrib;
    ICMS.Assign(TImposto(Source).ICMS);
    IPI.Assign(TImposto(Source).IPI);
    II.Assign(TImposto(Source).II);
    PIS.Assign(TImposto(Source).PIS);
    PISST.Assign(TImposto(Source).PISST);
    COFINS.Assign(TImposto(Source).COFINS);
    COFINSST.Assign(TImposto(Source).COFINSST);
    ISSQN.Assign(TImposto(Source).ISSQN);
    ICMSUFDest.Assign(TImposto(Source).ICMSUFDest);
  end
  else
    inherited;
end;

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
  FISSQN := TISSQN.Create;
  FICMSUFDest := TICMSUFDest.Create;
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
  FICMSUFDest.Free;
  inherited;
end;

{Total}

procedure TTotal.Assign(Source: TPersistent);
begin
  if Source is TTotal then
  begin
    ICMSTot.Assign(TTotal(Source).ICMSTot);
    ISSQNtot.Assign(TTotal(Source).ISSQNtot);
    retTrib.Assign(TTotal(Source).retTrib);
  end
  else
    inherited;
end;

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
////  Result.create;
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

procedure TVolCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TVolCollectionItem then
  begin
    qVol := TVolCollectionItem(Source).qVol;
    esp := TVolCollectionItem(Source).esp;
    marca := TVolCollectionItem(Source).marca;
    nVol := TVolCollectionItem(Source).nVol;
    pesoL := TVolCollectionItem(Source).pesoL;
    pesoB := TVolCollectionItem(Source).pesoB;
    Lacres.Assign(TVolCollectionItem(Source).Lacres);
  end
  else
    inherited;
end;

constructor TVolCollectionItem.Create(Collection: TCollection);
begin
  inherited;
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

procedure TretTransp.Assign(Source: TPersistent);
begin
  if Source is TretTransp then
  begin
    vServ := TretTransp(Source).vServ;
    vBCRet := TretTransp(Source).vBCRet;
    pICMSRet := TretTransp(Source).pICMSRet;
    vICMSRet := TretTransp(Source).vICMSRet;
    CFOP := TretTransp(Source).CFOP;
    cMunFG := TretTransp(Source).cMunFG;
  end
  else
    inherited;
end;

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

procedure TCobr.Assign(Source: TPersistent);
begin
  if Source is TCobr then
  begin
    Fat.Assign(TCobr(Source).Fat);
    Dup.Assign(TCobr(Source).Dup);
  end
  else
    inherited;
end;

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

procedure TInfAdic.Assign(Source: TPersistent);
begin
  if Source is TInfAdic then
  begin
    infAdFisco := TInfAdic(Source).infAdFisco;
    infCpl := TInfAdic(Source).infCpl;
    obsCont.Assign(TInfAdic(Source).obsCont);
    obsFisco.Assign(TInfAdic(Source).obsFisco);
    procRef.Assign(TInfAdic(Source).procRef);
  end
  else
    inherited;
end;

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

procedure TCana.Assign(Source: TPersistent);
begin
  if Source is TCana then
  begin
    safra := TCana(Source).safra;
    ref := TCana(Source).ref;
    fordia.Assign(TCana(Source).fordia);
    qTotMes := TCana(Source).qTotMes;
    qTotAnt := TCana(Source).qTotAnt;
    qTotGer := TCana(Source).qTotGer;
    deduc.Assign(TCana(Source).deduc);
    vFor := TCana(Source).vFor;
    vTotDed := TCana(Source).vTotDed;
    vLiqFor := TCana(Source).vLiqFor;
  end
  else
    inherited;
end;

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

procedure TinfNFe.Assign(Source: TPersistent);
begin
  if Source is TinfNFe then
  begin
    ID := TinfNFe(Source).ID;
    Versao := TinfNFe(Source).Versao;
  end
  else
    inherited;
end;

function TinfNFe.GetVersao: Real;
begin
  if FVersao <= 0 then
     Result := 2
  else
     Result := FVersao;
end;

function TinfNFe.GetVersaoStr: String;
begin
  if FVersao <= 0 then
     Result := V2_00
  else
     Result := 'versao="'+FloatToString(FVersao,'.','#0.00')+'"';
end;

{ TveicProd }

procedure TveicProd.Assign(Source: TPersistent);
begin
  if Source is TveicProd then
  begin
    tpOP := TveicProd(Source).tpOP;
    chassi := TveicProd(Source).chassi;
    cCor := TveicProd(Source).cCor;
    xCor := TveicProd(Source).xCor;
    pot := TveicProd(Source).pot;
    Cilin := TveicProd(Source).Cilin;
    pesoL := TveicProd(Source).pesoL;
    pesoB := TveicProd(Source).pesoB;
    nSerie := TveicProd(Source).nSerie;
    tpComb := TveicProd(Source).tpComb;
    nMotor := TveicProd(Source).nMotor;
    CMT := TveicProd(Source).CMT;
    dist := TveicProd(Source).dist;
    //RENAVAM := TveicProd(Source).RENAVAM;
    anoMod := TveicProd(Source).anoMod;
    anoFab := TveicProd(Source).anoFab;
    tpPint := TveicProd(Source).tpPint;
    tpVeic := TveicProd(Source).tpVeic;
    espVeic := TveicProd(Source).espVeic;
    VIN := TveicProd(Source).VIN;
    condVeic := TveicProd(Source).condVeic;
    cMod := TveicProd(Source).cMod;
    cCorDENATRAN := TveicProd(Source).cCorDENATRAN;
    lota := TveicProd(Source).lota;
    tpRest := TveicProd(Source).tpRest;
  end
  else
    inherited;
end;

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

procedure TpagCollection.Assign(Source: TPersistent);
begin
  if Source is TpagCollection then
    vTroco := TpagCollection(Source).vTroco;
  inherited;
end;

constructor TpagCollection.Create(AOwner: TNFe);
begin
  inherited Create(TpagCollectionItem);
  vTroco := 0;
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
////  Result.create;
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

{ TRefNF }

procedure TRefNF.Assign(Source: TPersistent);
begin
  if Source is TRefNF then
  begin
    cUF := TRefNF(Source).cUF;
    AAMM := TRefNF(Source).AAMM;
    CNPJ := TRefNF(Source).CNPJ;
    modelo := TRefNF(Source).modelo;
    serie := TRefNF(Source).serie;
    nNF := TRefNF(Source).nNF;
  end
  else
    inherited;
end;

{ TRefNFP }

procedure TRefNFP.Assign(Source: TPersistent);
begin
  if Source is TRefNFP then
  begin
    cUF := TRefNFP(Source).cUF;
    AAMM := TRefNFP(Source).AAMM;
    CNPJCPF := TRefNFP(Source).CNPJCPF;
    IE := TRefNFP(Source).IE;
    modelo := TRefNFP(Source).modelo;
    serie := TRefNFP(Source).serie;
    nNF := TRefNFP(Source).nNF;
  end
  else
    inherited;
end;

{ TRefECF }

procedure TRefECF.Assign(Source: TPersistent);
begin
  if Source is TRefECF then
  begin
    modelo := TRefECF(Source).modelo;
    nECF := TRefECF(Source).nECF;
    nCOO := TRefECF(Source).nCOO;
  end
  else
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
    UF := TenderEmit(Source).UF;
    CEP := TenderEmit(Source).CEP;
    cPais := TenderEmit(Source).cPais;
    xPais := TenderEmit(Source).xPais;
    fone := TenderEmit(Source).fone;
  end
  else
    inherited;
end;

{ TAvulsa }

procedure TAvulsa.Assign(Source: TPersistent);
begin
  if Source is TAvulsa then
  begin
    CNPJ := TAvulsa(Source).CNPJ;
    xOrgao := TAvulsa(Source).xOrgao;
    matr := TAvulsa(Source).matr;
    xAgente := TAvulsa(Source).xAgente;
    fone := TAvulsa(Source).fone;
    UF := TAvulsa(Source).UF;
    nDAR := TAvulsa(Source).nDAR;
    dEmi := TAvulsa(Source).dEmi;
    vDAR := TAvulsa(Source).vDAR;
    repEmi := TAvulsa(Source).repEmi;
    dPag := TAvulsa(Source).dPag;
  end
  else
    inherited;
end;

{ TEnderDest }

procedure TEnderDest.Assign(Source: TPersistent);
begin
  if Source is TEnderDest then
  begin
    xLgr := TEnderDest(Source).xLgr;
    nro := TEnderDest(Source).nro;
    xCpl := TEnderDest(Source).xCpl;
    xBairro := TEnderDest(Source).xBairro;
    cMun := TEnderDest(Source).cMun;
    xMun := TEnderDest(Source).xMun;
    UF := TEnderDest(Source).UF;
    CEP := TEnderDest(Source).CEP;
    cPais := TEnderDest(Source).cPais;
    xPais := TEnderDest(Source).xPais;
    fone := TEnderDest(Source).fone;
  end
  else
    inherited;
end;

{ TRetirada }

procedure TRetirada.Assign(Source: TPersistent);
begin
  if Source is TRetirada then
  begin
    CNPJCPF := TRetirada(Source).CNPJCPF;
    xNome := TRetirada(Source).xNome;
    xLgr := TRetirada(Source).xLgr;
    nro := TRetirada(Source).nro;
    xCpl := TRetirada(Source).xCpl;
    xBairro := TRetirada(Source).xBairro;
    cMun := TRetirada(Source).cMun;
    xMun := TRetirada(Source).xMun;
    UF := TRetirada(Source).UF;
    CEP := TRetirada(Source).CEP;
    cPais := TRetirada(Source).cPais;
    xPais := TRetirada(Source).xPais;
    fone := TRetirada(Source).fone;
    email := TRetirada(Source).email;
    IE := TRetirada(Source).IE;
  end
  else
    inherited;
end;

{ TEntrega }

procedure TEntrega.Assign(Source: TPersistent);
begin
  if Source is TEntrega then
  begin
    CNPJCPF := TEntrega(Source).CNPJCPF;
    xNome := TEntrega(Source).xNome;
    xLgr := TEntrega(Source).xLgr;
    nro := TEntrega(Source).nro;
    xCpl := TEntrega(Source).xCpl;
    xBairro := TEntrega(Source).xBairro;
    cMun := TEntrega(Source).cMun;
    xMun := TEntrega(Source).xMun;
    UF := TEntrega(Source).UF;
    CEP := TEntrega(Source).CEP;
    cPais := TEntrega(Source).cPais;
    xPais := TEntrega(Source).xPais;
    fone := TEntrega(Source).fone;
    email := TEntrega(Source).email;
    IE := TEntrega(Source).IE;
  end
  else
    inherited;
end;

procedure TProd.Assign(Source: TPersistent);
begin
  if Source is TProd then
  begin
    cProd := TProd(Source).cProd;
    nItem := TProd(Source).nItem;
    cEAN := TProd(Source).cEAN;
    xProd := TProd(Source).xProd;
    NCM := TProd(Source).NCM;
    NVE.Assign(TProd(Source).NVE);
    EXTIPI := TProd(Source).EXTIPI;
    //genero := TProd(Source).genero;
    CFOP := TProd(Source).CFOP;
    uCom := TProd(Source).uCom;
    qCom := TProd(Source).qCom;
    vUnCom := TProd(Source).vUnCom;
    vProd := TProd(Source).vProd;
    cEANTrib := TProd(Source).cEANTrib;
    uTrib := TProd(Source).uTrib;
    qTrib := TProd(Source).qTrib;
    vUnTrib := TProd(Source).vUnTrib;
    vFrete := TProd(Source).vFrete;
    vSeg := TProd(Source).vSeg;
    vDesc := TProd(Source).vDesc;
    vOutro := TProd(Source).vOutro;
    IndTot := TProd(Source).IndTot;
    DI.Assign(TProd(Source).DI);
    xPed := TProd(Source).xPed;
    nItemPed := TProd(Source).nItemPed;
    detExport.Assign(TProd(Source).detExport);
    veicProd.Assign(TProd(Source).veicProd);
    med.Assign(TProd(Source).med);
    rastro.Assign(TProd(Source).rastro);
    arma.Assign(TProd(Source).arma);
    comb.Assign(TProd(Source).comb);
    nRECOPI := TProd(Source).nRECOPI;
    nFCI := TProd(Source).nFCI;
    CEST := TProd(Source).CEST;
    indEscala := TProd(Source).indEscala;
    CNPJFab := TProd(Source).CNPJFab;
    cBenef := TProd(Source).cBenef;
  end
  else
    inherited;
end;

{ TrastroCollectionItem }

procedure TrastroCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TrastroCollectionItem then
  begin
    nLote := TrastroCollectionItem(Source).nLote;
    qLote := TrastroCollectionItem(Source).qLote;
    dFab := TrastroCollectionItem(Source).dFab;
    dVal := TrastroCollectionItem(Source).dVal;
    cAgreg := TrastroCollectionItem(Source).cAgreg;
  end
  else
    inherited;
end;

{ TMedCollectionItem }

procedure TMedCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TMedCollectionItem then
  begin
    cProdANVISA := TMedCollectionItem(Source).cProdANVISA;
    xMotivoIsencao := TMedCollectionItem(Source).xMotivoIsencao;
    nLote := TMedCollectionItem(Source).nLote;
    qLote := TMedCollectionItem(Source).qLote;
    dFab := TMedCollectionItem(Source).dFab;
    dVal := TMedCollectionItem(Source).dVal;
    vPMC := TMedCollectionItem(Source).vPMC;
  end
  else
    inherited;
end;

{ TArmaCollectionItem }

procedure TArmaCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TArmaCollectionItem then
  begin
    tpArma := TArmaCollectionItem(Source).tpArma;
    nSerie := TArmaCollectionItem(Source).nSerie;
    nCano := TArmaCollectionItem(Source).nCano;
    descr := TArmaCollectionItem(Source).descr;
  end
  else
    inherited;
end;

{ TCIDE }

procedure TCIDE.Assign(Source: TPersistent);
begin
  if Source is TCIDE then
  begin
    qBCProd := TCIDE(Source).qBCProd;
    vAliqProd := TCIDE(Source).vAliqProd;
    vCIDE := TCIDE(Source).vCIDE;
  end
  else
    inherited;
end;

{ TICMSComb }

procedure TICMSComb.Assign(Source: TPersistent);
begin
  if Source is TICMSComb then
  begin
    vBCICMS := TICMSComb(Source).vBCICMS;
    vICMS := TICMSComb(Source).vICMS;
    vBCICMSST := TICMSComb(Source).vBCICMSST;
    vICMSST := TICMSComb(Source).vICMSST;
  end
  else
    inherited;
end;

{ TICMSInter }

procedure TICMSInter.Assign(Source: TPersistent);
begin
  if Source is TICMSInter then
  begin
    vBCICMSSTDest := TICMSInter(Source).vBCICMSSTDest;
    vICMSSTDest := TICMSInter(Source).vICMSSTDest;
  end
  else
    inherited;
end;

{ TICMSCons }

procedure TICMSCons.Assign(Source: TPersistent);
begin
  if Source is TICMSCons then
  begin
    vBCICMSSTCons := TICMSCons(Source).vBCICMSSTCons;
    vICMSSTCons := TICMSCons(Source).vICMSSTCons;
    UFcons := TICMSCons(Source).UFcons;
  end
  else
    inherited;
end;

{ TAdiCollectionItem }

procedure TAdiCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TAdiCollectionItem then
  begin
    nAdicao := TAdiCollectionItem(Source).nAdicao;
    nSeqAdi := TAdiCollectionItem(Source).nSeqAdi;
    cFabricante := TAdiCollectionItem(Source).cFabricante;
    vDescDI := TAdiCollectionItem(Source).vDescDI;
    nDraw := TAdiCollectionItem(Source).nDraw;
  end
  else
    inherited;
end;

{ TNVECollectionItem }

procedure TNVECollectionItem.Assign(Source: TPersistent);
begin
  if Source is TNVECollectionItem then
  begin
    NVE := TNVECollectionItem(Source).NVE;
  end
  else
    inherited;
end;

{ TdetExportCollectionItem }

procedure TdetExportCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TdetExportCollectionItem then
  begin
    nDraw := TdetExportCollectionItem(Source).nDraw;
    nRE := TdetExportCollectionItem(Source).nRE;
    chNFe := TdetExportCollectionItem(Source).chNFe;
    qExport := TdetExportCollectionItem(Source).qExport;
  end
  else
    inherited;
end;

{ TICMS }

procedure TICMS.Assign(Source: TPersistent);
begin
  if Source is TICMS then
  begin
    orig := TICMS(Source).orig;
    CST := TICMS(Source).CST;
    CSOSN := TICMS(Source).CSOSN;
    modBC := TICMS(Source).modBC;
    pRedBC := TICMS(Source).pRedBC;
    vBC := TICMS(Source).vBC;
    pICMS := TICMS(Source).pICMS;
    vICMS := TICMS(Source).vICMS;
    modBCST := TICMS(Source).modBCST;
    pMVAST := TICMS(Source).pMVAST;
    pRedBCST := TICMS(Source).pRedBCST;
    vBCST := TICMS(Source).vBCST;
    pICMSST := TICMS(Source).pICMSST;
    vICMSST := TICMS(Source).vICMSST;
    UFST := TICMS(Source).UFST;
    pBCOp := TICMS(Source).pBCOp;
    vBCSTRet := TICMS(Source).vBCSTRet;
    vICMSSTRet := TICMS(Source).vICMSSTRet;
    motDesICMS := TICMS(Source).motDesICMS;
    pCredSN := TICMS(Source).pCredSN;
    vCredICMSSN := TICMS(Source).vCredICMSSN;
    vBCSTDest := TICMS(Source).vBCSTDest;
    vICMSSTDest := TICMS(Source).vICMSSTDest;
    vICMSDeson := TICMS(Source).vICMSDeson;
    vICMSOp := TICMS(Source).vICMSOp;
    pDif := TICMS(Source).pDif;
    vICMSDif := TICMS(Source).vICMSDif;
    vBCFCP := TICMS(Source).vBCFCP;
    pFCP := TICMS(Source).pFCP;
    vFCP := TICMS(Source).vFCP;
    vBCFCPST := TICMS(Source).vBCFCPST;
    pFCPST := TICMS(Source).pFCPST;
    vFCPST := TICMS(Source).vFCPST;
    vBCFCPSTRet := TICMS(Source).vBCFCPSTRet;
    pFCPSTRet := TICMS(Source).pFCPSTRet;
    vFCPSTRet := TICMS(Source).vFCPSTRet;
    pST := TICMS(Source).pST;
    pRedBCEfet := TICMS(Source).pRedBCEfet;
    vBCEfet := TICMS(Source).vBCEfet;
    pICMSEfet := TICMS(Source).pICMSEfet;
    vICMSEfet := TICMS(Source).vICMSEfet;
  end
  else
    inherited;
end;

{ TIPI }

procedure TIPI.Assign(Source: TPersistent);
begin
  if Source is TIPI then
  begin
    clEnq := TIPI(Source).clEnq;
    CNPJProd := TIPI(Source).CNPJProd;
    cSelo := TIPI(Source).cSelo;
    qSelo := TIPI(Source).qSelo;
    cEnq := TIPI(Source).cEnq;
    CST := TIPI(Source).CST;
    vBC := TIPI(Source).vBC;
    qUnid := TIPI(Source).qUnid;
    vUnid := TIPI(Source).vUnid;
    pIPI := TIPI(Source).pIPI;
    vIPI := TIPI(Source).vIPI;
  end
  else
    inherited;
end;

{ TII }

procedure TII.Assign(Source: TPersistent);
begin
  if Source is TII then
  begin
    vBc := TII(Source).vBc;
    vDespAdu := TII(Source).vDespAdu;
    vII := TII(Source).vII;
    vIOF := TII(Source).vIOF;
  end
  else
    inherited;
end;

{ TPIS }

procedure TPIS.Assign(Source: TPersistent);
begin
  if Source is TPIS then
  begin
    CST := TPIS(Source).CST;
    vBC := TPIS(Source).vBC;
    pPIS := TPIS(Source).pPIS;
    vPIS := TPIS(Source).vPIS;
    qBCProd := TPIS(Source).qBCProd;
    vAliqProd := TPIS(Source).vAliqProd;
  end
  else
    inherited;
end;

{ TPISST }

procedure TPISST.Assign(Source: TPersistent);
begin
  if Source is TPISST then
  begin
    vBc := TPISST(Source).vBc;
    pPis := TPISST(Source).pPis;
    qBCProd := TPISST(Source).qBCProd;
    vAliqProd := TPISST(Source).vAliqProd;
    vPIS := TPISST(Source).vPIS;
  end
  else
    inherited;
end;

{ TCOFINS }

procedure TCOFINS.Assign(Source: TPersistent);
begin
  if Source is TCOFINS then
  begin
    CST := TCOFINS(Source).CST;
    vBC := TCOFINS(Source).vBC;
    pCOFINS := TCOFINS(Source).pCOFINS;
    vCOFINS := TCOFINS(Source).vCOFINS;
    vBCProd := TCOFINS(Source).vBCProd;
    vAliqProd := TCOFINS(Source).vAliqProd;
    qBCProd := TCOFINS(Source).qBCProd;
  end
  else
    inherited;
end;

{ TICMSTot }

procedure TICMSTot.Assign(Source: TPersistent);
begin
  if Source is TICMSTot then
  begin
    vBC := TICMSTot(Source).vBC;
    vICMS := TICMSTot(Source).vICMS;
    vICMSDeson := TICMSTot(Source).vICMSDeson;
    vFCPUFDest := TICMSTot(Source).vFCPUFDest;
    vICMSUFDest := TICMSTot(Source).vICMSUFDest;
    vICMSUFRemet := TICMSTot(Source).vICMSUFRemet;
    vBCST := TICMSTot(Source).vBCST;
    vST := TICMSTot(Source).vST;
    vProd := TICMSTot(Source).vProd;
    vFrete := TICMSTot(Source).vFrete;
    vSeg := TICMSTot(Source).vSeg;
    vDesc := TICMSTot(Source).vDesc;
    vII := TICMSTot(Source).vII;
    vIPI := TICMSTot(Source).vIPI;
    vIPIDevol := TICMSTot(Source).vIPIDevol;
    vPIS := TICMSTot(Source).vPIS;
    vCOFINS := TICMSTot(Source).vCOFINS;
    vOutro := TICMSTot(Source).vOutro;
    vNF := TICMSTot(Source).vNF;
    vTotTrib := TICMSTot(Source).vTotTrib;
    vFCP := TICMSTot(Source).vFCP;
    vFCPST := TICMSTot(Source).vFCPST;
    vFCPSTRet := TICMSTot(Source).vFCPSTRet;
  end
  else
    inherited;
end;

{ TISSQNtot }

procedure TISSQNtot.Assign(Source: TPersistent);
begin
  if Source is TISSQNtot then
  begin
    vServ := TISSQNtot(Source).vServ;
    vBC := TISSQNtot(Source).vBC;
    vISS := TISSQNtot(Source).vISS;
    vPIS := TISSQNtot(Source).vPIS;
    vCOFINS := TISSQNtot(Source).vCOFINS;
    dCompet := TISSQNtot(Source).dCompet;
    vDeducao := TISSQNtot(Source).vDeducao;
//    vINSS := TISSQNtot(Source).vINSS;
//    vIR := TISSQNtot(Source).vIR;
//    vCSLL := TISSQNtot(Source).vCSLL;
    vOutro := TISSQNtot(Source).vOutro;
    vDescIncond := TISSQNtot(Source).vDescIncond;
    vDescCond := TISSQNtot(Source).vDescCond;
//    indISSRet := TISSQNtot(Source).indISSRet;
//    indISS := TISSQNtot(Source).indISS;
//    cServico := TISSQNtot(Source).cServico;
//    cMun := TISSQNtot(Source).cMun;
//    cPais := TISSQNtot(Source).cPais;
//    nProcesso := TISSQNtot(Source).nProcesso;
    vISSRet := TISSQNtot(Source).vISSRet;
    cRegTrib := TISSQNtot(Source).cRegTrib;
//    indIncentivo := TISSQNtot(Source).indIncentivo;
  end
  else
    inherited;
end;

{ TretTrib }

procedure TretTrib.Assign(Source: TPersistent);
begin
  if Source is TretTrib then
  begin
    vRetPIS := TretTrib(Source).vRetPIS;
    vRetCOFINS := TretTrib(Source).vRetCOFINS;
    vRetCSLL := TretTrib(Source).vRetCSLL;
    vBCIRRF := TretTrib(Source).vBCIRRF;
    vIRRF := TretTrib(Source).vIRRF;
    vBCRetPrev := TretTrib(Source).vBCRetPrev;
    vRetPrev := TretTrib(Source).vRetPrev;
  end
  else
    inherited;
end;

{ TCOFINSST }

procedure TCOFINSST.Assign(Source: TPersistent);
begin
  if Source is TCOFINSST then
  begin
    vBC := TCOFINSST(Source).vBC;
    pCOFINS := TCOFINSST(Source).pCOFINS;
    qBCProd := TCOFINSST(Source).qBCProd;
    vAliqProd := TCOFINSST(Source).vAliqProd;
    vCOFINS := TCOFINSST(Source).vCOFINS;
  end
  else
    inherited;
end;

{ TISSQN }

procedure TISSQN.Assign(Source: TPersistent);
begin
  if Source is TISSQN then
  begin
    vBC := TISSQN(Source).vBC;
    vAliq := TISSQN(Source).vAliq;
    vISSQN := TISSQN(Source).vISSQN;
    cMunFG := TISSQN(Source).cMunFG;
    cListServ := TISSQN(Source).cListServ;
    cSitTrib := TISSQN(Source).cSitTrib;
    vDeducao := TISSQN(Source).vDeducao;
    vOutro := TISSQN(Source).vOutro;
    vDescIncond := TISSQN(Source).vDescIncond;
    vDescCond := TISSQN(Source).vDescCond;
    indISSRet := TISSQN(Source).indISSRet;
    vISSRet := TISSQN(Source).vISSRet;
    indISS := TISSQN(Source).indISS;
    cServico := TISSQN(Source).cServico;
    cMun := TISSQN(Source).cMun;
    cPais := TISSQN(Source).cPais;
    nProcesso := TISSQN(Source).nProcesso;
    indIncentivo := TISSQN(Source).indIncentivo;
  end
  else
    inherited;
end;

procedure TTransp.Assign(Source: TPersistent);
begin
  if Source is TTransp then
  begin
    modFrete := TTransp(Source).modFrete;
    Transporta.Assign(TTransp(Source).Transporta);
    retTransp.Assign(TTransp(Source).retTransp);
    veicTransp.Assign(TTransp(Source).veicTransp);
    Vol.Assign(TTransp(Source).Vol);
    Reboque.Assign(TTransp(Source).Reboque);
    vagao := TTransp(Source).vagao;
    balsa := TTransp(Source).balsa;
  end
  else
    inherited;
end;

{ TTransporta }

procedure TTransporta.Assign(Source: TPersistent);
begin
  if Source is TTransporta then
  begin
    CNPJCPF := TTransporta(Source).CNPJCPF;
    xNome := TTransporta(Source).xNome;
    IE := TTransporta(Source).IE;
    xEnder := TTransporta(Source).xEnder;
    xMun := TTransporta(Source).xMun;
    UF := TTransporta(Source).UF;
  end
  else
    inherited;
end;

{ TveicTransp }

procedure TveicTransp.Assign(Source: TPersistent);
begin
  if Source is TveicTransp then
  begin
    placa := TveicTransp(Source).placa;
    UF := TveicTransp(Source).UF;
    RNTC := TveicTransp(Source).RNTC;
  end
  else
    inherited;
end;

{ TReboqueCollectionItem }

procedure TReboqueCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TReboqueCollectionItem then
  begin
    placa := TReboqueCollectionItem(Source).placa;
    UF := TReboqueCollectionItem(Source).UF;
    RNTC := TReboqueCollectionItem(Source).RNTC;
  end
  else
    inherited;
end;

{ TLacresCollectionItem }

procedure TLacresCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TLacresCollectionItem then
  begin
    nLacre := TLacresCollectionItem(Source).nLacre;
  end
  else
    inherited;
end;

{ TFat }

procedure TFat.Assign(Source: TPersistent);
begin
  if Source is TFat then
  begin
    nFat := TFat(Source).nFat;
    vOrig := TFat(Source).vOrig;
    vDesc := TFat(Source).vDesc;
    vLiq := TFat(Source).vLiq;
  end
  else
    inherited;
end;

{ TDupCollectionItem }

procedure TDupCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TDupCollectionItem then
  begin
    nDup := TDupCollectionItem(Source).nDup;
    dVenc := TDupCollectionItem(Source).dVenc;
    vDup := TDupCollectionItem(Source).vDup;
  end
  else
    inherited;
end;

{ TpagCollectionItem }

procedure TpagCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TpagCollectionItem then
  begin
    indPag := TpagCollectionItem(Source).indPag;
    tPag := TpagCollectionItem(Source).tPag;
    vPag := TpagCollectionItem(Source).vPag;
    tpIntegra := TpagCollectionItem(Source).tpIntegra;
    CNPJ := TpagCollectionItem(Source).CNPJ;
    tBand := TpagCollectionItem(Source).tBand;
    cAut := TpagCollectionItem(Source).cAut;
  end
  else
    inherited;
end;

constructor TpagCollectionItem.Create(AOwner: TCollection);
begin
  inherited Create(AOwner);
  FindPag := ipNenhum;
end;

{ TobsContCollectionItem }

procedure TobsContCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TobsContCollectionItem then
  begin
    xCampo := TobsContCollectionItem(Source).xCampo;
    xTexto := TobsContCollectionItem(Source).xTexto;
  end
  else
    inherited;
end;

{ TobsFiscoCollectionItem }

procedure TobsFiscoCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TobsFiscoCollectionItem then
  begin
    xCampo := TobsFiscoCollectionItem(Source).xCampo;
    xTexto := TobsFiscoCollectionItem(Source).xTexto;
  end
  else
    inherited;
end;

{ TprocRefCollectionItem }

procedure TprocRefCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TprocRefCollectionItem then
  begin
    nProc := TprocRefCollectionItem(Source).nProc;
    indProc := TprocRefCollectionItem(Source).indProc;
  end
  else
    inherited;
end;

{ TExporta }

procedure TExporta.Assign(Source: TPersistent);
begin
  if Source is TExporta then
  begin
    UFembarq := TExporta(Source).UFembarq;
    xLocEmbarq := TExporta(Source).xLocEmbarq;
    // Versao 3.10
    UFSaidaPais := TExporta(Source).UFSaidaPais;
    xLocExporta := TExporta(Source).xLocExporta;
    xLocDespacho := TExporta(Source).xLocDespacho;
  end
  else
    inherited;
end;

{ TCompra }

procedure TCompra.Assign(Source: TPersistent);
begin
  if Source is TCompra then
  begin
    xNEmp := TCompra(Source).xNEmp;
    xPed := TCompra(Source).xPed;
    xCont := TCompra(Source).xCont;
  end
  else
    inherited;
end;

{ TForDiaCollectionItem }

procedure TForDiaCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TForDiaCollectionItem then
  begin
    dia := TForDiaCollectionItem(Source).dia;
    qtde := TForDiaCollectionItem(Source).qtde;
  end
  else
    inherited;
end;

{ TDeducCollectionItem }

procedure TDeducCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TDeducCollectionItem then
  begin
    xDed := TDeducCollectionItem(Source).xDed;
    vDed := TDeducCollectionItem(Source).vDed;
  end
  else
    inherited;
end;

{ Tencerrante }

procedure Tencerrante.Assign(Source: TPersistent);
begin
  if Source is Tencerrante then
  begin
    nBico := Tencerrante(Source).nBico;
    nBomba := Tencerrante(Source).nBomba;
    nTanque := Tencerrante(Source).nTanque;
    vEncIni := Tencerrante(Source).vEncIni;
    vEncFin := Tencerrante(Source).vEncFin;
  end
  else
    inherited;
end;

{ TinfNFeSupl }

procedure TinfNFeSupl.Assign(Source: TPersistent);
begin
  if Source is TinfNFeSupl then
  begin
    qrCode := TinfNFeSupl(Source).qrCode;
    urlChave := TinfNFeSupl(Source).urlChave;
  end
  else
    inherited;
end;

{ TICMSUFDest }

procedure TICMSUFDest.Assign(Source: TPersistent);
begin
  if Source is TICMSUFDest then
  begin
    vBCUFDest := TICMSUFDest(Source).vBCUFDest;
    vBCFCPUFDest := TICMSUFDest(Source).vBCFCPUFDest;
    pFCPUFDest := TICMSUFDest(Source).pFCPUFDest;
    pICMSUFDest := TICMSUFDest(Source).pICMSUFDest;
    pICMSInter := TICMSUFDest(Source).pICMSInter;
    pICMSInterPart := TICMSUFDest(Source).pICMSInterPart;
    vFCPUFDest := TICMSUFDest(Source).vFCPUFDest;
    vICMSUFDest := TICMSUFDest(Source).vICMSUFDest;
    vICMSUFRemet := TICMSUFDest(Source).vICMSUFRemet;
  end
  else
    inherited;
end;

{ TinfRespTec }

procedure TinfRespTec.Assign(Source: TPersistent);
begin
  if Source is TinfRespTec then
  begin
    CNPJ := TinfRespTec(Source).CNPJ;
    xContato := TinfRespTec(Source).xContato;
    email := TinfRespTec(Source).email;
    fone := TinfRespTec(Source).fone;
    idCSRT := TinfRespTec(Source).idCSRT;
    hashCSRT := TinfRespTec(Source).hashCSRT;
  end
  else
    inherited;
end;

end.

