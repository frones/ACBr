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
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  pcnConversao, pcnConversaoNFe, pcnSignature, pcnProcNFe, pcnGerador,
  ACBrBase;

type
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
  TRastroCollection = class;
  TRastroCollectionItem = class;
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
  TinfIntermed = class;
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

  TNFe = class(TObject)
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
    FinfIntermed: TinfIntermed;
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

    procedure Assign(Source: TNFe);
    procedure SetXMLString(const AValue : AnsiString);
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
    property infIntermed: TinfIntermed read FinfIntermed write FinfIntermed;
    property InfAdic: TInfAdic read FInfAdic write FInfAdic;
    property exporta: Texporta read Fexporta write Fexporta;
    property compra: Tcompra read Fcompra write Fcompra;
    property cana: Tcana read Fcana write Fcana;
    property infNFeSupl: TinfNFeSupl read FinfNFeSupl write FinfNFeSupl;
    property signature: Tsignature read Fsignature write Fsignature;
    property procNFe: TProcNFe read FProcNFe write FProcNFe;
    property infRespTec: TinfRespTec read FinfRespTec write FinfRespTec;
  end;

  TinfNFeSupl = class(TObject)
  private
    FqrCode: String;
    FurlChave: String;
  public
    procedure Assign(Source: TinfNFeSupl);
    property qrCode: String read FqrCode write FqrCode;
    property urlChave: String read FurlChave write FurlChave;
  end;

  TinfNFe = class(TObject)
  private
    FID: String;
    FVersao: Real;
    function GetVersaoStr: String;
    function GetVersao: Real;
  public
    procedure Assign(Source: TinfNFe);
    property ID: String read FID write FID;
    property Versao: Real read GetVersao write FVersao;
    property VersaoStr: String read GetVersaoStr;
  end;

  TIde = class(TObject)
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
    FindIntermed: TindIntermed;
    FprocEmi: TpcnProcessoEmissao;
    FverProc: String;
    FdhCont : TDateTime;
    FxJust  : String;

    procedure SetNFref(Value: TNFrefCollection);
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TIde);
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
    property indIntermed: TindIntermed read FindIntermed write FindIntermed;
    property procEmi: TpcnProcessoEmissao read FprocEmi write FprocEmi default peAplicativoContribuinte;
    property verProc: String read FverProc write FverProc;
    property dhCont: TDateTime read FdhCont write FdhCont;
    property xJust: String read FxJust write FxJust;
  end;

  TNFrefCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TNFrefCollectionItem;
    procedure SetItem(Index: Integer; Value: TNFrefCollectionItem);
  public
    function Add: TNFrefCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TNFrefCollectionItem;
    property Items[Index: Integer]: TNFrefCollectionItem read GetItem write SetItem; default;
  end;

  TNFrefCollectionItem = class(TObject)
  private
    FrefNFe: String;
    FrefNFeSig: String;
    FrefCTe: String;
    FRefNF: TRefNF;
    FRefECF: TRefECF;
    FRefNFP: TRefNFP;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TNFrefCollectionItem);
    property refNFe: String read FrefNFe write FrefNFe;
    property refNFeSig: String read FrefNFeSig write FrefNFeSig;
    property refCTe: String read FrefCTe write FrefCTe;
    property RefNF: TRefNF read FRefNF write FRefNF;
    property RefNFP: TRefNFP read FRefNFP write FRefNFP;
    property RefECF: TRefECF read FRefECF write FRefECF;
  end;

  TRefNF = class(TObject)
  private
    FcUF: Integer;
    FAAMM: String;
    FCNPJ: String;
    Fmodelo: Integer;
    Fserie: Integer;
    FnNF: Integer;
  public
    procedure Assign(Source: TRefNF);
    property cUF: Integer read FcUF write FcUF;
    property AAMM: String read FAAMM write FAAMM;
    property CNPJ: String read FCNPJ write FCNPJ;
    property modelo: Integer read FModelo write Fmodelo;
    property serie: Integer read FSerie write Fserie;
    property nNF: Integer read FnNF write FnNF;
  end;

  TRefNFP = class(TObject)
  private
    FcUF: Integer;
    FAAMM: String;
    FCNPJCPF: String;
    FIE: String;
    Fmodelo: String;
    Fserie: Integer;
    FnNF: Integer;
  public
    procedure Assign(Source: TRefNFP);
    property cUF: Integer read FcUF write FcUF;
    property AAMM: String read FAAMM write FAAMM;
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property IE: String read FIE write FIE;
    property modelo: String read FModelo write Fmodelo;
    property serie: Integer read FSerie write Fserie;
    property nNF: Integer read FnNF write FnNF;
  end;

  TRefECF = class(TObject)
  private
    Fmodelo: TpcnECFModRef;
    FnECF: String;
    FnCOO: String;
  public
    procedure Assign(Source: TRefECF);
    property modelo:TpcnECFModRef read FModelo write Fmodelo default ECFModRefVazio;
    property nECF: String read FnECF write FnECF;
    property nCOO: String read FnCOO write FnCOO;
  end;


  TEmit = class(TObject)
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
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TEmit);
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

  TenderEmit = class(TObject)
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
    procedure Assign(Source: TenderEmit);
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

  TAvulsa = class(TObject)
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
    procedure Assign(Source: TAvulsa);
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

  TDest = class(TObject)
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
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TDest);
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

  TEnderDest = class(TObject)
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
    procedure Assign(Source: TEnderDest);
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

  TRetirada = class(TObject)
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
    procedure Assign(Source: TRetirada);
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

  TEntrega = class(TObject)
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
    procedure Assign(Source: TEntrega);
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

  TDetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDetCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetCollectionItem);
  public
    procedure Assign(Source: TDetCollection);
    function Add: TDetCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDetCollectionItem;
    property Items[Index: Integer]: TDetCollectionItem read GetItem write SetItem; default;
  end;

  TobsItem = class(TObject)
  private
    FxCampo: String;
    FxTexto: String;
  public
    procedure Assign(Source: TobsItem);

    property xCampo: String read FxCampo write FxCampo;
    property xTexto: String read FxTexto write FxTexto;
  end;

  TDetCollectionItem = class(TObject)
  private
    FProd: TProd;
    FImposto: TImposto;
    FpDevol: Currency;
    FvIPIDevol: Currency;
    FinfAdProd: String;
    FobsCont: TobsItem;
    FobsFisco: TobsItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TDetCollectionItem);

    property Prod: TProd read FProd write FProd;
    property Imposto: TImposto read FImposto write FImposto;
    property pDevol: Currency read FpDevol write FpDevol;
    property vIPIDevol: Currency read FvIPIDevol write FvIPIDevol;
    property infAdProd: String read FinfAdProd write FinfAdProd;
    property obsCont: TobsItem read FobsCont write FobsCont;
    property obsFisco: TobsItem read FobsFisco write FobsFisco;
  end;

  { TProd }

  TProd = class(TObject)
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
    FRastro: TRastroCollection;
    FveicProd: TveicProd;
    Fmed: TMedCollection;
    Farma: TarmaCollection;
    Fcomb: TComb;
    FnRECOPI: String;
    FnFCI: String;
    FNVE: TNVECollection;
    FCEST: String;
    FindEscala: TpcnIndEscala;
    FCNPJFab: String;
    FcBenef: String;
    FcBarra: String;
    FcBarraTrib: String;

    procedure SetDI(Value: TDICollection);
    procedure SetRastro(Value: TRastroCollection);
    procedure SetMed(Value: TmedCollection);
    procedure SetArma(Value: TarmaCollection);
    procedure SetdetExport(const Value: TdetExportCollection);
    procedure SetNVE(Value : TNVeCollection);
    procedure setCFOP(const Value: String);
  public
    constructor Create();
    destructor Destroy; override;
    procedure Assign(Source: TProd);
    property cProd: String read FcProd write FcProd;
    property nItem: Integer read FnItem write FnItem;
    property cEAN: String read FcEAN write FcEAN;
    property cBarra: String read FcBarra write FcBarra;
    property xProd: String read FxProd write FxProd;
    property NCM: String read FNCM write FNCM;
    property NVE : TNVECollection read FNVE write SetNVE; //FNVE;
    property EXTIPI: String read FEXTIPI write FEXTIPI;
    //property genero: Integer read Fgenero write Fgenero;
    property CFOP: String read FCFOP write setCFOP;
    property uCom: String read FuCom write FuCom;
    property qCom: Currency read FqCom write FqCom;
    property vUnCom: Double read FvUnCom write FvUnCom;
    property vProd: Currency read FvProd write FvProd;
    property cEANTrib: String read FcEANTrib write FcEANTrib;
    property cBarraTrib: String read FcBarraTrib write FcBarraTrib;
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
    property rastro: TRastroCollection read FRastro write SetRastro;
    property veicProd: TveicProd read FveicProd write FveicProd;
    property med: TMedCollection read Fmed write SetMed;
    property arma: TarmaCollection read Farma write SetArma;
    property comb: TComb read Fcomb write Fcomb;
    property nRECOPI: String read FnRECOPI write FnRECOPI;
    property nFCI: String read FnFCI write FnFCI;
    property CEST: String read FCEST write FCEST;
    property indEscala: TpcnIndEscala read FindEscala write FindEscala default ieNenhum;
    property CNPJFab: String read FCNPJFab write FCNPJFab;
    property cBenef: String read FcBenef write FcBenef;
  end;

  TveicProd = class(TObject)
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
    procedure Assign(Source: TveicProd);
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

  TRastroCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRastroCollectionItem;
    procedure SetItem(Index: Integer; Value: TRastroCollectionItem);
  public
    function Add: TRastroCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRastroCollectionItem;
    property Items[Index: Integer]: TRastroCollectionItem read GetItem write SetItem; default;
  end;

  TRastroCollectionItem = class(TObject)
  private
    FnLote: String;
    FqLote: Currency;
    FdFab: TDateTime;
    FdVal: TDateTime;
    FcAgreg: String;
  public
    procedure Assign(Source: TRastroCollectionItem);
    property nLote: String read FnLote write FnLote;
    property qLote: Currency read FqLote write FqLote;
    property dFab: TDateTime read FdFab write FdFab;
    property dVal: TDateTime read FdVal write FdVal;
    property cAgreg: String read FcAgreg write FcAgreg;
  end;

  TMedCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TMedCollectionItem;
    procedure SetItem(Index: Integer; Value: TMedCollectionItem);
  public
    function Add: TMedCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TMedCollectionItem;
    property Items[Index: Integer]: TMedCollectionItem read GetItem write SetItem; default;
  end;

  TMedCollectionItem = class(TObject)
  private
    FcProdANVISA: String;
    FxMotivoIsencao: String;
    FnLote: String;
    FqLote: Currency;
    FdFab: TDateTime;
    FdVal: TDateTime;
    FvPMC: Currency;
  public
    procedure Assign(Source: TMedCollectionItem);
    property cProdANVISA: String read FcProdANVISA write FcProdANVISA;
    property xMotivoIsencao: String read FxMotivoIsencao write FxMotivoIsencao;
    property nLote: String read FnLote write FnLote;
    property qLote: Currency read FqLote write FqLote;
    property dFab: TDateTime read FdFab write FdFab;
    property dVal: TDateTime read FdVal write FdVal;
    property vPMC: Currency read FvPMC write FvPMC;
  end;

  TArmaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TArmaCollectionItem;
    procedure SetItem(Index: Integer; Value: TArmaCollectionItem);
  public
    function Add: TArmaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TArmaCollectionItem;
    property Items[Index: Integer]: TArmaCollectionItem read GetItem write SetItem; default;
  end;

  TArmaCollectionItem = class(TObject)
  private
    FtpArma: TpcnTipoArma;
    FnSerie: String;
    FnCano: String;
    Fdescr: String;
  public
    procedure Assign(Source: TArmaCollectionItem);
    property tpArma: TpcnTipoArma read FtpArma write FtpArma default taUsoPermitido;
    property nSerie: String read FnSerie write FnSerie;
    property nCano: String read FnCano write FnCano;
    property descr: String read Fdescr write Fdescr;
  end;

  TorigCombCollectionItem = class(TObject)
  private
    FindImport: TindImport;
    FcUFOrig: Integer;
    FpOrig: Currency;
  public
    procedure Assign(Source: TorigCombCollectionItem);

    property indImport: TindImport read FindImport write FindImport;
    property cUFOrig: Integer read FcUFOrig write FcUFOrig;
    property pOrig: Currency read FpOrig write FpOrig;
  end;

  TorigCombCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TorigCombCollectionItem;
    procedure SetItem(Index: Integer; Value: TorigCombCollectionItem);
  public
    function Add: TorigCombCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TorigCombCollectionItem;
    property Items[Index: Integer]: TorigCombCollectionItem read GetItem write SetItem; default;
  end;

  Tencerrante = class(TObject)
  private
    FnBico: Integer;
    FnBomba: Integer;
    FnTanque: Integer;
    FvEncIni: Currency;
    FvEncFin: Currency;
  public
    procedure Assign(Source: Tencerrante);

    property nBico: Integer read FnBico write FnBico;
    property nBomba: Integer read FnBomba write FnBomba;
    property nTanque: Integer read FnTanque write FnTanque;
    property vEncIni: Currency read FvEncIni write FvEncIni;
    property vEncFin: Currency read FvEncFin write FvEncFin;
  end;

  TComb = class(TObject)
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
    FpBio: Currency;
    ForigComb: TorigCombCollection;

    procedure SetorigComb(const Value: TorigCombCollection);
  public
    constructor Create();
    destructor Destroy; override;
    procedure Assign(Source: TComb);

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
    property pBio: Currency read FpBio write FpBio;
    property origComb: TorigCombCollection read ForigComb write SetorigComb;
  end;

  TCIDE = class(TObject)
  private
    FqBCProd: Currency;
    FvAliqProd: Currency;
    FvCIDE: Currency;
  public
    procedure Assign(Source: TCIDE);
    property qBCProd: Currency read FqBCProd write FqBCProd;
    property vAliqProd: Currency read FvAliqProd write FvAliqProd;
    property vCIDE: Currency read FvCIDE write FvCIDE;
  end;

  TICMSComb = class(TObject)
  private
    FvBCICMS: Currency;
    FvICMS: Currency;
    FvBCICMSST: Currency;
    FvICMSST: Currency;
  public
    procedure Assign(Source: TICMSComb);
    property vBCICMS: Currency read FvBCICMS write FvBCICMS;
    property vICMS: Currency read FvICMS write FvICMS;
    property vBCICMSST: Currency read FvBCICMSST write FvBCICMSST;
    property vICMSST: Currency read FvICMSST write FvICMSST;
  end;

  TICMSInter = class(TObject)
  private
    FvBCICMSSTDest: Currency;
    FvICMSSTDest: Currency;
  public
    procedure Assign(Source: TICMSInter);
    property vBCICMSSTDest: Currency read FvBCICMSSTDest write FvBCICMSSTDest;
    property vICMSSTDest: Currency read FvICMSSTDest write FvICMSSTDest;
  end;

  TICMSCons = class(TObject)
  private
    FvBCICMSSTCons: Currency;
    FvICMSSTCons: Currency;
    FUFcons: String;
  public
    procedure Assign(Source: TICMSCons);
    property vBCICMSSTCons: Currency read FvBCICMSSTCons write FvBCICMSSTCons;
    property vICMSSTCons: Currency read FvICMSSTCons write FvICMSSTCons;
    property UFcons: String read FUFcons write FUFcons;
  end;

  TDICollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDICollectionItem;
    procedure SetItem(Index: Integer; Value: TDICollectionItem);
  public
    function Add: TDICollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDICollectionItem;
    property Items[Index: Integer]: TDICollectionItem read GetItem write SetItem; default;
  end;

  TDICollectionItem = class(TObject)
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
    Fadi: TAdiCollection;

    procedure SetAdi(Value: TAdiCollection);
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TDICollectionItem);
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

  TAdiCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TAdiCollectionItem;
    procedure SetItem(Index: Integer; Value: TAdiCollectionItem);
  public
    function Add: TAdiCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TAdiCollectionItem;
    property Items[Index: Integer]: TAdiCollectionItem read GetItem write SetItem; default;
  end;

  TAdiCollectionItem = class(TObject)
  private
    FnAdicao: Integer;
    FnSeqAdi: Integer;
    FcFabricante: String;
    FvDescDI: Currency;
    FnDraw: String;
  public
    procedure Assign(Source: TAdiCollectionItem);
    property nAdicao: Integer read FnAdicao write FnAdicao;
    property nSeqAdi: Integer read FnSeqAdi write FnSeqAdi;
    property cFabricante: String read FcFabricante write FcFabricante;
    property vDescDI: Currency read FvDescDI write FvDescDI;
    property nDraw: String read FnDraw write FnDraw;
  end;

  TNVECollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TNVECollectionItem;
    procedure SetItem(Index: Integer; Value: TNVECollectionItem);
  public
    function Add: TNVECollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TNVECollectionItem;
    property Items[Index: Integer]: TNVECollectionItem read GetItem write SetItem; default;
  end;

  TNVECollectionItem = class(TObject)
  private
    FNve: String;
  public
    procedure Assign(Source: TNVECollectionItem);
    property NVE: String read FNve write FNve;
  end;

  TdetExportCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdetExportCollectionItem;
    procedure SetItem(Index: Integer; Value: TdetExportCollectionItem);
  public
    function Add: TdetExportCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TdetExportCollectionItem;
    property Items[Index: Integer]: TdetExportCollectionItem read GetItem write SetItem; default;
  end;

  TdetExportCollectionItem = class(TObject)
  private
    FnDraw: String;
    FnRE: String;
    FchNFe: String;
    FqExport: Currency;
  public
    procedure Assign(Source: TdetExportCollectionItem);
    property nDraw: String read FnDraw write FnDraw;
    property nRE: String read FnRE write FnRE;
    property chNFe: String read FchNFe write FchNFe;
    property qExport: Currency read FqExport write FqExport;
  end;

  TImposto = class(TObject)
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
    constructor Create();
    destructor Destroy; override;
    procedure Assign(Source: TImposto);
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

  TICMS = class(TObject)
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
    FvICMSSubstituto: Currency;
    FvICMSSTDeson: Currency;
    FmotDesICMSST: TpcnMotivoDesoneracaoICMS;
    FvFCPDif: Currency;
    FvFCPEfet: Currency;
    FpFCPDif: Currency;
    FadRemICMS: Currency;
    FvICMSMono: Currency;
    FadRemICMSReten: Currency;
    FvICMSMonoReten: Currency;
//    FadRemICMSDif: Currency;
    FvICMSMonoDif: Currency;
    FadRemICMSRet: Currency;
    FvICMSMonoRet: Currency;
    FqBCMono: Currency;
    FqBCMonoReten: Currency;
    FpRedAdRem: Currency;
    FmotRedAdRem: TmotRedAdRem;
    FvICMSMonoOp: Currency;
    FqBCMonoRet: Currency;
    FindDeduzDeson: TIndicador;
  public
    constructor Create;
    procedure Assign(Source: TICMS);

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
    property vICMSSubstituto: Currency read FvICMSSubstituto write FvICMSSubstituto;
    property vICMSSTDeson: Currency read FvICMSSTDeson write FvICMSSTDeson;
    property motDesICMSST: TpcnMotivoDesoneracaoICMS read FmotDesICMSST write FmotDesICMSST;
    property pFCPDif: Currency read FpFCPDif write FpFCPDif;
    property vFCPDif: Currency read FvFCPDif write FvFCPDif;
    property vFCPEfet: Currency read FvFCPEfet write FvFCPEfet;
    // CST 02, 15
    property adRemICMS: Currency read FadRemICMS write FadRemICMS;
    property vICMSMono: Currency read FvICMSMono write FvICMSMono;
    property qBCMono: Currency read FqBCMono write FqBCMono;
    // CST 15
    property adRemICMSReten: Currency read FadRemICMSReten write FadRemICMSReten;
    property vICMSMonoReten: Currency read FvICMSMonoReten write FvICMSMonoReten;
    property qBCMonoReten: Currency read FqBCMonoReten write FqBCMonoReten;
    property pRedAdRem: Currency read FpRedAdRem write FpRedAdRem;
    property motRedAdRem: TmotRedAdRem read FmotRedAdRem write FmotRedAdRem;
    // CST 53
//    property adRemICMSDif: Currency read FadRemICMSDif write FadRemICMSDif;
    property vICMSMonoOp: Currency read FvICMSMonoOp write FvICMSMonoOp;
    property vICMSMonoDif: Currency read FvICMSMonoDif write FvICMSMonoDif;
    // CST 61
    property adRemICMSRet: Currency read FadRemICMSRet write FadRemICMSRet;
    property vICMSMonoRet: Currency read FvICMSMonoRet write FvICMSMonoRet;
    property qBCMonoRet: Currency read FqBCMonoRet write FqBCMonoRet;

    property indDeduzDeson: TIndicador read FindDeduzDeson write FindDeduzDeson default tiNao;
  end;

  TIPI = class(TObject)
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
    procedure Assign(Source: TIPI);
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

  TII = class(TObject)
  private
    FvBc: Currency;
    FvDespAdu: Currency;
    FvII: Currency;
    FvIOF: Currency;
  public
    procedure Assign(Source: TII);
    property vBc: Currency read FvBC write FvBC;
    property vDespAdu: Currency read FvDespAdu write FvDespAdu;
    property vII: Currency read FvII write FvII;
    property vIOF: Currency read FvIOF write FvIOF;
  end;

  TPIS = class(TObject)
  private
    FCST: TpcnCstPis;
    FvBC: Currency;
    FpPIS: Currency;
    FvPIS: Currency;
    FqBCProd: Currency;
    FvAliqProd: Currency;
  public
    procedure Assign(Source: TPIS);
    property CST: TpcnCstPis read FCST write FCST default pis01;
    property vBC: Currency read FvBC write FvBC;
    property pPIS: Currency read FpPIS write FpPIS;
    property vPIS: Currency read FvPIS write FvPIS;
    property qBCProd: Currency read FqBCProd write FqBCProd;
    property vAliqProd: Currency read FvAliqProd write FvAliqProd;
  end;

  TPISST = class(TObject)
  private
    FvBc: Currency;
    FpPis: Currency;
    FqBCProd: Currency;
    FvAliqProd: Currency;
    FvPIS: Currency;
    FindSomaPISST: TIndSomaPISST;
  public
    procedure Assign(Source: TPISST);
    property vBc: Currency read FvBc write FvBc;
    property pPis: Currency read FpPis write FpPis;
    property qBCProd: Currency read FqBCProd write FqBCProd;
    property vAliqProd: Currency read FvAliqProd write FvAliqProd;
    property vPIS: Currency read FvPIS write FvPIS;
    property indSomaPISST: TIndSomaPISST read FindSomaPISST write FindSomaPISST;
  end;

  TCOFINS = class(TObject)
  private
    FCST: TpcnCstCofins;
    FvBC: Currency;
    FpCOFINS: Currency;
    FvCOFINS: Currency;
    FvBCProd: Currency;
    FvAliqProd: Currency;
    FqBCProd: Currency;
  public
    procedure Assign(Source: TCOFINS);
    property CST: TpcnCstCofins read FCST write FCST default cof01;
    property vBC: Currency read FvBC write FvBC;
    property pCOFINS: Currency read FpCOFINS write FpCOFINS;
    property vCOFINS: Currency read FvCOFINS write FvCOFINS;
    property vBCProd: Currency read FvBCProd write FvBCProd;
    property vAliqProd: Currency read FvAliqProd write FvAliqProd;
    property qBCProd: Currency read FqBCProd write FqBCProd;
  end;

  TTotal = class(TObject)
  private
    FICMSTot: TICMSTot;
    FISSQNtot: TISSQNtot;
    FretTrib: TretTrib;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TTotal);
    property ICMSTot: TICMSTot read FICMSTot write FICMSTot;
    property ISSQNtot: TISSQNtot read FISSQNtot write FISSQNtot;
    property retTrib: TretTrib read FretTrib write FretTrib;
  end;

  TICMSTot = class(TObject)
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
    FvICMSMono: Currency;
    FvICMSMonoReten: Currency;
    FvICMSMonoRet: Currency;
    FqBCMono: Currency;
    FqBCMonoReten: Currency;
    FqBCMonoRet: Currency;
  public
    procedure Assign(Source: TICMSTot);

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
    property vICMSMono: Currency read FvICMSMono write FvICMSMono;
    property vICMSMonoReten: Currency read FvICMSMonoReten write FvICMSMonoReten;
    property vICMSMonoRet: Currency read FvICMSMonoRet write FvICMSMonoRet;
    property qBCMono: Currency read FqBCMono write FqBCMono;
    property qBCMonoReten: Currency read FqBCMonoReten write FqBCMonoReten;
    property qBCMonoRet: Currency read FqBCMonoRet write FqBCMonoRet;
  end;

  TISSQNtot = class(TObject)
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
    procedure Assign(Source: TISSQNtot);
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

  TretTrib = class(TObject)
  private
    FvRetPIS: Currency;
    FvRetCOFINS: Currency;
    FvRetCSLL: Currency;
    FvBCIRRF: Currency;
    FvIRRF: Currency;
    FvBCRetPrev: Currency;
    FvRetPrev: Currency;
  public
    procedure Assign(Source: TretTrib);
    property vRetPIS: Currency read FvRetPIS write FvRetPIS;
    property vRetCOFINS: Currency read FvRetCOFINS write FvRetCOFINS;
    property vRetCSLL: Currency read FvRetCSLL write FvRetCSLL;
    property vBCIRRF: Currency read FvBCIRRF write FvBCIRRF;
    property vIRRF: Currency read FvIRRF write FvIRRF;
    property vBCRetPrev: Currency read FvBCRetPrev write FvBCRetPrev;
    property vRetPrev: Currency read FvRetPrev write FvRetPrev;
  end;

  TCOFINSST = class(TObject)
  private
    FvBC: Currency;
    FpCOFINS: Currency;
    FqBCProd: Currency;
    FvAliqProd: Currency;
    FvCOFINS: Currency;
    FindSomaCOFINSST: TIndSomaCOFINSST;
  public
    procedure Assign(Source: TCOFINSST);
    property vBC: Currency read FvBC write FvBC;
    property pCOFINS: Currency read FpCOFINS write FpCOFINS;
    property qBCProd: Currency read FqBCProd write FqBCProd;
    property vAliqProd: Currency read FvAliqProd write FvAliqProd;
    property vCOFINS: Currency read FvCOFINS write FvCOFINS;
    property indSomaCOFINSST: TIndSomaCOFINSST read FindSomaCOFINSST write FindSomaCOFINSST;
  end;

  TISSQN = class(TObject)
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
    procedure Assign(Source: TISSQN);
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

  TICMSUFDest = class(TObject)
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
    procedure Assign(Source: TICMSUFDest);
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

  TTransp = class(TObject)
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
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TTransp);
    property modFrete: TpcnModalidadeFrete read FmodFrete write FmodFrete;
    property Transporta: TTransporta read FTransporta write FTransporta;
    property retTransp: TretTransp read FretTransp write FretTransp;
    property veicTransp: TveicTransp read FveicTransp write FveicTransp;
    property Vol: TVolCollection read FVol write SetVol;
    property Reboque: TReboqueCollection read FReboque write SetReboque;
    property vagao: String read Fvagao write Fvagao;
    property balsa: String read Fbalsa write Fbalsa;
  end;

  TTransporta = class(TObject)
  private
    FCNPJCPF: String;
    FxNome: String;
    FIE: String;
    FxEnder: String;
    FxMun: String;
    FUF: String;
  public
    procedure Assign(Source: TTransporta);
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property xNome: String read FxNome write FxNome;
    property IE: String read FIE write FIE;
    property xEnder: String read FxEnder write FxEnder;
    property xMun: String read FxMun write FxMun;
    property UF: String read FUF write FUF;
  end;

  TretTransp = class(TObject)
  private
    FvServ: Currency;
    FvBCRet: Currency;
    FpICMSRet: Currency;
    FvICMSRet: Currency;
    FCFOP: String;
    FcMunFG: Integer;
  public
    procedure Assign(Source: TretTransp);
    property vServ: Currency read FvServ write FvServ;
    property vBCRet: Currency read FvBCRet write FvBCRet;
    property pICMSRet: Currency read FpICMSRet write FpICMSRet;
    property vICMSRet: Currency read FvICMSRet write FvICMSRet;
    property CFOP: String read FCFOP write FCFOP;
    property cMunFG: Integer read FcMunFG write FcMunFG;
  end;

  TveicTransp = class(TObject)
  private
    Fplaca: String;
    FUF: String;
    FRNTC: String;
  public
    procedure Assign(Source: TveicTransp);
    property placa: String read Fplaca write Fplaca;
    property UF: String read FUF write FUF;
    property RNTC: String read FRNTC write FRNTC;
  end;

  TReboqueCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TReboqueCollectionItem;
    procedure SetItem(Index: Integer; Value: TReboqueCollectionItem);
  public
    function Add: TReboqueCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TReboqueCollectionItem;
    property Items[Index: Integer]: TReboqueCollectionItem read GetItem write SetItem; default;
  end;

  TReboqueCollectionItem = class(TObject)
  private
    Fplaca: String;
    FUF: String;
    FRNTC: String;
  public
    procedure Assign(Source: TReboqueCollectionItem);

    property placa: String read Fplaca write Fplaca;
    property UF: String read FUF write FUF;
    property RNTC: String read FRNTC write FRNTC;
  end;

  TVolCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TVolCollectionItem;
    procedure SetItem(Index: Integer; Value: TVolCollectionItem);
  public
    function Add: TVolCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TVolCollectionItem;
    property Items[Index: Integer]: TVolCollectionItem read GetItem write SetItem; default;
  end;

  TVolCollectionItem = class(TObject)
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
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TVolCollectionItem);
    property qVol: Integer read FqVol write FqVol;
    property esp: String read Fesp write Fesp;
    property marca: String read Fmarca write Fmarca;
    property nVol: String read FnVol write FnVol;
    property pesoL: Currency read FpesoL write FpesoL;
    property pesoB: Currency read FpesoB write FpesoB;
    property Lacres: TLacresCollection read FLacres write SetLacres;
  end;

  TLacresCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TLacresCollectionItem;
    procedure SetItem(Index: Integer; Value: TLacresCollectionItem);
  public
    function Add: TLacresCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TLacresCollectionItem;
    property Items[Index: Integer]: TLacresCollectionItem read GetItem write SetItem; default;
  end;

  TLacresCollectionItem = class(TObject)
  private
    FnLacre: String;
  public
    procedure Assign(Source: TLacresCollectionItem);
    property nLacre: String read FnLacre write FnLacre;
  end;

  TCobr = class(TObject)
  private
    FFat: TFat;
    FDup: TDupCollection;

    procedure SetDup(Value: TDupCollection);
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TCobr);
    property Fat: TFat read FFat write FFat;
    property Dup: TDupCollection read FDup write SetDup;
  end;

  TFat = class(TObject)
  private
    FnFat: String;
    FvOrig: Currency;
    FvDesc: Currency;
    FvLiq: Currency;
  public
    procedure Assign(Source: TFat);
    property nFat: String read FnFat write FnFat;
    property vOrig: Currency read FvOrig write FvOrig;
    property vDesc: Currency read FvDesc write FvDesc;
    property vLiq: Currency read FvLiq write FvLiq;
  end;

  TDupCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDupCollectionItem;
    procedure SetItem(Index: Integer; Value: TDupCollectionItem);
  public
    function Add: TDupCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDupCollectionItem;
    property Items[Index: Integer]: TDupCollectionItem read GetItem write SetItem; default;
  end;

  TDupCollectionItem = class(TObject)
  private
    FnDup: String;
    FdVenc: TDateTime;
    FvDup: Currency;
  public
    procedure Assign(Source: TDupCollectionItem);
    property nDup: String read FnDup write FnDup;
    property dVenc: TDateTime read FdVenc write FdVenc;
    property vDup: Currency read FvDup write FvDup;
  end;

  { TpagCollection }

  TpagCollection = class(TACBrObjectList)
  private
    FvTroco: Currency;

    function GetItem(Index: Integer): TpagCollectionItem;
    procedure SetItem(Index: Integer; Value: TpagCollectionItem);
  public
    constructor Create;
    procedure Assign(Source: TpagCollection);

    function Add: TpagCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TpagCollectionItem;

    property Items[Index: Integer]: TpagCollectionItem read GetItem write SetItem; default;
    property vTroco: Currency read FvTroco write FvTroco;
  end;

  TpagCollectionItem = class(TObject)
  private
    FtPag: TpcnFormaPagamento;
    FxPag: string;
    FvPag: Currency;
    FtpIntegra: TtpIntegra;
    FCNPJ: String;
    FtBand: TpcnBandeiraCartao;
    FcAut: String;
    FindPag: TpcnIndicadorPagamento;
    FdPag: TDateTime;
    FCNPJPag: String;
    FUFPag: String;
    FCNPJReceb: String;
    FidTermPag: String;
  public
    constructor Create;
    procedure Assign(Source: TpagCollectionItem);

    property indPag: TpcnIndicadorPagamento read FindPag write FindPag default ipNenhum;
    property tPag: TpcnFormaPagamento read FtPag write FtPag;
    property xPag: string read FxPag write FxPag;
    property vPag: Currency read FvPag write FvPag;
    property tpIntegra: TtpIntegra read FtpIntegra write FtpIntegra;
    property CNPJ: String read FCNPJ write FCNPJ;
    property tBand: TpcnBandeiraCartao read FtBand write FtBand;
    property cAut: String read FcAut write FcAut;
    property dPag: TDateTime read FdPag write FdPag;
    property CNPJPag: String read FCNPJPag write FCNPJPag;
    property UFPag: String read FUFPag write FUFPag;
    property CNPJReceb: String read FCNPJReceb write FCNPJReceb;
    property idTermPag: String read FidTermPag write FidTermPag;
  end;

  TinfIntermed = class(TObject)
  private
    FCNPJ: String;
    FidCadIntTran: String;
  public
    procedure Assign(Source: TinfIntermed);
    property CNPJ: String read FCNPJ write FCNPJ;
    property idCadIntTran: String read FidCadIntTran write FidCadIntTran;
  end;

  TInfAdic = class(TObject)
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

    procedure Assign(Source: TInfAdic);
    property infAdFisco: String read FinfAdFisco write FinfAdFisco;
    property infCpl: String read FinfCpl write FinfCpl;
    property obsCont: TobsContCollection read FobsCont write SetobsCont;
    property obsFisco: TobsFiscoCollection read FobsFisco write SetobsFisco;
    property procRef: TprocRefCollection read FprocRef write SetprocRef;
  end;

  TobsContCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TobsContCollectionItem;
    procedure SetItem(Index: Integer; Value: TobsContCollectionItem);
  public
    function Add: TobsContCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TobsContCollectionItem;
    property Items[Index: Integer]: TobsContCollectionItem read GetItem write SetItem; default;
  end;

  TobsContCollectionItem = class(TObject)
  private
    FxCampo: String;
    FxTexto: String;
  public
    procedure Assign(Source: TobsContCollectionItem);
    property xCampo: String read FxCampo write FxCampo;
    property xTexto: String read FxTexto write FxTexto;
  end;

  TobsFiscoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TobsFiscoCollectionItem;
    procedure SetItem(Index: Integer; Value: TobsFiscoCollectionItem);
  public
    function Add: TobsFiscoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TobsFiscoCollectionItem;
    property Items[Index: Integer]: TobsFiscoCollectionItem read GetItem write SetItem; default;
  end;

  TobsFiscoCollectionItem = class(TObject)
  private
    FxCampo: String;
    FxTexto: String;
  public
    procedure Assign(Source: TobsFiscoCollectionItem);
    property xCampo: String read FxCampo write FxCampo;
    property xTexto: String read FxTexto write FxTexto;
  end;

  TprocRefCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TprocRefCollectionItem;
    procedure SetItem(Index: Integer; Value: TprocRefCollectionItem);
  public
    function Add: TprocRefCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TprocRefCollectionItem;
    property Items[Index: Integer]: TprocRefCollectionItem read GetItem write SetItem; default;
  end;

  TprocRefCollectionItem = class(TObject)
  private
    FnProc: String;
    FindProc: TpcnIndicadorProcesso;
    FtpAto: TtpAto;
  public
    procedure Assign(Source: TprocRefCollectionItem);

    property nProc: String read FnProc write FnProc;
    property indProc: TpcnIndicadorProcesso read FindProc write FindProc default ipSEFAZ;
    property tpAto: TtpAto read FtpAto write FtpAto;
  end;

  TExporta = class(TObject)
  private
    FUFembarq: String;
    FxLocEmbarq: String;
    // Versao 3.10
    FUFSaidaPais: String;
    FxLocExporta: String;
    FxLocDespacho: String;
  public
    procedure Assign(Source: TExporta);
    property UFembarq: String read FUFembarq write FUFembarq;
    property xLocEmbarq: String read FxLocEmbarq write FxLocEmbarq;
    // Versao 3.10
    property UFSaidaPais: String read FUFSaidaPais write FUFSaidaPais;
    property xLocExporta: String read FxLocExporta write FxLocExporta;
    property xLocDespacho: String read FxLocDespacho write FxLocDespacho;
  end;

  TCompra = class(TObject)
  private
    FxNEmp: String;
    FxPed: String;
    FxCont: String;
  public
    procedure Assign(Source: TCompra);
    property xNEmp: String read FxNEmp write FxNEmp;
    property xPed: String read FxPed write FxPed;
    property xCont: String read FxCont write FxCont;
  end;

  TCana = class(TObject)
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
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TCana);
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

  TForDiaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TForDiaCollectionItem;
    procedure SetItem(Index: Integer; Value: TForDiaCollectionItem);
  public
    function Add: TForDiaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TForDiaCollectionItem;
    property Items[Index: Integer]: TForDiaCollectionItem read GetItem write SetItem; default;
  end;

  TForDiaCollectionItem = class(TObject)
  private
    Fdia: Integer;
    Fqtde: Currency;
  public
    procedure Assign(Source: TForDiaCollectionItem);
    property dia: Integer read Fdia write Fdia;
    property qtde: Currency read Fqtde write Fqtde;
  end;

  TDeducCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDeducCollectionItem;
    procedure SetItem(Index: Integer; Value: TDeducCollectionItem);
  public
    function Add: TDeducCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TDeducCollectionItem;
    property Items[Index: Integer]: TDeducCollectionItem read GetItem write SetItem; default;
  end;

  TDeducCollectionItem = class(TObject)
  private
    FxDed: String;
    FvDed: Currency;
  public
    procedure Assign(Source: TDeducCollectionItem);
    property xDed: String read FxDed write FxDed;
    property vDed: Currency read FvDed write FvDed;
  end;

  TautXMLCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TautXMLCollectionItem;
    procedure SetItem(Index: Integer; Value: TautXMLCollectionItem);
  public
    function Add: TautXMLCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TautXMLCollectionItem;
    property Items[Index: Integer]: TautXMLCollectionItem read GetItem write SetItem; default;
  end;

  TautXMLCollectionItem = class(TObject)
  private
    FCNPJCPF: String;
  public
    procedure Assign(Source: TautXMLCollectionItem);
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
  end;

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

const
  CMUN_EXTERIOR = 9999999;
  XMUN_EXTERIOR = 'EXTERIOR';
  UF_EXTERIOR = 'EX';

implementation

uses
  ACBrUtil.Base, pcnNFeR;

{ TNFe }

procedure TNFe.Assign(Source: TNFe);
begin
  infNFe.Assign(Source.infNFe);
  Ide.Assign(Source.Ide);
  Emit.Assign(Source.Emit);
  Avulsa.Assign(Source.Avulsa);
  Dest.Assign(Source.Dest);
  Retirada.Assign(Source.Retirada);
  Entrega.Assign(Source.Entrega);
  autXML.Assign(Source.autXML);
  Det.Assign(Source.Det);
  Total.Assign(Source.Total);
  Transp.Assign(Source.Transp);
  Cobr.Assign(Source.Cobr);
  pag.Assign(Source.pag);
  infIntermed.Assign(Source.infIntermed);
  InfAdic.Assign(Source.InfAdic);
  exporta.Assign(Source.exporta);
  compra.Assign(Source.compra);
  cana.Assign(Source.cana);
  infNFeSupl.Assign(Source.infNFeSupl);
  signature.Assign(Source.signature);
  procNFe.Assign(Source.procNFe);
  infRespTec.Assign(Source.infRespTec);
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
  FIde     := TIde.Create;
  FEmit    := TEmit.Create;
  FAvulsa  := TAvulsa.Create;
  FDest    := TDest.Create;
  FRetirada := TRetirada.Create;
  FEntrega := TEntrega.Create;
  FautXML  := TautXMLCollection.Create;
  FDet     := TDetCollection.Create;
  FTotal   := TTotal.Create;
  FCobr    := TCobr.Create;
  Fpag     := TpagCollection.Create;
  FTransp  := TTransp.Create;
  FinfIntermed := TinfIntermed.Create;
  FinfAdic := TinfAdic.Create(self);
  FExporta := TExporta.Create;
  FCompra  := TCompra.Create;
  FCana    := TCana.Create;
  FinfNFeSupl := TinfNFeSupl.Create;
  Fsignature := Tsignature.create;
  FProcNFe := TProcNFe.create;
  FinfRespTec := TinfRespTec.create;

  FinfNFe.Versao := 0;

  FEmit.EnderEmit.xPais := 'BRASIL';
  FEmit.EnderEmit.cPais := 1058;

  FDest.EnderDest.xPais := 'BRASIL';
  FDest.EnderDest.cPais := 1058;
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
  FinfIntermed.Free;
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

function TDetCollection.Add: TDetCollectionItem;
begin
  Result := Self.New;
end;

procedure TDetCollection.Assign(Source: TDetCollection);
var
  I: Integer;
begin
  Self.Clear;
  for I := 0 to Source.Count - 1 do
    Self.New.Assign(Source.Items[I]);
end;

function TDetCollection.GetItem(Index: Integer): TDetCollectionItem;
begin
  Result := TDetCollectionItem(inherited Items[Index]);
end;

procedure TDetCollection.SetItem(Index: Integer; Value: TDetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDetCollection.New: TDetCollectionItem;
begin
  Result := TDetCollectionItem.Create;
  Self.Add(Result);
end;

{ TDetCollectionItem }

procedure TDetCollectionItem.Assign(Source: TDetCollectionItem);
begin
  Prod.Assign(Source.Prod);
  Imposto.Assign(Source.Imposto);

  pDevol := Source.pDevol;
  vIPIDevol := Source.vIPIDevol;
  infAdProd := Source.infAdProd;

  obsCont.Assign(Source.obsCont);
  obsFisco.Assign(Source.obsFisco);
end;

constructor TDetCollectionItem.Create;
begin
  inherited Create;
  FProd := TProd.Create;
  FImposto := TImposto.Create;
  FobsCont := TobsItem.Create;
  FobsFisco := TobsItem.Create;
end;

destructor TDetCollectionItem.Destroy;
begin
  FProd.Free;
  FImposto.Free;
  FobsCont.Free;
  FobsFisco.Free;
  inherited;
end;

{ TIde }

procedure TIde.Assign(Source: TIde);
begin
  cUF      := Source.cUF;
  cNF      := Source.cNF;
  natOp    := Source.natOp;
  indPag   := Source.indPag;
  modelo   := Source.modelo;
  serie    := Source.serie;
  nNF      := Source.nNF;
  dEmi     := Source.dEmi;
  dSaiEnt  := Source.dSaiEnt;
  hSaiEnt  := Source.hSaiEnt;
  tpNF     := Source.tpNF;
  idDest   := Source.idDest;
  cMunFG   := Source.cMunFG;
  NFref.Assign(Source.NFref);
  tpImp    := Source.tpImp;
  tpEmis   := Source.tpEmis;
  cDV      := Source.cDV;
  tpAmb    := Source.tpAmb;
  finNFe   := Source.finNFe;
  indFinal := Source.indFinal;
  indPres  := Source.indPres;
  indIntermed := Source.indIntermed;
  procEmi  := Source.procEmi;
  verProc  := Source.verProc;
  dhCont   := Source.dhCont;
  xJust    := Source.xJust;
end;

constructor TIde.Create;
begin
  inherited Create;
  FNFref := TNFrefCollection.Create();
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

{ TNFrefCollection }

function TNFrefCollection.Add: TNFrefCollectionItem;
begin
  Result := Self.New;
end;

function TNFrefCollection.GetItem(Index: Integer): TNFrefCollectionItem;
begin
  Result := TNFrefCollectionItem(inherited Items[Index]);
end;

procedure TNFrefCollection.SetItem(Index: Integer; Value: TNFrefCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TNFrefCollection.New: TNFrefCollectionItem;
begin
  Result := TNFrefCollectionItem.Create;
  Self.Add(Result);
end;

{ TNFrefCollectionItem }

procedure TNFrefCollectionItem.Assign(Source: TNFrefCollectionItem);
begin
  refNFe := Source.refNFe;
  refNFeSig := Source.refNFeSig;
  refCTe := Source.refCTe;
  RefNF.Assign(Source.RefNF);
  RefNFP.Assign(Source.RefNFP);
  RefECF.Assign(Source.RefECF);
end;

constructor TNFrefCollectionItem.Create();
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

{ TEmit }

procedure TEmit.Assign(Source: TEmit);
begin
  CNPJCPF := Source.CNPJCPF;
  xNome   := Source.xNome;
  xFant   := Source.xFant;
  EnderEmit.Assign(Source.EnderEmit);
  IE      := Source.IE;
  IEST    := Source.IEST;
  IM      := Source.IM;
  CNAE    := Source.CNAE;
  CRT     := Source.CRT;
end;

constructor TEmit.Create();
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

{ TDest }

procedure TDest.Assign(Source: TDest);
begin
  CNPJCPF       := Source.CNPJCPF;
  idEstrangeiro := Source.idEstrangeiro;
  xNome         := Source.xNome;
  EnderDest.Assign(Source.EnderDest);
  indIEDest     := Source.indIEDest;
  IE            := Source.IE;
  ISUF          := Source.ISUF;
  IM            := Source.IM;
  Email         := Source.Email;
end;

constructor TDest.Create();
begin
  inherited Create;
  FEnderDest := TEnderDest.Create;
end;

destructor TDest.Destroy;
begin
  FEnderDest.Free;
  inherited;
end;

{ TProd }

constructor TProd.Create();
begin
  inherited Create;
  FindEscala := ieNenhum;

  FDI        := TDICollection.Create;
  FNVE       := TNVECollection.Create;
  FdetExport := TdetExportCollection.Create;
  FRastro    := TRastroCollection.Create;
  FveicProd  := TveicProd.Create;
  FMed       := TMedCollection.Create;
  Farma      := TArmaCollection.Create;
  Fcomb      := TComb.Create;
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

procedure TProd.Assign(Source: TProd);
begin
  cProd     := Source.cProd;
  nItem     := Source.nItem;
  cEAN      := Source.cEAN;
  cBarra    := Source.cBarra;
  xProd     := Source.xProd;
  NCM       := Source.NCM;
  NVE.Assign(Source.NVE);
  EXTIPI    := Source.EXTIPI;
  //genero  := Source.genero;
  CFOP      := Source.CFOP;
  uCom      := Source.uCom;
  qCom      := Source.qCom;
  vUnCom    := Source.vUnCom;
  vProd     := Source.vProd;
  cEANTrib  := Source.cEANTrib;
  cBarraTrib := Source.FcBarraTrib;
  uTrib     := Source.uTrib;
  qTrib     := Source.qTrib;
  vUnTrib   := Source.vUnTrib;
  vFrete    := Source.vFrete;
  vSeg      := Source.vSeg;
  vDesc     := Source.vDesc;
  vOutro    := Source.vOutro;
  IndTot    := Source.IndTot;
  DI.Assign(Source.DI);
  xPed      := Source.xPed;
  nItemPed  := Source.nItemPed;
  detExport.Assign(Source.detExport);
  veicProd.Assign(Source.veicProd);
  med.Assign(Source.med);
  rastro.Assign(Source.rastro);
  arma.Assign(Source.arma);
  comb.Assign(Source.comb);
  nRECOPI   := Source.nRECOPI;
  nFCI      := Source.nFCI;
  CEST      := Source.CEST;
  indEscala := Source.indEscala;
  CNPJFab   := Source.CNPJFab;
  cBenef    := Source.cBenef;
end;

procedure TProd.setCFOP(const Value: String);
begin
  FCFOP := Value;
end;

procedure TProd.SetDI(Value: TDICollection);
begin
  FDI.Assign(Value);
end;

procedure TProd.SetRastro(Value: TRastroCollection);
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

{ TRastroCollection }

function TRastroCollection.GetItem(Index: Integer): TRastroCollectionItem;
begin
  Result := TRastroCollectionItem(inherited Items[Index]);
end;

procedure TRastroCollection.SetItem(Index: Integer; Value: TRastroCollectionItem
  );
begin
  inherited Items[Index] := Value;
end;

function TRastroCollection.Add: TRastroCollectionItem;
begin
  Result := Self.New;
end;

function TRastroCollection.New: TRastroCollectionItem;
begin
  Result := TRastroCollectionItem.Create;
  Self.Add(Result);
end;

{ TMedCollection }

function TMedCollection.Add: TMedCollectionItem;
begin
  Result := Self.New;
end;

function TMedCollection.GetItem(Index: Integer): TMedCollectionItem;
begin
  Result := TMedCollectionItem(inherited Items[Index]);
end;

procedure TMedCollection.SetItem(Index: Integer; Value: TMedCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TMedCollection.New: TMedCollectionItem;
begin
  Result := TmedCollectionItem.Create;
  Self.Add(Result);
end;

{ TArmaCollection }

function TArmaCollection.Add: TArmaCollectionItem;
begin
  Result := Self.New;
end;

function TArmaCollection.GetItem(Index: Integer): TArmaCollectionItem;
begin
  Result := TArmaCollectionItem(inherited Items[Index]);
end;

procedure TArmaCollection.SetItem(Index: Integer; Value: TArmaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TArmaCollection.New: TArmaCollectionItem;
begin
  Result := TarmaCollectionItem.Create;
  Self.Add(Result);
end;

{ Tcomb }

procedure TComb.Assign(Source: TComb);
begin
  cProdANP := Source.cProdANP;
  descANP  := Source.descANP;
  pGLP     := Source.pGLP;
  pGNn     := Source.pGNn;
  pGNi     := Source.pGNi;
  vPart    := Source.vPart;
  pMixGN   := Source.pMixGN;
  CODIF    := Source.CODIF;
  qTemp    := Source.qTemp;
  UFcons   := Source.UFcons;
  CIDE.Assign(Source.CIDE);
  ICMS.Assign(Source.ICMS);
  ICMSInter.Assign(Source.ICMSInter);
  ICMSCons.Assign(Source.ICMSCons);
  encerrante.Assign(Source.encerrante);

  pBio    := Source.pBio;
  origComb.Assign(Source.origComb);
end;

constructor TComb.Create();
begin
  inherited Create;

  FCIDE := TCIDE.Create;
  FICMS := TICMSComb.Create;
  FICMSInter  := TICMSInter.Create;
  FICMScons   := TICMScons.Create;
  Fencerrante := Tencerrante.Create;
  origComb := TorigCombCollection.Create;
end;

destructor TComb.Destroy;
begin
  FCIDE.Free;
  FICMS.Free;
  FICMSInter.Free;
  FICMScons.Free;
  Fencerrante.Free;
  origComb.Free;

  inherited;
end;

procedure TComb.SetorigComb(const Value: TorigCombCollection);
begin
  ForigComb := Value;
end;

{ TDICollection }

function TDICollection.Add: TDICollectionItem;
begin
  Result := Self.New;
end;

function TDICollection.GetItem(Index: Integer): TDICollectionItem;
begin
  Result := TDICollectionItem(inherited Items[Index]);
end;

procedure TDICollection.SetItem(Index: Integer; Value: TDICollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDICollection.New: TDICollectionItem;
begin
  Result := TDICollectionItem.Create;
  Self.Add(Result);
end;

{ TDICollectionItem }

procedure TDICollectionItem.Assign(Source: TDICollectionItem);
begin
  nDi          := Source.nDi;
  dDi          := Source.dDi;
  xLocDesemb   := Source.xLocDesemb;
  UFDesemb     := Source.UFDesemb;
  dDesemb      := Source.dDesemb;
  tpViaTransp  := Source.tpViaTransp;
  vAFRMM       := Source.vAFRMM;
  tpIntermedio := Source.tpIntermedio;
  CNPJ         := Source.CNPJ;
  UFTerceiro   := Source.UFTerceiro;
  cExportador  := Source.cExportador;
  adi.Assign(Source.adi);
end;

constructor TDICollectionItem.Create();
begin
  inherited;
  FAdi := TadiCollection.Create;
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

{ TAdiCollection }

function TAdiCollection.Add: TAdiCollectionItem;
begin
  Result := Self.New;
end;

function TAdiCollection.GetItem(Index: Integer): TAdiCollectionItem;
begin
  Result := TAdiCollectionItem(inherited Items[Index]);
end;

procedure TAdiCollection.SetItem(Index: Integer; Value: TAdiCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TAdiCollection.New: TAdiCollectionItem;
begin
  Result := TAdiCollectionItem.Create;
  Self.Add(Result);
end;

{ TImposto }

procedure TImposto.Assign(Source: TImposto);
begin
  vTotTrib := Source.vTotTrib;
  ICMS.Assign(Source.ICMS);
  IPI.Assign(Source.IPI);
  II.Assign(Source.II);
  PIS.Assign(Source.PIS);
  PISST.Assign(Source.PISST);
  COFINS.Assign(Source.COFINS);
  COFINSST.Assign(Source.COFINSST);
  ISSQN.Assign(Source.ISSQN);
  ICMSUFDest.Assign(Source.ICMSUFDest);
end;

constructor TImposto.Create();
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

{ TTotal }

procedure TTotal.Assign(Source: TTotal);
begin
  ICMSTot.Assign(Source.ICMSTot);
  ISSQNtot.Assign(Source.ISSQNtot);
  retTrib.Assign(Source.retTrib);
end;

constructor TTotal.Create();
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

{ TTransp }

constructor TTransp.Create();
begin
  inherited Create;
  FTransporta := TTransporta.Create;
  FretTransp  := TretTransp.Create;
  FveicTransp := TveicTransp.Create;
  FVol        := TVolCollection.Create;
  Freboque    := TreboqueCollection.Create;
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

{ TVolCollection }

function TVolCollection.Add: TVolCollectionItem;
begin
  Result := Self.New;
end;

function TVolCollection.GetItem(Index: Integer): TVolCollectionItem;
begin
  Result := TVolCollectionItem(inherited Items[Index]);
end;

procedure TVolCollection.SetItem(Index: Integer; Value: TVolCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TVolCollection.New: TVolCollectionItem;
begin
  Result := TVolCollectionItem.Create;
  Self.Add(Result);
end;

{ TVolCollectionItem }

procedure TVolCollectionItem.Assign(Source: TVolCollectionItem);
begin
  qVol  := Source.qVol;
  esp   := Source.esp;
  marca := Source.marca;
  nVol  := Source.nVol;
  pesoL := Source.pesoL;
  pesoB := Source.pesoB;
  Lacres.Assign(Source.Lacres);
end;

constructor TVolCollectionItem.Create();
begin
  inherited Create;
  FLacres := TLacresCollection.Create;
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

{ TLacresCollection }

function TLacresCollection.Add: TLacresCollectionItem;
begin
  Result := Self.New;
end;

function TLacresCollection.GetItem(Index: Integer): TLacresCollectionItem;
begin
  Result := TLacresCollectionItem(inherited Items[Index]);
end;

procedure TLacresCollection.SetItem(Index: Integer; Value: TLacresCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TLacresCollection.New: TLacresCollectionItem;
begin
  Result := TLacresCollectionItem.Create;
  Self.Add(Result);
end;

{ TretTransp }

procedure TretTransp.Assign(Source: TretTransp);
begin
  vServ    := Source.vServ;
  vBCRet   := Source.vBCRet;
  pICMSRet := Source.pICMSRet;
  vICMSRet := Source.vICMSRet;
  CFOP     := Source.CFOP;
  cMunFG   := Source.cMunFG;
end;

{ TreboqueCollection }

function TreboqueCollection.Add: TreboqueCollectionItem;
begin
  Result := Self.New;
end;

function TreboqueCollection.GetItem(Index: Integer): TreboqueCollectionItem;
begin
  Result := TreboqueCollectionItem(inherited Items[Index]);
end;

procedure TreboqueCollection.SetItem(Index: Integer; Value: TreboqueCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TReboqueCollection.New: TReboqueCollectionItem;
begin
  Result := TreboqueCollectionItem.Create;
  Self.Add(Result);
end;

{ TCobr }

procedure TCobr.Assign(Source: TCobr);
begin
  Fat.Assign(Source.Fat);
  Dup.Assign(Source.Dup);
end;

constructor TCobr.Create();
begin
  inherited Create;
  FFat := TFat.Create;
  FDup := TDupCollection.Create;
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

function TDupCollection.Add: TDupCollectionItem;
begin
  Result := Self.New;
end;

function TDupCollection.GetItem(Index: Integer): TDupCollectionItem;
begin
  Result := TDupCollectionItem(inherited Items[Index]);
end;

procedure TDupCollection.SetItem(Index: Integer; Value: TDupCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDupCollection.New: TDupCollectionItem;
begin
  Result := TDupCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfAdic }

procedure TInfAdic.Assign(Source: TInfAdic);
begin
  infAdFisco := Source.infAdFisco;
  infCpl     := Source.infCpl;
  obsCont.Assign(Source.obsCont);
  obsFisco.Assign(Source.obsFisco);
  procRef.Assign(Source.procRef);
end;

constructor TinfAdic.Create(AOwner: TNFe);
begin
  inherited Create;
  FobsCont  := TobsContCollection.Create;
  FobsFisco := TobsFiscoCollection.Create;
  FprocRef  := TprocRefCollection.Create;
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

{ TobsContCollection }

function TobsContCollection.Add: TobsContCollectionItem;
begin
  Result := Self.New;
end;

function TobsContCollection.GetItem(Index: Integer): TobsContCollectionItem;
begin
  Result := TobsContCollectionItem(inherited Items[Index]);
end;

procedure TobsContCollection.SetItem(Index: Integer; Value: TobsContCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TobsContCollection.New: TobsContCollectionItem;
begin
  Result := TobsContCollectionItem.Create;
  Self.Add(Result);
end;

{ TobsFiscoCollection }

function TobsFiscoCollection.Add: TobsFiscoCollectionItem;
begin
  Result := Self.New;
end;

function TobsFiscoCollection.GetItem(Index: Integer): TobsFiscoCollectionItem;
begin
  Result := TobsFiscoCollectionItem(inherited Items[Index]);
end;

procedure TobsFiscoCollection.SetItem(Index: Integer; Value: TobsFiscoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TobsFiscoCollection.New: TobsFiscoCollectionItem;
begin
  Result := TobsFiscoCollectionItem.Create;
  Self.Add(Result);
end;

{ TprocRefCollection }

function TprocRefCollection.Add: TprocRefCollectionItem;
begin
  Result := Self.New;
end;

function TprocRefCollection.GetItem(Index: Integer): TprocRefCollectionItem;
begin
  Result := TprocRefCollectionItem(inherited Items[Index]);
end;

procedure TprocRefCollection.SetItem(Index: Integer; Value: TprocRefCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TprocRefCollection.New: TprocRefCollectionItem;
begin
  Result := TprocRefCollectionItem.Create;
  Self.Add(Result);
end;

{ TCana }

procedure TCana.Assign(Source: TCana);
begin
  safra   := Source.safra;
  ref     := Source.ref;
  fordia.Assign(Source.fordia);
  qTotMes := Source.qTotMes;
  qTotAnt := Source.qTotAnt;
  qTotGer := Source.qTotGer;
  deduc.Assign(Source.deduc);
  vFor    := Source.vFor;
  vTotDed := Source.vTotDed;
  vLiqFor := Source.vLiqFor;
end;

constructor TCana.Create();
begin
  inherited Create;
  Ffordia := TForDiaCollection.Create;
  Fdeduc  := TDeducCollection.Create;
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
  Result := Self.New;
end;

function TForDiaCollection.GetItem(Index: Integer): TForDiaCollectionItem;
begin
  Result := TForDiaCollectionItem(inherited Items[Index]);
end;

procedure TForDiaCollection.SetItem(Index: Integer;
  Value: TForDiaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TForDiaCollection.New: TForDiaCollectionItem;
begin
  Result := TForDiaCollectionItem.Create;
  Self.Add(Result);
end;

{ TDeducCollection }

function TDeducCollection.Add: TDeducCollectionItem;
begin
  Result := Self.New;
end;

function TDeducCollection.GetItem(Index: Integer): TDeducCollectionItem;
begin
  Result := TDeducCollectionItem(inherited Items[Index]);
end;

procedure TDeducCollection.SetItem(Index: Integer;
  Value: TDeducCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDeducCollection.New: TDeducCollectionItem;
begin
  Result := TDeducCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfNFe }

procedure TinfNFe.Assign(Source: TinfNFe);
begin
  ID     := Source.ID;
  Versao := Source.Versao;
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

procedure TveicProd.Assign(Source: TveicProd);
begin
  tpOP         := Source.tpOP;
  chassi       := Source.chassi;
  cCor         := Source.cCor;
  xCor         := Source.xCor;
  pot          := Source.pot;
  Cilin        := Source.Cilin;
  pesoL        := Source.pesoL;
  pesoB        := Source.pesoB;
  nSerie       := Source.nSerie;
  tpComb       := Source.tpComb;
  nMotor       := Source.nMotor;
  CMT          := Source.CMT;
  dist         := Source.dist;
  //RENAVAM    := Source.RENAVAM;
  anoMod       := Source.anoMod;
  anoFab       := Source.anoFab;
  tpPint       := Source.tpPint;
  tpVeic       := Source.tpVeic;
  espVeic      := Source.espVeic;
  VIN          := Source.VIN;
  condVeic     := Source.condVeic;
  cMod         := Source.cMod;
  cCorDENATRAN := Source.cCorDENATRAN;
  lota         := Source.lota;
  tpRest       := Source.tpRest;
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
  Result := Self.New;
end;

procedure TpagCollection.Assign(Source: TpagCollection);
var
  I: Integer;
begin
  vTroco := Source.vTroco;

  Self.Clear;
  for I := 0 to Source.Count - 1 do
    Self.New.Assign(Source.Items[I]);
end;

constructor TpagCollection.Create;
begin
  inherited Create;
  vTroco := 0;
end;

function TpagCollection.GetItem(Index: Integer): TpagCollectionItem;
begin
  Result := TpagCollectionItem(inherited Items[Index]);
end;

procedure TpagCollection.SetItem(Index: Integer;
  Value: TpagCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TpagCollection.New: TpagCollectionItem;
begin
  Result := TpagCollectionItem.Create;
  Self.Add(Result);
end;

{ TautXMLCollection }

function TautXMLCollection.Add: TautXMLCollectionItem;
begin
  Result := Self.New;
end;

function TautXMLCollection.GetItem(Index: Integer): TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem(inherited Items[Index]);
end;

procedure TautXMLCollection.SetItem(Index: Integer;
  Value: TautXMLCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TautXMLCollection.New: TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem.Create;
  Self.Add(Result);
end;

{ TautXMLCollectionItem }

procedure TautXMLCollectionItem.Assign(Source: TautXMLCollectionItem);
begin
  CNPJCPF := Source.CNPJCPF;
end;

{ TdetExportCollection }

function TdetExportCollection.Add: TdetExportCollectionItem;
begin
  Result := Self.New;
end;

function TdetExportCollection.GetItem(
  Index: Integer): TdetExportCollectionItem;
begin
  Result := TdetExportCollectionItem(inherited Items[Index]);
end;

procedure TdetExportCollection.SetItem(Index: Integer;
  Value: TdetExportCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TdetExportCollection.New: TdetExportCollectionItem;
begin
  Result := TdetExportCollectionItem.Create;
  Self.Add(Result);
end;

{ TNVECollection }

function TNVECollection.Add: TNVECollectionItem;
begin
  Result := Self.New;
end;

function TNVECollection.GetItem(Index: Integer): TNVECollectionItem;
begin
  Result := TNVECollectionItem(inherited Items[Index]);
end;

procedure TNVECollection.SetItem(Index: Integer; Value: TNVECollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TNVECollection.New: TNVECollectionItem;
begin
  Result := TNVECollectionItem.Create;
  Self.Add(Result);
end;

{ TRefNF }

procedure TRefNF.Assign(Source: TRefNF);
begin
  cUF    := Source.cUF;
  AAMM   := Source.AAMM;
  CNPJ   := Source.CNPJ;
  modelo := Source.modelo;
  serie  := Source.serie;
  nNF    := Source.nNF;
end;

{ TRefNFP }

procedure TRefNFP.Assign(Source: TRefNFP);
begin
  cUF     := Source.cUF;
  AAMM    := Source.AAMM;
  CNPJCPF := Source.CNPJCPF;
  IE      := Source.IE;
  modelo  := Source.modelo;
  serie   := Source.serie;
  nNF     := Source.nNF;
end;

{ TRefECF }

procedure TRefECF.Assign(Source: TRefECF);
begin
  modelo := Source.modelo;
  nECF   := Source.nECF;
  nCOO   := Source.nCOO;
end;

{ TenderEmit }

procedure TenderEmit.Assign(Source: TenderEmit);
begin
  xLgr    := Source.xLgr;
  nro     := Source.nro;
  xCpl    := Source.xCpl;
  xBairro := Source.xBairro;
  cMun    := Source.cMun;
  xMun    := Source.xMun;
  UF      := Source.UF;
  CEP     := Source.CEP;
  cPais   := Source.cPais;
  xPais   := Source.xPais;
  fone    := Source.fone;
end;

{ TAvulsa }

procedure TAvulsa.Assign(Source: TAvulsa);
begin
  CNPJ    := Source.CNPJ;
  xOrgao  := Source.xOrgao;
  matr    := Source.matr;
  xAgente := Source.xAgente;
  fone    := Source.fone;
  UF      := Source.UF;
  nDAR    := Source.nDAR;
  dEmi    := Source.dEmi;
  vDAR    := Source.vDAR;
  repEmi  := Source.repEmi;
  dPag    := Source.dPag;
end;

{ TEnderDest }

procedure TEnderDest.Assign(Source: TEnderDest);
begin
  xLgr    := Source.xLgr;
  nro     := Source.nro;
  xCpl    := Source.xCpl;
  xBairro := Source.xBairro;
  cMun    := Source.cMun;
  xMun    := Source.xMun;
  UF      := Source.UF;
  CEP     := Source.CEP;
  cPais   := Source.cPais;
  xPais   := Source.xPais;
  fone    := Source.fone;
end;

{ TRetirada }

procedure TRetirada.Assign(Source: TRetirada);
begin
  CNPJCPF := Source.CNPJCPF;
  xNome   := Source.xNome;
  xLgr    := Source.xLgr;
  nro     := Source.nro;
  xCpl    := Source.xCpl;
  xBairro := Source.xBairro;
  cMun    := Source.cMun;
  xMun    := Source.xMun;
  UF      := Source.UF;
  CEP     := Source.CEP;
  cPais   := Source.cPais;
  xPais   := Source.xPais;
  fone    := Source.fone;
  email   := Source.email;
  IE      := Source.IE;
end;

{ TEntrega }

procedure TEntrega.Assign(Source: TEntrega);
begin
  CNPJCPF := Source.CNPJCPF;
  xNome   := Source.xNome;
  xLgr    := Source.xLgr;
  nro     := Source.nro;
  xCpl    := Source.xCpl;
  xBairro := Source.xBairro;
  cMun    := Source.cMun;
  xMun    := Source.xMun;
  UF      := Source.UF;
  CEP     := Source.CEP;
  cPais   := Source.cPais;
  xPais   := Source.xPais;
  fone    := Source.fone;
  email   := Source.email;
  IE      := Source.IE;
end;

{ TrastroCollectionItem }

procedure TRastroCollectionItem.Assign(Source: TRastroCollectionItem);
begin
  nLote  := Source.nLote;
  qLote  := Source.qLote;
  dFab   := Source.dFab;
  dVal   := Source.dVal;
  cAgreg := Source.cAgreg;
end;

{ TMedCollectionItem }

procedure TMedCollectionItem.Assign(Source: TMedCollectionItem);
begin
  cProdANVISA    := Source.cProdANVISA;
  xMotivoIsencao := Source.xMotivoIsencao;
  nLote          := Source.nLote;
  qLote          := Source.qLote;
  dFab           := Source.dFab;
  dVal           := Source.dVal;
  vPMC           := Source.vPMC;
end;

{ TArmaCollectionItem }

procedure TArmaCollectionItem.Assign(Source: TArmaCollectionItem);
begin
  tpArma := Source.tpArma;
  nSerie := Source.nSerie;
  nCano  := Source.nCano;
  descr  := Source.descr;
end;

{ TCIDE }

procedure TCIDE.Assign(Source: TCIDE);
begin
  qBCProd   := Source.qBCProd;
  vAliqProd := Source.vAliqProd;
  vCIDE     := Source.vCIDE;
end;

{ TICMSComb }

procedure TICMSComb.Assign(Source: TICMSComb);
begin
  vBCICMS   := Source.vBCICMS;
  vICMS     := Source.vICMS;
  vBCICMSST := Source.vBCICMSST;
  vICMSST   := Source.vICMSST;
end;

{ TICMSInter }

procedure TICMSInter.Assign(Source: TICMSInter);
begin
  vBCICMSSTDest := Source.vBCICMSSTDest;
  vICMSSTDest   := Source.vICMSSTDest;
end;

{ TICMSCons }

procedure TICMSCons.Assign(Source: TICMSCons);
begin
  vBCICMSSTCons := Source.vBCICMSSTCons;
  vICMSSTCons   := Source.vICMSSTCons;
  UFcons        := Source.UFcons;
end;

{ TAdiCollectionItem }

procedure TAdiCollectionItem.Assign(Source: TAdiCollectionItem);
begin
  nAdicao     := Source.nAdicao;
  nSeqAdi     := Source.nSeqAdi;
  cFabricante := Source.cFabricante;
  vDescDI     := Source.vDescDI;
  nDraw       := Source.nDraw;
end;

{ TNVECollectionItem }

procedure TNVECollectionItem.Assign(Source: TNVECollectionItem);
begin
  NVE := Source.NVE;
end;

{ TdetExportCollectionItem }

procedure TdetExportCollectionItem.Assign(Source: TdetExportCollectionItem);
begin
  nDraw   := Source.nDraw;
  nRE     := Source.nRE;
  chNFe   := Source.chNFe;
  qExport := Source.qExport;
end;

{ TICMS }

procedure TICMS.Assign(Source: TICMS);
begin
  orig        := Source.orig;
  CST         := Source.CST;
  CSOSN       := Source.CSOSN;
  modBC       := Source.modBC;
  pRedBC      := Source.pRedBC;
  vBC         := Source.vBC;
  pICMS       := Source.pICMS;
  vICMS       := Source.vICMS;
  modBCST     := Source.modBCST;
  pMVAST      := Source.pMVAST;
  pRedBCST    := Source.pRedBCST;
  vBCST       := Source.vBCST;
  pICMSST     := Source.pICMSST;
  vICMSST     := Source.vICMSST;
  UFST        := Source.UFST;
  pBCOp       := Source.pBCOp;
  vBCSTRet    := Source.vBCSTRet;
  vICMSSTRet  := Source.vICMSSTRet;
  motDesICMS  := Source.motDesICMS;
  pCredSN     := Source.pCredSN;
  vCredICMSSN := Source.vCredICMSSN;
  vBCSTDest   := Source.vBCSTDest;
  vICMSSTDest := Source.vICMSSTDest;
  vICMSDeson  := Source.vICMSDeson;
  vICMSOp     := Source.vICMSOp;
  pDif        := Source.pDif;
  vICMSDif    := Source.vICMSDif;
  vBCFCP      := Source.vBCFCP;
  pFCP        := Source.pFCP;
  vFCP        := Source.vFCP;
  vBCFCPST    := Source.vBCFCPST;
  pFCPST      := Source.pFCPST;
  vFCPST      := Source.vFCPST;
  vBCFCPSTRet := Source.vBCFCPSTRet;
  pFCPSTRet   := Source.pFCPSTRet;
  vFCPSTRet   := Source.vFCPSTRet;
  pST         := Source.pST;
  pRedBCEfet  := Source.pRedBCEfet;
  vBCEfet     := Source.vBCEfet;
  pICMSEfet   := Source.pICMSEfet;
  vICMSEfet   := Source.vICMSEfet;
  vICMSSubstituto := Source.vICMSSubstituto;
  vICMSSTDeson := Source.vICMSSTDeson;
  motDesICMSST := Source.motDesICMSST;
  pFCPDif  := Source.pFCPDif;
  vFCPDif  := Source.vFCPDif;
  vFCPEfet := Source.vFCPEfet;
  adRemICMS := Source.adRemICMS;
  vICMSMono := Source.vICMSMono;
  adRemICMSReten := Source.adRemICMSReten;
  vICMSMonoReten := Source.vICMSMonoReten;
//  adRemICMSDif := Source.adRemICMSDif;
  vICMSMonoDif := Source.vICMSMonoDif;
  adRemICMSRet := Source.adRemICMSRet;
  vICMSMonoRet := Source.vICMSMonoRet;
  qBCMono := Source.qBCMono;
  qBCMonoReten := Source.qBCMonoReten;
  pRedAdRem := Source.pRedAdRem;
  motRedAdRem := Source.motRedAdRem;
  FvICMSMonoOp := Source.FvICMSMonoOp;
  qBCMonoRet := Source.qBCMonoRet;
  indDeduzDeson := Source.indDeduzDeson;
end;

constructor TICMS.Create;
begin
  inherited Create;
  FindDeduzDeson := tiNao;
end;

{ TIPI }

procedure TIPI.Assign(Source: TIPI);
begin
  clEnq    := Source.clEnq;
  CNPJProd := Source.CNPJProd;
  cSelo    := Source.cSelo;
  qSelo    := Source.qSelo;
  cEnq     := Source.cEnq;
  CST      := Source.CST;
  vBC      := Source.vBC;
  qUnid    := Source.qUnid;
  vUnid    := Source.vUnid;
  pIPI     := Source.pIPI;
  vIPI     := Source.vIPI;
end;

{ TII }

procedure TII.Assign(Source: TII);
begin
  vBc      := Source.vBc;
  vDespAdu := Source.vDespAdu;
  vII      := Source.vII;
  vIOF     := Source.vIOF;
end;

{ TPIS }

procedure TPIS.Assign(Source: TPIS);
begin
  CST       := Source.CST;
  vBC       := Source.vBC;
  pPIS      := Source.pPIS;
  vPIS      := Source.vPIS;
  qBCProd   := Source.qBCProd;
  vAliqProd := Source.vAliqProd;
end;

{ TPISST }

procedure TPISST.Assign(Source: TPISST);
begin
  vBc       := Source.vBc;
  pPis      := Source.pPis;
  qBCProd   := Source.qBCProd;
  vAliqProd := Source.vAliqProd;
  vPIS      := Source.vPIS;
  indSomaPISST := Source.indSomaPISST;
end;

{ TCOFINS }

procedure TCOFINS.Assign(Source: TCOFINS);
begin
  CST       := Source.CST;
  vBC       := Source.vBC;
  pCOFINS   := Source.pCOFINS;
  vCOFINS   := Source.vCOFINS;
  vBCProd   := Source.vBCProd;
  vAliqProd := Source.vAliqProd;
  qBCProd   := Source.qBCProd;
end;

{ TICMSTot }

procedure TICMSTot.Assign(Source: TICMSTot);
begin
  vBC          := Source.vBC;
  vICMS        := Source.vICMS;
  vICMSDeson   := Source.vICMSDeson;
  vFCPUFDest   := Source.vFCPUFDest;
  vICMSUFDest  := Source.vICMSUFDest;
  vICMSUFRemet := Source.vICMSUFRemet;
  vBCST        := Source.vBCST;
  vST          := Source.vST;
  vProd        := Source.vProd;
  vFrete       := Source.vFrete;
  vSeg         := Source.vSeg;
  vDesc        := Source.vDesc;
  vII          := Source.vII;
  vIPI         := Source.vIPI;
  vIPIDevol    := Source.vIPIDevol;
  vPIS         := Source.vPIS;
  vCOFINS      := Source.vCOFINS;
  vOutro       := Source.vOutro;
  vNF          := Source.vNF;
  vTotTrib     := Source.vTotTrib;
  vFCP         := Source.vFCP;
  vFCPST       := Source.vFCPST;
  vFCPSTRet    := Source.vFCPSTRet;

  vICMSMono := Source.vICMSMono;
  vICMSMonoReten := Source.vICMSMonoReten;
  vICMSMonoRet := Source.vICMSMonoRet;
  qBCMono := Source.qBCMono;
  qBCMonoReten := Source.qBCMonoReten;
  qBCMonoRet := Source.qBCMonoRet;
end;

{ TISSQNtot }

procedure TISSQNtot.Assign(Source: TISSQNtot);
begin
  vServ          := Source.vServ;
  vBC            := Source.vBC;
  vISS           := Source.vISS;
  vPIS           := Source.vPIS;
  vCOFINS        := Source.vCOFINS;
  dCompet        := Source.dCompet;
  vDeducao       := Source.vDeducao;
//  vINSS        := Source.vINSS;
//  vIR          := Source.vIR;
//  vCSLL        := Source.vCSLL;
  vOutro         := Source.vOutro;
  vDescIncond    := Source.vDescIncond;
  vDescCond      := Source.vDescCond;
//  indISSRet    := Source.indISSRet;
//  indISS       := Source.indISS;
//  cServico     := Source.cServico;
//  cMun         := Source.cMun;
//  cPais        := Source.cPais;
//  nProcesso    := Source.nProcesso;
  vISSRet        := Source.vISSRet;
  cRegTrib       := Source.cRegTrib;
//  indIncentivo := Source.indIncentivo;
end;

{ TretTrib }

procedure TretTrib.Assign(Source: TretTrib);
begin
  vRetPIS    := Source.vRetPIS;
  vRetCOFINS := Source.vRetCOFINS;
  vRetCSLL   := Source.vRetCSLL;
  vBCIRRF    := Source.vBCIRRF;
  vIRRF      := Source.vIRRF;
  vBCRetPrev := Source.vBCRetPrev;
  vRetPrev   := Source.vRetPrev;
end;

{ TCOFINSST }

procedure TCOFINSST.Assign(Source: TCOFINSST);
begin
  vBC       := Source.vBC;
  pCOFINS   := Source.pCOFINS;
  qBCProd   := Source.qBCProd;
  vAliqProd := Source.vAliqProd;
  vCOFINS   := Source.vCOFINS;
  indSomaCOFINSST := Source.indSomaCOFINSST;
end;

{ TISSQN }

procedure TISSQN.Assign(Source: TISSQN);
begin
  vBC          := Source.vBC;
  vAliq        := Source.vAliq;
  vISSQN       := Source.vISSQN;
  cMunFG       := Source.cMunFG;
  cListServ    := Source.cListServ;
  cSitTrib     := Source.cSitTrib;
  vDeducao     := Source.vDeducao;
  vOutro       := Source.vOutro;
  vDescIncond  := Source.vDescIncond;
  vDescCond    := Source.vDescCond;
  indISSRet    := Source.indISSRet;
  vISSRet      := Source.vISSRet;
  indISS       := Source.indISS;
  cServico     := Source.cServico;
  cMun         := Source.cMun;
  cPais        := Source.cPais;
  nProcesso    := Source.nProcesso;
  indIncentivo := Source.indIncentivo;
end;

procedure TTransp.Assign(Source: TTransp);
begin
  modFrete := Source.modFrete;
  Transporta.Assign(Source.Transporta);
  retTransp.Assign(Source.retTransp);
  veicTransp.Assign(Source.veicTransp);
  Vol.Assign(Source.Vol);
  Reboque.Assign(Source.Reboque);
  vagao  := Source.vagao;
  balsa  := Source.balsa;
end;

{ TTransporta }

procedure TTransporta.Assign(Source: TTransporta);
begin
  CNPJCPF := Source.CNPJCPF;
  xNome   := Source.xNome;
  IE      := Source.IE;
  xEnder  := Source.xEnder;
  xMun    := Source.xMun;
  UF      := Source.UF;
end;

{ TveicTransp }

procedure TveicTransp.Assign(Source: TveicTransp);
begin
  placa := Source.placa;
  UF    := Source.UF;
  RNTC  := Source.RNTC;
end;

{ TReboqueCollectionItem }

procedure TReboqueCollectionItem.Assign(Source: TReboqueCollectionItem);
begin
  placa := Source.placa;
  UF    := Source.UF;
  RNTC  := Source.RNTC;
end;

{ TLacresCollectionItem }

procedure TLacresCollectionItem.Assign(Source: TLacresCollectionItem);
begin
  nLacre := Source.nLacre;
end;

{ TFat }

procedure TFat.Assign(Source: TFat);
begin
  nFat  := Source.nFat;
  vOrig := Source.vOrig;
  vDesc := Source.vDesc;
  vLiq  := Source.vLiq;
end;

{ TDupCollectionItem }

procedure TDupCollectionItem.Assign(Source: TDupCollectionItem);
begin
  nDup  := Source.nDup;
  dVenc := Source.dVenc;
  vDup  := Source.vDup;
end;

{ TpagCollectionItem }

procedure TpagCollectionItem.Assign(Source: TpagCollectionItem);
begin
  indPag    := Source.indPag;
  tPag      := Source.tPag;
  xPag      := Source.xPag;
  vPag      := Source.vPag;
  tpIntegra := Source.tpIntegra;
  CNPJ      := Source.CNPJ;
  tBand     := Source.tBand;
  cAut      := Source.cAut;
  dPag      := Source.dPag;
  CNPJPag   := Source.CNPJPag;
  UFPag     := Source.UFPag;
  CNPJReceb := Source.CNPJReceb;
  idTermPag := Source.idTermPag;
end;

constructor TpagCollectionItem.Create;
begin
  inherited Create;
  FindPag := ipNenhum;
  FtBand := TpcnBandeiraCartao(-1);
end;

{ TobsContCollectionItem }

procedure TobsContCollectionItem.Assign(Source: TobsContCollectionItem);
begin
  xCampo := Source.xCampo;
  xTexto := Source.xTexto;
end;

{ TobsFiscoCollectionItem }

procedure TobsFiscoCollectionItem.Assign(Source: TobsFiscoCollectionItem);
begin
  xCampo := Source.xCampo;
  xTexto := Source.xTexto;
end;

{ TprocRefCollectionItem }

procedure TprocRefCollectionItem.Assign(Source: TprocRefCollectionItem);
begin
  nProc := Source.nProc;
  indProc := Source.indProc;
  tpAto := Source.tpAto;
end;

{ TExporta }

procedure TExporta.Assign(Source: TExporta);
begin
  UFembarq     := Source.UFembarq;
  xLocEmbarq   := Source.xLocEmbarq;
  // Versao 3.10
  UFSaidaPais  := Source.UFSaidaPais;
  xLocExporta  := Source.xLocExporta;
  xLocDespacho := Source.xLocDespacho;
end;

{ TCompra }

procedure TCompra.Assign(Source: TCompra);
begin
  xNEmp := Source.xNEmp;
  xPed  := Source.xPed;
  xCont := Source.xCont;
end;

{ TForDiaCollectionItem }

procedure TForDiaCollectionItem.Assign(Source: TForDiaCollectionItem);
begin
  dia  := Source.dia;
  qtde := Source.qtde;
end;

{ TDeducCollectionItem }

procedure TDeducCollectionItem.Assign(Source: TDeducCollectionItem);
begin
  xDed := Source.xDed;
  vDed := Source.vDed;
end;

{ Tencerrante }

procedure Tencerrante.Assign(Source: Tencerrante);
begin
  nBico   := Source.nBico;
  nBomba  := Source.nBomba;
  nTanque := Source.nTanque;
  vEncIni := Source.vEncIni;
  vEncFin := Source.vEncFin;
end;

{ TinfNFeSupl }

procedure TinfNFeSupl.Assign(Source: TinfNFeSupl);
begin
  qrCode   := Source.qrCode;
  urlChave := Source.urlChave;
end;

{ TICMSUFDest }

procedure TICMSUFDest.Assign(Source: TICMSUFDest);
begin
  vBCUFDest      := Source.vBCUFDest;
  vBCFCPUFDest   := Source.vBCFCPUFDest;
  pFCPUFDest     := Source.pFCPUFDest;
  pICMSUFDest    := Source.pICMSUFDest;
  pICMSInter     := Source.pICMSInter;
  pICMSInterPart := Source.pICMSInterPart;
  vFCPUFDest     := Source.vFCPUFDest;
  vICMSUFDest    := Source.vICMSUFDest;
  vICMSUFRemet   := Source.vICMSUFRemet;
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

{ TinfIntermed }

procedure TinfIntermed.Assign(Source: TinfIntermed);
begin
  CNPJ     := Source.CNPJ;
  idCadIntTran := Source.idCadIntTran;
end;

{ TobsItem }

procedure TobsItem.Assign(Source: TobsItem);
begin
  xCampo := Source.xCampo;
  xTexto := Source.xTexto;
end;

{ TorigCombCollection }

function TorigCombCollection.Add: TorigCombCollectionItem;
begin
  Result := Self.New;
end;

function TorigCombCollection.GetItem(Index: Integer): TorigCombCollectionItem;
begin
  Result := TorigCombCollectionItem(inherited Items[Index]);
end;

function TorigCombCollection.New: TorigCombCollectionItem;
begin
  Result := TorigCombCollectionItem.Create;
  Self.Add(Result);
end;

procedure TorigCombCollection.SetItem(Index: Integer;
  Value: TorigCombCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TorigCombCollectionItem }

procedure TorigCombCollectionItem.Assign(Source: TorigCombCollectionItem);
begin
  indImport := Source.indImport;
  cUFOrig := Source.cUFOrig;
  pOrig := Source.pOrig;
end;

end.
