{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Fork do projeto: PCN - Projeto Cooperar NFe                                  }
{                  Coordenação: (c) 2009 - Paulo Casagrande                    }
{                  http://projetocooperar.googlecode.com/svn/trunk/            }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

{$I ACBr.inc}

unit ACBrNFe.Classes;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrDFe.Conversao,
  pcnConversao,
  pcnConversaoNFe,
  pcnSignature, pcnProcNFe, pcnGerador,
  ACBrBase;

type
  { TinfNFe }

  TinfNFe = class(TObject)
  private
    FID: string;
    FVersao: Double;
  public
    procedure Assign(Source: TinfNFe);
    property ID: string read FID write FID;
    property Versao: Double read FVersao write FVersao;
  end;

  { TRefNF }

  TRefNF = class(TObject)
  private
    FcUF: Integer;
    FAAMM: string;
    FCNPJ: string;
    Fmodelo: Integer;
    Fserie: Integer;
    FnNF: Integer;
  public
    procedure Assign(Source: TRefNF);
    property cUF: Integer read FcUF write FcUF;
    property AAMM: string read FAAMM write FAAMM;
    property CNPJ: string read FCNPJ write FCNPJ;
    property modelo: Integer read FModelo write Fmodelo;
    property serie: Integer read FSerie write Fserie;
    property nNF: Integer read FnNF write FnNF;
  end;

  { TRefECF }

  TRefECF = class(TObject)
  private
    Fmodelo: TpcnECFModRef;
    FnECF: string;
    FnCOO: string;
  public
    procedure Assign(Source: TRefECF);
    property modelo:TpcnECFModRef read FModelo write Fmodelo default ECFModRefVazio;
    property nECF: string read FnECF write FnECF;
    property nCOO: string read FnCOO write FnCOO;
  end;

  { TRefNFP }

  TRefNFP = class(TObject)
  private
    FcUF: Integer;
    FAAMM: string;
    FCNPJCPF: string;
    FIE: string;
    Fmodelo: string;
    Fserie: Integer;
    FnNF: Integer;
  public
    procedure Assign(Source: TRefNFP);
    property cUF: Integer read FcUF write FcUF;
    property AAMM: string read FAAMM write FAAMM;
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property IE: string read FIE write FIE;
    property modelo: string read FModelo write Fmodelo;
    property serie: Integer read FSerie write Fserie;
    property nNF: Integer read FnNF write FnNF;
  end;

  { TNFrefCollectionItem }

  TNFrefCollectionItem = class(TObject)
  private
    FrefNFe: string;
    FrefNFeSig: string;
    FrefCTe: string;
    FRefNF: TRefNF;
    FRefECF: TRefECF;
    FRefNFP: TRefNFP;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TNFrefCollectionItem);
    property refNFe: string read FrefNFe write FrefNFe;
    property refNFeSig: string read FrefNFeSig write FrefNFeSig;
    property refCTe: string read FrefCTe write FrefCTe;
    property RefNF: TRefNF read FRefNF write FRefNF;
    property RefECF: TRefECF read FRefECF write FRefECF;
    property RefNFP: TRefNFP read FRefNFP write FRefNFP;
  end;

  { TNFrefCollection }

  TNFrefCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TNFrefCollectionItem;
    procedure SetItem(Index: Integer; Value: TNFrefCollectionItem);
  public
    function Add: TNFrefCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TNFrefCollectionItem;
    property Items[Index: Integer]: TNFrefCollectionItem read GetItem write SetItem; default;
  end;

  // Reforma Tributária
  { TgCompraGov }

  TgCompraGov = class(TObject)
  private
    FtpEnteGov: TtpEnteGov;
    FpRedutor: Double;
    FtpOperGov: TtpOperGov;
  public
    procedure Assign(Source: TgCompraGov);

    property tpEnteGov: TtpEnteGov read FtpEnteGov write FtpEnteGov;
    property pRedutor: Double read FpRedutor write FpRedutor;
    property tpOperGov: TtpOperGov read FtpOperGov write FtpOperGov;
  end;

  { TgPagAntecipadoCollectionItem }

  TgPagAntecipadoCollectionItem = class(TObject)
  private
    FrefNFe: string;
  public
    procedure Assign(Source: TgPagAntecipadoCollectionItem);

    property refNFe: string read FrefNFe write FrefNFe;
  end;

  { TgPagAntecipadoCollection }

  TgPagAntecipadoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TgPagAntecipadoCollectionItem;
    procedure SetItem(Index: Integer; Value: TgPagAntecipadoCollectionItem);
  public
    function Add: TgPagAntecipadoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TgPagAntecipadoCollectionItem;
    property Items[Index: Integer]: TgPagAntecipadoCollectionItem read GetItem write SetItem; default;
  end;

  { TIde }

  TIde = class(TObject)
  private
    FcUF: Integer;
    FcNF: Integer;
    FnatOp: string;
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
    FverProc: string;
    FdhCont : TDateTime;
    FxJust  : string;
    FcMunFGIBS: Integer;
    FtpNFDebito: TtpNFDebito;
    FtpNFCredito: TtpNFCredito;
    FgCompraGov: TgCompraGov;
    FgPagAntecipado: TgPagAntecipadoCollection;

    procedure SetNFref(Value: TNFrefCollection);
    procedure SetgPagAntecipado(const Value: TgPagAntecipadoCollection);
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TIde);

    property cUF: Integer read FcUF write FcUF;
    property cNF: Integer read FcNF write FcNF;
    property natOp: string read FnatOp write FnatOp;
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
    property verProc: string read FverProc write FverProc;
    property dhCont: TDateTime read FdhCont write FdhCont;
    property xJust: string read FxJust write FxJust;
    // Reforma Tributária
    property cMunFGIBS: Integer read FcMunFGIBS write FcMunFGIBS;
    property tpNFDebito: TtpNFDebito read FtpNFDebito write FtpNFDebito;
    property tpNFCredito: TtpNFCredito read FtpNFCredito write FtpNFCredito;
    property gCompraGov: TgCompraGov read FgCompraGov write FgCompraGov;
    property gPagAntecipado: TgPagAntecipadoCollection read FgPagAntecipado write SetgPagAntecipado;
  end;

  { TenderEmit }

  TenderEmit = class(TObject)
  private
    FxLgr: string;
    Fnro: string;
    fxCpl: string;
    FxBairro: string;
    FcMun: Integer;
    FxMun: string;
    FUF: string;
    FCEP: Integer;
    FcPais: Integer;
    FxPais: string;
    Ffone: string;
  public
    procedure Assign(Source: TenderEmit);
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property UF: string read FUF write FUF;
    property CEP: Integer read FCEP write FCEP;
    property cPais: Integer read FcPais write FcPais;
    property xPais: string read FxPais write FxPais;
    property fone: string read Ffone write Ffone;
  end;

  { TEmit }

  TEmit = class(TObject)
  private
    FCNPJCPF: string;
    FxNome: string;
    FxFant: string;
    FenderEmit: TenderEmit;
    FIE: string;
    FIEST: string;
    FIM: string;
    FCNAE: string;
    FCRT: TpcnCRT;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TEmit);
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property xNome: string read FxNome write FxNome;
    property xFant: string read FxFant write FxFant;
    property EnderEmit: TEnderEmit read FEnderEmit write FEnderEmit;
    property IE: string read FIE write FIE;
    property IEST: string read FIEST write FIEST;
    property IM: string read FIM write FIM;
    property CNAE: string read FCNAE write FCNAE;
    property CRT: TpcnCRT read FCRT write FCRT;
  end;

  { TAvulsa }

  TAvulsa = class(TObject)
  private
    FCNPJ: string;
    FxOrgao: string;
    Fmatr: string;
    FxAgente: string;
    Ffone: string;
    FUF: string;
    FnDAR: string;
    FdEmi: TDateTime;
    FvDAR: Currency;
    FrepEmi: string;
    FdPag: TDateTime;
  public
    procedure Assign(Source: TAvulsa);
    property CNPJ: string read FCNPJ write FCNPJ;
    property xOrgao: string read FxOrgao write FxOrgao;
    property matr: string read Fmatr write Fmatr;
    property xAgente: string read FxAgente write FxAgente;
    property fone: string read Ffone write Ffone;
    property UF: string read FUF write FUF;
    property nDAR: string read FnDAR write FnDAR;
    property dEmi: TDateTime read FdEmi write FdEmi;
    property vDAR: Currency read FvDAR write FvDAR;
    property repEmi: string read FrepEmi write FrepEmi;
    property dPag: TDateTime read FdPag write FdPag;
  end;

  { TEnderDest }

  TEnderDest = class(TObject)
  private
    FxLgr: string;
    Fnro: string;
    fxCpl: string;
    FxBairro: string;
    FcMun: Integer;
    FxMun: string;
    FUF: string;
    FCEP: Integer;
    FcPais: Integer;
    FxPais: string;
    Ffone: string;
  public
    procedure Assign(Source: TEnderDest);
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property UF: string read FUF write FUF;
    property CEP: Integer read FCEP write FCEP;
    property cPais: Integer read FcPais write FcPais;
    property xPais: string read FxPais write FxPais;
    property fone: string read Ffone write Ffone;
  end;

  { TDest }

  TDest = class(TObject)
  private
    FCNPJCPF: string;
    FidEstrangeiro: string;
    FxNome: string;
    FEnderDest: TEnderDest;
    FindIEDest: TpcnindIEDest;
    FIE: string;
    FISUF: string;
    FIM: string;
    Femail: string;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TDest);
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property idEstrangeiro: string read FidEstrangeiro write FidEstrangeiro;
    property xNome: string read FxNome write FxNome;
    property EnderDest: TEnderDest read FEnderDest write FEnderDest;
    property indIEDest: TpcnindIEDest read FindIEDest write FindIEDest;
    property IE: string read FIE write FIE;
    property ISUF: string read FISUF write FISUF;
    property IM: string read FIM write FIM;
    property Email: string read Femail write Femail;
  end;

  { TRetirada }

  TRetirada = class(TObject)
  private
    FCNPJCPF: string;
    FxNome: string;
    FxLgr: string;
    Fnro: string;
    fxCpl: string;
    FxBairro: string;
    FcMun: Integer;
    FxMun: string;
    FUF: string;
    FCEP: Integer;
    FcPais: Integer;
    FxPais: string;
    Ffone: string;
    Femail: string;
    FIE: string;
  public
    procedure Assign(Source: TRetirada);
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property xNome: string read FxNome write FxNome;
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property UF: string read FUF write FUF;
    property CEP: Integer read FCEP write FCEP;
    property cPais: Integer read FcPais write FcPais;
    property xPais: string read FxPais write FxPais;
    property fone: string read Ffone write Ffone;
    property Email: string read Femail write Femail;
    property IE: string read FIE write FIE;
  end;

  { TEntrega }

  TEntrega = class(TObject)
  private
    FCNPJCPF: string;
    FxNome: string;
    FxLgr: string;
    Fnro: string;
    fxCpl: string;
    FxBairro: string;
    FcMun: Integer;
    FxMun: string;
    FUF: string;
    FCEP: Integer;
    FcPais: Integer;
    FxPais: string;
    Ffone: string;
    Femail: string;
    FIE: string;
  public
    procedure Assign(Source: TEntrega);
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property xNome: string read FxNome write FxNome;
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property UF: string read FUF write FUF;
    property CEP: Integer read FCEP write FCEP;
    property cPais: Integer read FcPais write FcPais;
    property xPais: string read FxPais write FxPais;
    property fone: string read Ffone write Ffone;
    property Email: string read Femail write Femail;
    property IE: string read FIE write FIE;
  end;

  { TAdiCollectionItem }

  TAdiCollectionItem = class(TObject)
  private
    FnAdicao: Integer;
    FnSeqAdi: Integer;
    FcFabricante: string;
    FvDescDI: Currency;
    FnDraw: string;
  public
    procedure Assign(Source: TAdiCollectionItem);
    property nAdicao: Integer read FnAdicao write FnAdicao;
    property nSeqAdi: Integer read FnSeqAdi write FnSeqAdi;
    property cFabricante: string read FcFabricante write FcFabricante;
    property vDescDI: Currency read FvDescDI write FvDescDI;
    property nDraw: string read FnDraw write FnDraw;
  end;

  { TAdiCollection }

  TAdiCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TAdiCollectionItem;
    procedure SetItem(Index: Integer; Value: TAdiCollectionItem);
  public
    function Add: TAdiCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TAdiCollectionItem;
    property Items[Index: Integer]: TAdiCollectionItem read GetItem write SetItem; default;
  end;

  { TDICollectionItem }

  TDICollectionItem = class(TObject)
  private
    FnDi: string;
    FdDi: TDateTime;
    FxLocDesemb: string;
    FUFDesemb: string;
    FdDesemb: TDateTime;
    FtpViaTransp: TpcnTipoViaTransp;
    FvAFRMM: Currency;
    FtpIntermedio: TpcnTipoIntermedio;
    FCNPJ: string;
    FUFTerceiro: string;
    FcExportador: string;
    Fadi: TAdiCollection;

    procedure SetAdi(Value: TAdiCollection);
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TDICollectionItem);
    property nDi: string read FnDi write FnDi;
    property dDi: TDateTime read FdDi write FdDi;
    property xLocDesemb: string read FxLocDesemb write FxLocDesemb;
    property UFDesemb: string read FUFDesemb write FUFDesemb;
    property dDesemb: TDateTime read FdDesemb write FdDesemb;
    property tpViaTransp: TpcnTipoViaTransp read FtpViaTransp write FtpViaTransp;
    property vAFRMM: Currency read FvAFRMM write FvAFRMM;
    property tpIntermedio: TpcnTipoIntermedio read FtpIntermedio write FtpIntermedio;
    property CNPJ: string read FCNPJ write FCNPJ;
    property UFTerceiro: string read FUFTerceiro write FUFTerceiro;
    property cExportador: string read FcExportador write FcExportador;
    property adi: TAdiCollection read Fadi write SetAdi;
  end;

  { TDICollection }

  TDICollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDICollectionItem;
    procedure SetItem(Index: Integer; Value: TDICollectionItem);
  public
    function Add: TDICollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDICollectionItem;
    property Items[Index: Integer]: TDICollectionItem read GetItem write SetItem; default;
  end;

  { TdetExportCollectionItem }

  TdetExportCollectionItem = class(TObject)
  private
    FnDraw: string;
    FnRE: string;
    FchNFe: string;
    FqExport: Currency;
  public
    procedure Assign(Source: TdetExportCollectionItem);
    property nDraw: string read FnDraw write FnDraw;
    property nRE: string read FnRE write FnRE;
    property chNFe: string read FchNFe write FchNFe;
    property qExport: Currency read FqExport write FqExport;
  end;

  { TdetExportCollection }

  TdetExportCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdetExportCollectionItem;
    procedure SetItem(Index: Integer; Value: TdetExportCollectionItem);
  public
    function Add: TdetExportCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TdetExportCollectionItem;
    property Items[Index: Integer]: TdetExportCollectionItem read GetItem write SetItem; default;
  end;

  { TRastroCollectionItem }

  TRastroCollectionItem = class(TObject)
  private
    FnLote: string;
    FqLote: Currency;
    FdFab: TDateTime;
    FdVal: TDateTime;
    FcAgreg: string;
  public
    procedure Assign(Source: TRastroCollectionItem);
    property nLote: string read FnLote write FnLote;
    property qLote: Currency read FqLote write FqLote;
    property dFab: TDateTime read FdFab write FdFab;
    property dVal: TDateTime read FdVal write FdVal;
    property cAgreg: string read FcAgreg write FcAgreg;
  end;

  { TRastroCollection }

  TRastroCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRastroCollectionItem;
    procedure SetItem(Index: Integer; Value: TRastroCollectionItem);
  public
    function Add: TRastroCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRastroCollectionItem;
    property Items[Index: Integer]: TRastroCollectionItem read GetItem write SetItem; default;
  end;

  { TveicProd }

  TveicProd = class(TObject)
  private
    FtpOP: TpcnTipoOperacao;
    Fchassi: string;
    FcCor: string;
    FxCor: string;
    Fpot: string;
    FCilin: string;
    FpesoL: string;
    FpesoB: string;
    FnSerie: string;
    FtpComb: string;
    FnMotor: string;
    FCMT: string;
    Fdist: string;
    FanoMod: Integer;
    FanoFab: Integer;
    FtpPint: string;
    FtpVeic: Integer;
    FespVeic: Integer;
    FVIN: string;
    FcondVeic: TpcnCondicaoVeiculo;
    FcMod: string;
    FcCorDENATRAN: string;
    Flota: Integer;
    FtpRest: Integer;

    function getCombDescricao: string;
  public
    procedure Assign(Source: TveicProd);
    property tpOP: TpcnTipoOperacao read FtpOP write FtpOP;
    property chassi: string read Fchassi write Fchassi;
    property cCor: string read FcCor write FcCor;
    property xCor: string read FxCor write FxCor;
    property pot: string read Fpot write Fpot;
    property Cilin: string read FCilin write FCilin;
    property pesoL: string read FpesoL write FpesoL;
    property pesoB: string read FpesoB write FpesoB;
    property nSerie: string read FnSerie write FnSerie;
    property tpComb: string read FtpComb write FtpComb;
    property CombDescricao: string read getCombDescricao;
    property nMotor: string read FnMotor write FnMotor;
    property CMT: string read FCMT write FCMT;
    property dist: string read Fdist write Fdist;
    property anoMod: Integer read FanoMod write FanoMod;
    property anoFab: Integer read FanoFab write FanoFab;
    property tpPint: string read FtpPint write FtpPint;
    property tpVeic: Integer read FtpVeic write FtpVeic;
    property espVeic: Integer read FespVeic write FespVeic;
    property VIN: string read FVIN write FVIN;
    property condVeic: TpcnCondicaoVeiculo read FcondVeic write FcondVeic;
    property cMod: string read FcMod write FcMod;
    property cCorDENATRAN: string read FcCorDENATRAN write FcCorDENATRAN;
    property lota: Integer read Flota write Flota;
    property tpRest: Integer read FtpRest write FtpRest;
  end;

  { TMedCollectionItem }

  TMedCollectionItem = class(TObject)
  private
    FcProdANVISA: string;
    FxMotivoIsencao: string;
    FnLote: string;
    FqLote: Currency;
    FdFab: TDateTime;
    FdVal: TDateTime;
    FvPMC: Currency;
  public
    procedure Assign(Source: TMedCollectionItem);
    property cProdANVISA: string read FcProdANVISA write FcProdANVISA;
    property xMotivoIsencao: string read FxMotivoIsencao write FxMotivoIsencao;
    property nLote: string read FnLote write FnLote;
    property qLote: Currency read FqLote write FqLote;
    property dFab: TDateTime read FdFab write FdFab;
    property dVal: TDateTime read FdVal write FdVal;
    property vPMC: Currency read FvPMC write FvPMC;
  end;

  { TMedCollection }

  TMedCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TMedCollectionItem;
    procedure SetItem(Index: Integer; Value: TMedCollectionItem);
  public
    function Add: TMedCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TMedCollectionItem;
    property Items[Index: Integer]: TMedCollectionItem read GetItem write SetItem; default;
  end;

  { TArmaCollectionItem }

  TArmaCollectionItem = class(TObject)
  private
    FtpArma: TpcnTipoArma;
    FnSerie: string;
    FnCano: string;
    Fdescr: string;
  public
    procedure Assign(Source: TArmaCollectionItem);
    property tpArma: TpcnTipoArma read FtpArma write FtpArma default taUsoPermitido;
    property nSerie: string read FnSerie write FnSerie;
    property nCano: string read FnCano write FnCano;
    property descr: string read Fdescr write Fdescr;
  end;

  { TArmaCollection }

  TArmaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TArmaCollectionItem;
    procedure SetItem(Index: Integer; Value: TArmaCollectionItem);
  public
    function Add: TArmaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TArmaCollectionItem;
    property Items[Index: Integer]: TArmaCollectionItem read GetItem write SetItem; default;
  end;

  { TCIDE }

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

  { TICMSComb }

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

  { TICMSInter }

  TICMSInter = class(TObject)
  private
    FvBCICMSSTDest: Currency;
    FvICMSSTDest: Currency;
  public
    procedure Assign(Source: TICMSInter);
    property vBCICMSSTDest: Currency read FvBCICMSSTDest write FvBCICMSSTDest;
    property vICMSSTDest: Currency read FvICMSSTDest write FvICMSSTDest;
  end;

  { TICMSCons }

  TICMSCons = class(TObject)
  private
    FvBCICMSSTCons: Currency;
    FvICMSSTCons: Currency;
    FUFcons: string;
  public
    procedure Assign(Source: TICMSCons);
    property vBCICMSSTCons: Currency read FvBCICMSSTCons write FvBCICMSSTCons;
    property vICMSSTCons: Currency read FvICMSSTCons write FvICMSSTCons;
    property UFcons: string read FUFcons write FUFcons;
  end;

  { Tencerrante }

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

  { TorigCombCollectionItem }

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

  { TorigCombCollection }

  TorigCombCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TorigCombCollectionItem;
    procedure SetItem(Index: Integer; Value: TorigCombCollectionItem);
  public
    function Add: TorigCombCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TorigCombCollectionItem;
    property Items[Index: Integer]: TorigCombCollectionItem read GetItem write SetItem; default;
  end;

  { TComb }

  TComb = class(TObject)
  private
    FcProdANP: Integer;
    FpMixGN: Currency;
    FdescANP: string;
    FpGLP: Currency;
    FpGNn: Currency;
    FpGNi: Currency;
    FvPart: Currency;
    FCODIF: string;
    FqTemp: Currency;
    FUFcons: string;
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
    property descANP: string read FdescANP write FdescANP;
    property pGLP: Currency read FpGLP write FpGLP;
    property pGNn: Currency read FpGNn write FpGNn;
    property pGNi: Currency read FpGNi write FpGNi;
    property vPart: Currency read FvPart write FvPart;
    property CODIF: string read FCODIF write FCODIF;
    property qTemp: Currency read FqTemp write FqTemp;
    property UFcons: string read FUFcons write FUFcons;
    property CIDE: TCIDE read FCIDE write FCIDE;
    property ICMS: TICMSComb read FICMS write FICMS;
    property ICMSInter: TICMSInter read FICMSInter write FICMSInter;
    property ICMSCons: TICMSCons read FICMSCons write FICMSCons;
    property encerrante: Tencerrante read Fencerrante write Fencerrante;
    property pBio: Currency read FpBio write FpBio;
    property origComb: TorigCombCollection read ForigComb write SetorigComb;
  end;

  { TNVECollectionItem }

  TNVECollectionItem = class(TObject)
  private
    FNve: string;
  public
    procedure Assign(Source: TNVECollectionItem);
    property NVE: string read FNve write FNve;
  end;

  { TNVECollection }

  TNVECollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TNVECollectionItem;
    procedure SetItem(Index: Integer; Value: TNVECollectionItem);
  public
    function Add: TNVECollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TNVECollectionItem;
    property Items[Index: Integer]: TNVECollectionItem read GetItem write SetItem; default;
  end;

  { TCredPresumidoCollectionItem }

  TCredPresumidoCollectionItem = class(TObject)
  private
    FcCredPresumido: string;
    FpCredPresumido: Double;
    FvCredPresumido: Double;
  public
    procedure Assign(Source: TCredPresumidoCollectionItem);

    property cCredPresumido: string read FcCredPresumido write FcCredPresumido;
    property pCredPresumido: Double read FpCredPresumido write FpCredPresumido;
    property vCredPresumido: Double read FvCredPresumido write FvCredPresumido;
  end;

  { TCredPresumidoCollection }

  TCredPresumidoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TCredPresumidoCollectionItem;
    procedure SetItem(Index: Integer; Value: TCredPresumidoCollectionItem);
  public
    function Add: TCredPresumidoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TCredPresumidoCollectionItem;
    property Items[Index: Integer]: TCredPresumidoCollectionItem read GetItem write SetItem; default;
  end;

  { TProd }

  TProd = class(TObject)
  private
    FcProd: string;
    FnItem: Integer;
    FcEAN: string;
    FxProd: string;
    FNCM: string;
    FEXTIPI: string;
    //Fgenero: Integer;
    FCFOP: string;
    FuCom: string;
    FqCom: Currency;
    FvUnCom: Double;
    FvProd: Currency;
    FcEANTrib: string;
    FuTrib: string;
    FqTrib: Currency;
    FvUnTrib: Double;
    FvFrete: Currency;
    FvSeg: Currency;
    FvDesc: Currency;
    FvOutro: Currency;
    FIndTot: TpcnIndicadorTotal;
    FDI: TDICollection;
    FxPed: string;
    FnItemPed: string;
    FdetExport: TdetExportCollection;
    FRastro: TRastroCollection;
    FveicProd: TveicProd;
    Fmed: TMedCollection;
    Farma: TarmaCollection;
    Fcomb: TComb;
    FnRECOPI: string;
    FnFCI: string;
    FNVE: TNVECollection;
    FCEST: string;
    FindEscala: TpcnIndEscala;
    FCNPJFab: string;
    FcBenef: string;
    FcBarra: string;
    FcBarraTrib: string;
    FCredPresumido: TCredPresumidoCollection;
    FindBemMovelUsado: TIndicadorEx;

    procedure SetDI(Value: TDICollection);
    procedure SetRastro(Value: TRastroCollection);
    procedure SetMed(Value: TmedCollection);
    procedure SetArma(Value: TarmaCollection);
    procedure SetdetExport(const Value: TdetExportCollection);
    procedure SetNVE(Value : TNVeCollection);
    procedure setCFOP(const Value: string);
    procedure SetCredPresumido(const Value: TCredPresumidoCollection);
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TProd);

    property cProd: string read FcProd write FcProd;
    property nItem: Integer read FnItem write FnItem;
    property cEAN: string read FcEAN write FcEAN;
    property cBarra: string read FcBarra write FcBarra;
    property xProd: string read FxProd write FxProd;
    property NCM: string read FNCM write FNCM;
    property NVE : TNVECollection read FNVE write SetNVE; //FNVE;
    property EXTIPI: string read FEXTIPI write FEXTIPI;
    //property genero: Integer read Fgenero write Fgenero;
    property CFOP: string read FCFOP write setCFOP;
    property uCom: string read FuCom write FuCom;
    property qCom: Currency read FqCom write FqCom;
    property vUnCom: Double read FvUnCom write FvUnCom;
    property vProd: Currency read FvProd write FvProd;
    property cEANTrib: string read FcEANTrib write FcEANTrib;
    property cBarraTrib: string read FcBarraTrib write FcBarraTrib;
    property uTrib: string read FuTrib write FuTrib;
    property qTrib: Currency read FqTrib write FqTrib;
    property vUnTrib: Double read FvUnTrib write FvUnTrib;
    property vFrete: Currency read FvFrete write FvFrete;
    property vSeg: Currency read FvSeg write FvSeg;
    property vDesc: Currency read FvDesc write FvDesc;
    property vOutro: Currency read FvOutro write FvOutro;
    property IndTot: TpcnIndicadorTotal read FIndTot write FIndTot default itSomaTotalNFe;
    property DI: TDICollection read FDI write SetDI;
    property xPed: string read FxPed write FxPed;
    property nItemPed : string read FnItemPed write FnItemPed;
    property detExport: TdetExportCollection read FdetExport write SetdetExport;
    property rastro: TRastroCollection read FRastro write SetRastro;
    property veicProd: TveicProd read FveicProd write FveicProd;
    property med: TMedCollection read Fmed write SetMed;
    property arma: TarmaCollection read Farma write SetArma;
    property comb: TComb read Fcomb write Fcomb;
    property nRECOPI: string read FnRECOPI write FnRECOPI;
    property nFCI: string read FnFCI write FnFCI;
    property CEST: string read FCEST write FCEST;
    property indEscala: TpcnIndEscala read FindEscala write FindEscala default ieNenhum;
    property CNPJFab: string read FCNPJFab write FCNPJFab;
    property cBenef: string read FcBenef write FcBenef;
    property CredPresumido: TCredPresumidoCollection read FCredPresumido write SetCredPresumido;
    property indBemMovelUsado: TIndicadorEx read FindBemMovelUsado write FindBemMovelUsado default tieNenhum;
  end;

  { TICMS }

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
    FUFST: string;                        //N24
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
    FvICMSMonoDif: Currency;
    FadRemICMSRet: Currency;
    FvICMSMonoRet: Currency;
    FqBCMono: Currency;
    FqBCMonoReten: Currency;
    FpRedAdRem: Currency;
    FmotRedAdRem: TmotRedAdRem;
    FvICMSMonoOp: Currency;
    FqBCMonoRet: Currency;
    FindDeduzDeson: TIndicadorEx;
    FcBenefRBC: string;
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
    property UFST: string read FUFST write FUFST;
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
    property vICMSMonoOp: Currency read FvICMSMonoOp write FvICMSMonoOp;
    property vICMSMonoDif: Currency read FvICMSMonoDif write FvICMSMonoDif;
    // CST 61
    property adRemICMSRet: Currency read FadRemICMSRet write FadRemICMSRet;
    property vICMSMonoRet: Currency read FvICMSMonoRet write FvICMSMonoRet;
    property qBCMonoRet: Currency read FqBCMonoRet write FqBCMonoRet;

    property indDeduzDeson: TIndicadorEx read FindDeduzDeson write FindDeduzDeson default tieNenhum;
    property cBenefRBC: string read FcBenefRBC write FcBenefRBC;
  end;

  { TIPI }

  TIPI = class(TObject)
  private
    FclEnq: string;
    FCNPJProd: string;
    FcSelo: string;
    FqSelo: Integer;
    FcEnq: string;
    FCST: TpcnCstIpi;
    FvBC: Currency;
    FqUnid: Currency;
    FvUnid: Currency;
    FpIPI: Currency;
    FvIPI: Currency;
  public
    procedure Assign(Source: TIPI);
    property clEnq: string read FclEnq write FclEnq;
    property CNPJProd: string read FCNPJProd write FCNPJProd;
    property cSelo: string read FcSelo write FcSelo;
    property qSelo: Integer read FqSelo write FqSelo;
    property cEnq: string read FcEnq write FcEnq;
    property CST: TpcnCstIpi read FCST write FCST default ipi00;
    property vBC: Currency read FvBC write FvBC;
    property qUnid: Currency read FqUnid write FqUnid;
    property vUnid: Currency read FvUnid write FvUnid;
    property pIPI: Currency read FpIPI write FpIPI;
    property vIPI: Currency read FvIPI write FvIPI;
  end;

  { TII }

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

  { TPIS }

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

  { TPISST }

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

  { TCOFINS }

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

  { TCOFINSST }

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

  { TISSQN }

  TISSQN = class(TObject)
  private
    FvBC: Currency;
    FvAliq: Currency;
    FvISSQN: Currency;
    FcMunFG: Integer;
    FcListServ: string;
    FcSitTrib: TpcnISSQNcSitTrib;
    FvDeducao: Currency;
    FvOutro: Currency;
    FvDescIncond: Currency;
    FvDescCond: Currency;
    FindISSRet: TpcnindISSRet;
    FvISSRet: Currency;
    FindISS: TpcnindISS;
    FcServico: string;
    FcMun: Integer;
    FcPais: Integer;
    FnProcesso: string;
    FindIncentivo: TpcnindIncentivo;
  public
    procedure Assign(Source: TISSQN);
    property vBC: Currency read FvBC write FvBC;
    property vAliq: Currency read FvAliq write FvAliq;
    property vISSQN: Currency read FvISSQN write FvISSQN;
    property cMunFG: Integer read FcMunFG write FcMunFG;
    property cListServ: string read FcListServ write FcListServ;
    property cSitTrib: TpcnISSQNcSitTrib read FcSitTrib write FcSitTrib default ISSQNcSitTribVazio;
    property vDeducao: Currency read FvDeducao write FvDeducao;
    property vOutro: Currency read FvOutro write FvOutro;
    property vDescIncond: Currency read FvDescIncond write FvDescIncond;
    property vDescCond: Currency read FvDescCond write FvDescCond;
    property indISSRet: TpcnindISSRet read FindISSRet write FindISSRet;
    property vISSRet: Currency read FvISSRet write FvISSRet;
    property indISS: TpcnindISS read FindISS write FindISS;
    property cServico: string read FcServico write FcServico;
    property cMun: Integer read FcMun write FcMun;
    property cPais: Integer read FcPais write FcPais;
    property nProcesso: string read FnProcesso write FnProcesso;
    property indIncentivo: TpcnindIncentivo read FindIncentivo write FindIncentivo;
  end;

  { TICMSUFDest }

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

  { TgIS }

  TgIS = class(TObject)
  private
    FCSTIS: TCSTIS;
    FcClassTribIS: TcClassTribIS;
    FvBCIS: Double;
    FpIS: Double;
    FpISEspec: Double;
    FuTrib: string;
    FqTrib: Double;
    FvIS: Double;
  public
    procedure Assign(Source: TgIS);

    property CSTIS: TCSTIS read FCSTIS write FCSTIS;
    property cClassTribIS: TcClassTribIS read FcClassTribIS write FcClassTribIS;
    property vBCIS: Double read FvBCIS write FvBCIS;
    property pIS: Double read FpIS write FpIS;
    property pISEspec: Double read FpISEspec write FpISEspec;
    property uTrib: string read FuTrib write FuTrib;
    property qTrib: Double read FqTrib write FqTrib;
    property vIS: Double read FvIS write FvIS;
  end;

  { TgDif }

  TgDif = class(TObject)
  private
    FpDif: Double;
    FvDif: Double;
  public
    procedure Assign(Source: TgDif);
    property pDif: Double read FpDif write FpDif;
    property vDif: Double read FvDif write FvDif;
  end;

  { TgDevTrib }

  TgDevTrib = class(TObject)
  private
    FvDevTrib: Double;
  public
    procedure Assign(Source: TgDevTrib);
    property vDevTrib: Double read FvDevTrib write FvDevTrib;
  end;

  { TgRed }

  TgRed = class(TObject)
  private
    FpRedAliq: Double;
    FpAliqEfet: Double;
  public
    procedure Assign(Source: TgRed);
    property pRedAliq: Double read FpRedAliq write FpRedAliq;
    property pAliqEfet: Double read FpAliqEfet write FpAliqEfet;
  end;

  { TgIBSUF }

  TgIBSUF = class(TObject)
  private
    FpIBSUF: Double;
    FgDif: TgDif;
    FgDevTrib: TgDevTrib;
    FgRed: TgRed;
    FvIBSUF: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgIBSUF);

    property pIBSUF: Double read FpIBSUF write FpIBSUF;
    property gDif: TgDif read FgDif write FgDif;
    property gDevTrib: TgDevTrib read FgDevTrib write FgDevTrib;
    property gRed: TgRed read FgRed write FgRed;
    property vIBSUF: Double read FvIBSUF write FvIBSUF;
  end;

  { TgIBSMun }

  TgIBSMun = class(TObject)
  private
    FpIBSMun: Double;
    FgDif: TgDif;
    FgDevTrib: TgDevTrib;
    FgRed: TgRed;
    FvIBSMun: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgIBSMun);

    property pIBSMun: Double read FpIBSMun write FpIBSMun;
    property gDif: TgDif read FgDif write FgDif;
    property gDevTrib: TgDevTrib read FgDevTrib write FgDevTrib;
    property gRed: TgRed read FgRed write FgRed;
    property vIBSMun: Double read FvIBSMun write FvIBSMun;
  end;

  { TgIBSCBSCredPres }

  TgIBSCBSCredPres = class(TObject)
  private
    FcCredPres: TcCredPres;
    FpCredPres: Double;
    FvCredPres: Double;
    FvCredPresCondSus: Double;
  public
    procedure Assign(Source: TgIBSCBSCredPres);
    property cCredPres: TcCredPres read FcCredPres write FcCredPres;
    property pCredPres: Double read FpCredPres write FpCredPres;
    property vCredPres: Double read FvCredPres write FvCredPres;
    property vCredPresCondSus: Double read FvCredPresCondSus write FvCredPresCondSus;
  end;

  { TgCBS }

  TgCBS = class(TObject)
  private
    FpCBS: Double;
    FgDif: TgDif;
    FgDevTrib: TgDevTrib;
    FgRed: TgRed;
    FvCBS: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgCBS);

    property pCBS: Double read FpCBS write FpCBS;
    property gDif: TgDif read FgDif write FgDif;
    property gDevTrib: TgDevTrib read FgDevTrib write FgDevTrib;
    property gRed: TgRed read FgRed write FgRed;
    property vCBS: Double read FvCBS write FvCBS;
  end;

  { TgTribRegular }

  TgTribRegular = class(TObject)
  private
    FCSTReg: TCSTIBSCBS;
    FcClassTribReg: TcClassTrib;
    FpAliqEfetRegIBSUF: Double;
    FvTribRegIBSUF: Double;
    FpAliqEfetRegIBSMun: Double;
    FvTribRegIBSMun: Double;
    FpAliqEfetRegCBS: Double;
    FvTribRegCBS: Double;
  public
    procedure Assign(Source: TgTribRegular);
    property CSTReg: TCSTIBSCBS read FCSTReg write FCSTReg;
    property cClassTribReg: TcClassTrib read FcClassTribReg write FcClassTribReg;
    property pAliqEfetRegIBSUF: Double read FpAliqEfetRegIBSUF write FpAliqEfetRegIBSUF;
    property vTribRegIBSUF: Double read FvTribRegIBSUF write FvTribRegIBSUF;
    property pAliqEfetRegIBSMun: Double read FpAliqEfetRegIBSMun write FpAliqEfetRegIBSMun;
    property vTribRegIBSMun: Double read FvTribRegIBSMun write FvTribRegIBSMun;
    property pAliqEfetRegCBS: Double read FpAliqEfetRegCBS write FpAliqEfetRegCBS;
    property vTribRegCBS: Double read FvTribRegCBS write FvTribRegCBS;
  end;

  { TgTribCompraGov }

  TgTribCompraGov = class(TObject)
  private
    FpAliqIBSUF: Double;
    FvTribIBSUF: Double;
    FpAliqIBSMun: Double;
    FvTribIBSMun: Double;
    FpAliqCBS: Double;
    FvTribCBS: Double;
  public
    procedure Assign(Source: TgTribCompraGov);
    property pAliqIBSUF: Double read FpAliqIBSUF write FpAliqIBSUF;
    property vTribIBSUF: Double read FvTribIBSUF write FvTribIBSUF;
    property pAliqIBSMun: Double read FpAliqIBSMun write FpAliqIBSMun;
    property vTribIBSMun: Double read FvTribIBSMun write FvTribIBSMun;
    property pAliqCBS: Double read FpAliqCBS write FpAliqCBS;
    property vTribCBS: Double read FvTribCBS write FvTribCBS;
  end;

  { TgIBSCBS }

  TgIBSCBS = class(TObject)
  private
    FvBC: Double;
    FvIBS: Double;
    FgIBSUF: TgIBSUF;
    FgIBSMun: TgIBSMun;
    FgCBS: TgCBS;
    FgTribRegular: TgTribRegular;

    FgIBSCredPres: TgIBSCBSCredPres;
    FgCBSCredPres: TgIBSCBSCredPres;
    FgTribCompraGov: TgTribCompraGov;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgIBSCBS);

    property vBC: Double read FvBC write FvBC;
    property vIBS: Double read FvIBS write FvIBS;
    property gIBSUF: TgIBSUF read FgIBSUF write FgIBSUF;
    property gIBSMun: TgIBSMun read FgIBSMun write FgIBSMun;
    property gCBS: TgCBS read FgCBS write FgCBS;
    property gTribRegular: TgTribRegular read FgTribRegular write FgTribRegular;

    property gIBSCredPres: TgIBSCBSCredPres read FgIBSCredPres write FgIBSCredPres;
    property gCBSCredPres: TgIBSCBSCredPres read FgCBSCredPres write FgCBSCredPres;
    property gTribCompraGov: TgTribCompraGov read FgTribCompraGov write FgTribCompraGov;
  end;

  { TgIBSCBSMono }

  TgIBSCBSMono = class(TObject)
  private
    FqBCMono: Double;
    FadRemIBS: Double;
    FadRemCBS: Double;
    FvIBSMono: Double;
    FvCBSMono: Double;

    FqBCMonoReten: Double;
    FadRemIBSReten: Double;
    FvIBSMonoReten: Double;
    FadRemCBSReten: Double;
    FvCBSMonoReten: Double;

    FqBCMonoRet: Double;
    FadRemIBSRet: Double;
    FvIBSMonoRet: Double;
    FadRemCBSRet: Double;
    FvCBSMonoRet: Double;

    FpDifIBS: Double;
    FvIBSMonoDif: Double;
    FpDifCBS: Double;
    FvCBSMonoDif: Double;

    FvTotIBSMonoItem: Double;
    FvTotCBSMonoItem: Double;
  public
    procedure Assign(Source: TgIBSCBSMono);

    property qBCMono: Double read FqBCMono write FqBCMono;
    property adRemIBS: Double read FadRemIBS write FadRemIBS;
    property adRemCBS: Double read FadRemCBS write FadRemCBS;
    property vIBSMono: Double read FvIBSMono write FvIBSMono;
    property vCBSMono: Double read FvCBSMono write FvCBSMono;

    property qBCMonoReten: Double read FqBCMonoReten write FqBCMonoReten;
    property adRemIBSReten: Double read FadRemIBSReten write FadRemIBSReten;
    property vIBSMonoReten: Double read FvIBSMonoReten write FvIBSMonoReten;
    property adRemCBSReten: Double read FadRemCBSReten write FadRemCBSReten;
    property vCBSMonoReten: Double read FvCBSMonoReten write FvCBSMonoReten;

    property qBCMonoRet: Double read FqBCMonoRet write FqBCMonoRet;
    property adRemIBSRet: Double read FadRemIBSRet write FadRemIBSRet;
    property vIBSMonoRet: Double read FvIBSMonoRet write FvIBSMonoRet;
    property adRemCBSRet: Double read FadRemCBSRet write FadRemCBSRet;
    property vCBSMonoRet: Double read FvCBSMonoRet write FvCBSMonoRet;

    property pDifIBS: Double read FpDifIBS write FpDifIBS;
    property vIBSMonoDif: Double read FvIBSMonoDif write FvIBSMonoDif;
    property pDifCBS: Double read FpDifCBS write FpDifCBS;
    property vCBSMonoDif: Double read FvCBSMonoDif write FvCBSMonoDif;

    property vTotIBSMonoItem: Double read FvTotIBSMonoItem write FvTotIBSMonoItem;
    property vTotCBSMonoItem: Double read FvTotCBSMonoItem write FvTotCBSMonoItem;
  end;

  { TgTransfCred }

  TgTransfCred = class(TObject)
  private
    FvCBS: Double;
    FvIBS: Double;
  public
    procedure Assign(Source: TgTransfCred);

    property vIBS: Double read FvIBS write FvIBS;
    property vCBS: Double read FvCBS write FvCBS;
  end;

  { TCredPresIBSZFM }

  TCredPresIBSZFM = class(TObject)
  private
    FtpCredPresIBSZFM: TTpCredPresIBSZFM;
    FvCredPresIBSZFM: Double;
  public
    procedure Assign(Source: TCredPresIBSZFM);

    property tpCredPresIBSZFM: TTpCredPresIBSZFM read FtpCredPresIBSZFM write FtpCredPresIBSZFM;
    property vCredPresIBSZFM: Double read FvCredPresIBSZFM write FvCredPresIBSZFM;
  end;

  { TIBSCBS }

  TIBSCBS = class(TObject)
  private
    FCST: TCSTIBSCBS;
    FcClassTrib: TcClassTrib;

    FgIBSCBS: TgIBSCBS;
    FgIBSCBSMono: TgIBSCBSMono;
    FgTransfCred: TgTransfCred;
    FgCredPresIBSZFM: TCredPresIBSZFM;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TIBSCBS);

    property CST: TCSTIBSCBS read FCST write FCST;
    property cClassTrib: TcClassTrib read FcClassTrib write FcClassTrib;

    property gIBSCBS: TgIBSCBS read FgIBSCBS write FgIBSCBS;
    property gIBSCBSMono: TgIBSCBSMono read FgIBSCBSMono write FgIBSCBSMono;
    property gTransfCred: TgTransfCred read FgTransfCred write FgTransfCred;
    property gCredPresIBSZFM: TCredPresIBSZFM read FgCredPresIBSZFM write FgCredPresIBSZFM;
  end;

  { TImposto }

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
    FISel: TgIS;
    FIBSCBS: TIBSCBS;
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
    // Reforma Tributária
    property ISel: TgIS read FISel write FISel;
    property IBSCBS: TIBSCBS read FIBSCBS write FIBSCBS;
  end;

  { TobsItem }

  TobsItem = class(TObject)
  private
    FxCampo: string;
    FxTexto: string;
  public
    procedure Assign(Source: TobsItem);

    property xCampo: string read FxCampo write FxCampo;
    property xTexto: string read FxTexto write FxTexto;
  end;

  { TDFeReferenciado }

  TDFeReferenciado = class(TObject)
  private
    FchaveAcesso: string;
    FnItem: Integer;
  public
    procedure Assign(Source: TDFeReferenciado);
    property chaveAcesso: string read FchaveAcesso write FchaveAcesso;
    property nItem: Integer read FnItem write FnItem;
  end;

  { TDetCollectionItem }

  TDetCollectionItem = class(TObject)
  private
    FProd: TProd;
    FImposto: TImposto;
    FpDevol: Currency;
    FvIPIDevol: Currency;
    FinfAdProd: string;
    FobsCont: TobsItem;
    FobsFisco: TobsItem;
    FvItem: Currency;
    FDFeReferenciado: TDFeReferenciado;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TDetCollectionItem);

    property Prod: TProd read FProd write FProd;
    property Imposto: TImposto read FImposto write FImposto;
    property pDevol: Currency read FpDevol write FpDevol;
    property vIPIDevol: Currency read FvIPIDevol write FvIPIDevol;
    property infAdProd: string read FinfAdProd write FinfAdProd;
    property obsCont: TobsItem read FobsCont write FobsCont;
    property obsFisco: TobsItem read FobsFisco write FobsFisco;
    // Reforma Tributária
    property vItem: Currency read FvItem write FvItem;
    property DFeReferenciado: TDFeReferenciado read FDFeReferenciado write FDFeReferenciado;
  end;

  { TDetCollection }

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

  { TICMSTot }

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

  { TISSQNtot }

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
//    FcServico: string;
//    FcMun: Integer;
//    FcPais: Integer;
//    FnProcesso: string;
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
//    property cServico: string read FcServico write FcServico;
//    property cMun: Integer read FcMun write FcMun;
//    property cPais: Integer read FcPais write FcPais;
//    property nProcesso: string read FnProcesso write FnProcesso;
    property vISSRet: Currency read FvISSRet write FvISSRet;
    property cRegTrib: TpcnRegTribISSQN read FcRegTrib write FcRegTrib;
//    property indIncentivo: TpcnindIncentivo read FindIncentivo write FindIncentivo;
  end;

  { TretTrib }

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

  { TISTot }

  TISTot = class(TObject)
  private
    FvIS: Double;
  public
    procedure Assign(Source: TISTot);

    property vIS: Double read FvIS write FvIS;
  end;

  { TgIBSUFTot }

  TgIBSUFTot = class(TObject)
  private
    FvDif: Double;
    FvDevTrib: Double;
    FvIBSUF: Double;
  public
    procedure Assign(Source: TgIBSUFTot);
    property vDif: Double read FvDif write FvDif;
    property vDevTrib: Double read FvDevTrib write FvDevTrib;
    property vIBSUF: Double read FvIBSUF write FvIBSUF;
  end;

  { TgIBSMunTot }

  TgIBSMunTot = class(TObject)
  private
    FvDif: Double;
    FvDevTrib: Double;
    FvIBSMun: Double;
  public
    procedure Assign(Source: TgIBSMunTot);
    property vDif: Double read FvDif write FvDif;
    property vDevTrib: Double read FvDevTrib write FvDevTrib;
    property vIBSMun: Double read FvIBSMun write FvIBSMun;
  end;

  { TgIBSTot }

  TgIBSTot = class(TObject)
  private
    FgIBSUFTot: TgIBSUFTot;
    FgIBSMunTot: TgIBSMunTot;
    FvIBS: Double;
    FvCredPres: Double;
    FvCredPresCondSus: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgIBSTot);

    property gIBSUFTot: TgIBSUFTot read FgIBSUFTot write FgIBSUFTot;
    property gIBSMunTot: TgIBSMunTot read FgIBSMunTot write FgIBSMunTot;
    property vIBS: Double read FvIBS write FvIBS;
    property vCredPres: Double read FvCredPres write FvCredPres;
    property vCredPresCondSus: Double read FvCredPresCondSus write FvCredPresCondSus;
  end;

  { TgCBSTot }

  TgCBSTot = class(TObject)
  private
    FvDif: Double;
    FvDevTrib: Double;
    FvCBS: Double;
    FvCredPres: Double;
    FvCredPresCondSus: Double;
  public
    procedure Assign(Source: TgCBSTot);
    property vDif: Double read FvDif write FvDif;
    property vDevTrib: Double read FvDevTrib write FvDevTrib;
    property vCBS: Double read FvCBS write FvCBS;
    property vCredPres: Double read FvCredPres write FvCredPres;
    property vCredPresCondSus: Double read FvCredPresCondSus write FvCredPresCondSus;
  end;

  { TgMono }

  TgMono = class(TObject)
  private
    FvIBSMono: Double;
    FvCBSMono: Double;
    FvIBSMonoReten: Double;
    FvCBSMonoReten: Double;
    FvIBSMonoRet: Double;
    FvCBSMonoRet: Double;
  public
    procedure Assign(Source: TgMono);
    property vIBSMono: Double read FvIBSMono write FvIBSMono;
    property vCBSMono: Double read FvCBSMono write FvCBSMono;
    property vIBSMonoReten: Double read FvIBSMonoReten write FvIBSMonoReten;
    property vCBSMonoReten: Double read FvCBSMonoReten write FvCBSMonoReten;
    property vIBSMonoRet: Double read FvIBSMonoRet write FvIBSMonoRet;
    property vCBSMonoRet: Double read FvCBSMonoRet write FvCBSMonoRet;
  end;

  { TIBSCBSTot }

  TIBSCBSTot = class(TObject)
  private
    FvBCIBSCBS: Double;

    FgIBS: TgIBSTot;
    FgCBS: TgCBSTot;
    FgMono: TgMono;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TIBSCBSTot);

    property vBCIBSCBS: Double read FvBCIBSCBS write FvBCIBSCBS;

    property gIBS: TgIBSTot read FgIBS write FgIBS;
    property gCBS: TgCBSTot read FgCBS write FgCBS;
    property gMono: TgMono read FgMono write FgMono;
  end;

  { TTotal }

  TTotal = class(TObject)
  private
    FICMSTot: TICMSTot;
    FISSQNtot: TISSQNtot;
    FretTrib: TretTrib;

    FISTot: TISTot;
    FIBSCBSTot: TIBSCBSTot;
    FvNFTot: Double;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TTotal);

    property ICMSTot: TICMSTot read FICMSTot write FICMSTot;
    property ISSQNtot: TISSQNtot read FISSQNtot write FISSQNtot;
    property retTrib: TretTrib read FretTrib write FretTrib;
    // Reforma Tributária
    property ISTot: TISTot read FISTot write FISTot;
    property IBSCBSTot: TIBSCBSTot read FIBSCBSTot write FIBSCBSTot;
    property vNFTot: Double read FvNFTot write FvNFTot;
  end;

  { TTransporta }

  TTransporta = class(TObject)
  private
    FCNPJCPF: string;
    FxNome: string;
    FIE: string;
    FxEnder: string;
    FxMun: string;
    FUF: string;
  public
    procedure Assign(Source: TTransporta);
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property xNome: string read FxNome write FxNome;
    property IE: string read FIE write FIE;
    property xEnder: string read FxEnder write FxEnder;
    property xMun: string read FxMun write FxMun;
    property UF: string read FUF write FUF;
  end;

  { TretTransp }

  TretTransp = class(TObject)
  private
    FvServ: Currency;
    FvBCRet: Currency;
    FpICMSRet: Currency;
    FvICMSRet: Currency;
    FCFOP: string;
    FcMunFG: Integer;
  public
    procedure Assign(Source: TretTransp);
    property vServ: Currency read FvServ write FvServ;
    property vBCRet: Currency read FvBCRet write FvBCRet;
    property pICMSRet: Currency read FpICMSRet write FpICMSRet;
    property vICMSRet: Currency read FvICMSRet write FvICMSRet;
    property CFOP: string read FCFOP write FCFOP;
    property cMunFG: Integer read FcMunFG write FcMunFG;
  end;

  { TveicTransp }

  TveicTransp = class(TObject)
  private
    Fplaca: string;
    FUF: string;
    FRNTC: string;
  public
    procedure Assign(Source: TveicTransp);
    property placa: string read Fplaca write Fplaca;
    property UF: string read FUF write FUF;
    property RNTC: string read FRNTC write FRNTC;
  end;

  { TLacresCollectionItem }

  TLacresCollectionItem = class(TObject)
  private
    FnLacre: string;
  public
    procedure Assign(Source: TLacresCollectionItem);
    property nLacre: string read FnLacre write FnLacre;
  end;

  { TLacresCollection }

  TLacresCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TLacresCollectionItem;
    procedure SetItem(Index: Integer; Value: TLacresCollectionItem);
  public
    function Add: TLacresCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TLacresCollectionItem;
    property Items[Index: Integer]: TLacresCollectionItem read GetItem write SetItem; default;
  end;

  { TVolCollectionItem }

  TVolCollectionItem = class(TObject)
  private
    FqVol: Integer;
    Fesp: string;
    Fmarca: string;
    FnVol: string;
    FpesoL: Currency;
    FpesoB: Currency;
    FLacres: TLacresCollection;

    procedure SetLacres(Value: TLacresCollection);
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TVolCollectionItem);
    property qVol: Integer read FqVol write FqVol;
    property esp: string read Fesp write Fesp;
    property marca: string read Fmarca write Fmarca;
    property nVol: string read FnVol write FnVol;
    property pesoL: Currency read FpesoL write FpesoL;
    property pesoB: Currency read FpesoB write FpesoB;
    property Lacres: TLacresCollection read FLacres write SetLacres;
  end;

  { TVolCollection }

  TVolCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TVolCollectionItem;
    procedure SetItem(Index: Integer; Value: TVolCollectionItem);
  public
    function Add: TVolCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TVolCollectionItem;
    property Items[Index: Integer]: TVolCollectionItem read GetItem write SetItem; default;
  end;

  { TReboqueCollectionItem }

  TReboqueCollectionItem = class(TObject)
  private
    Fplaca: string;
    FUF: string;
    FRNTC: string;
  public
    procedure Assign(Source: TReboqueCollectionItem);

    property placa: string read Fplaca write Fplaca;
    property UF: string read FUF write FUF;
    property RNTC: string read FRNTC write FRNTC;
  end;

  { TReboqueCollection }

  TReboqueCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TReboqueCollectionItem;
    procedure SetItem(Index: Integer; Value: TReboqueCollectionItem);
  public
    function Add: TReboqueCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TReboqueCollectionItem;
    property Items[Index: Integer]: TReboqueCollectionItem read GetItem write SetItem; default;
  end;

  { TTransp }

  TTransp = class(TObject)
  private
    FmodFrete: TpcnModalidadeFrete;
    FTransporta: TTransporta;
    FretTransp: TretTransp;
    FveicTransp: TveicTransp;
    FVol: TVolCollection;
    FReboque: TReboqueCollection;
    Fvagao: string;
    Fbalsa: string;

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
    property vagao: string read Fvagao write Fvagao;
    property balsa: string read Fbalsa write Fbalsa;
  end;

  { TFat }

  TFat = class(TObject)
  private
    FnFat: string;
    FvOrig: Currency;
    FvDesc: Currency;
    FvLiq: Currency;
  public
    procedure Assign(Source: TFat);
    property nFat: string read FnFat write FnFat;
    property vOrig: Currency read FvOrig write FvOrig;
    property vDesc: Currency read FvDesc write FvDesc;
    property vLiq: Currency read FvLiq write FvLiq;
  end;

  { TDupCollectionItem }

  TDupCollectionItem = class(TObject)
  private
    FnDup: string;
    FdVenc: TDateTime;
    FvDup: Currency;
  public
    procedure Assign(Source: TDupCollectionItem);
    property nDup: string read FnDup write FnDup;
    property dVenc: TDateTime read FdVenc write FdVenc;
    property vDup: Currency read FvDup write FvDup;
  end;

  { TDupCollection }

  TDupCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDupCollectionItem;
    procedure SetItem(Index: Integer; Value: TDupCollectionItem);
  public
    function Add: TDupCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDupCollectionItem;
    property Items[Index: Integer]: TDupCollectionItem read GetItem write SetItem; default;
  end;

  { TCobr }

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

  { TpagCollectionItem }

  TpagCollectionItem = class(TObject)
  private
    FtPag: TpcnFormaPagamento;
    FxPag: string;
    FvPag: Currency;
    FtpIntegra: TtpIntegra;
    FCNPJ: string;
    FtBand: TpcnBandeiraCartao;
    FcAut: string;
    FindPag: TpcnIndicadorPagamento;
    FdPag: TDateTime;
    FCNPJPag: string;
    FUFPag: string;
    FCNPJReceb: string;
    FidTermPag: string;
  public
    constructor Create;
    procedure Assign(Source: TpagCollectionItem);

    property indPag: TpcnIndicadorPagamento read FindPag write FindPag default ipNenhum;
    property tPag: TpcnFormaPagamento read FtPag write FtPag;
    property xPag: string read FxPag write FxPag;
    property vPag: Currency read FvPag write FvPag;
    property tpIntegra: TtpIntegra read FtpIntegra write FtpIntegra;
    property CNPJ: string read FCNPJ write FCNPJ;
    property tBand: TpcnBandeiraCartao read FtBand write FtBand;
    property cAut: string read FcAut write FcAut;
    property dPag: TDateTime read FdPag write FdPag;
    property CNPJPag: string read FCNPJPag write FCNPJPag;
    property UFPag: string read FUFPag write FUFPag;
    property CNPJReceb: string read FCNPJReceb write FCNPJReceb;
    property idTermPag: string read FidTermPag write FidTermPag;
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

  { TinfIntermed }

  TinfIntermed = class(TObject)
  private
    FCNPJ: string;
    FidCadIntTran: string;
  public
    procedure Assign(Source: TinfIntermed);
    property CNPJ: string read FCNPJ write FCNPJ;
    property idCadIntTran: string read FidCadIntTran write FidCadIntTran;
  end;

  { TobsContCollectionItem }

  TobsContCollectionItem = class(TObject)
  private
    FxCampo: string;
    FxTexto: string;
  public
    procedure Assign(Source: TobsContCollectionItem);
    property xCampo: string read FxCampo write FxCampo;
    property xTexto: string read FxTexto write FxTexto;
  end;

  { TobsContCollection }

  TobsContCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TobsContCollectionItem;
    procedure SetItem(Index: Integer; Value: TobsContCollectionItem);
  public
    function Add: TobsContCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TobsContCollectionItem;
    property Items[Index: Integer]: TobsContCollectionItem read GetItem write SetItem; default;
  end;

  { TobsFiscoCollectionItem }

  TobsFiscoCollectionItem = class(TObject)
  private
    FxCampo: string;
    FxTexto: string;
  public
    procedure Assign(Source: TobsFiscoCollectionItem);
    property xCampo: string read FxCampo write FxCampo;
    property xTexto: string read FxTexto write FxTexto;
  end;

  { TobsFiscoCollection }

  TobsFiscoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TobsFiscoCollectionItem;
    procedure SetItem(Index: Integer; Value: TobsFiscoCollectionItem);
  public
    function Add: TobsFiscoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TobsFiscoCollectionItem;
    property Items[Index: Integer]: TobsFiscoCollectionItem read GetItem write SetItem; default;
  end;

  { TprocRefCollectionItem }

  TprocRefCollectionItem = class(TObject)
  private
    FnProc: string;
    FindProc: TpcnIndicadorProcesso;
    FtpAto: TtpAto;
  public
    procedure Assign(Source: TprocRefCollectionItem);

    property nProc: string read FnProc write FnProc;
    property indProc: TpcnIndicadorProcesso read FindProc write FindProc default ipSEFAZ;
    property tpAto: TtpAto read FtpAto write FtpAto;
  end;

  { TprocRefCollection }

  TprocRefCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TprocRefCollectionItem;
    procedure SetItem(Index: Integer; Value: TprocRefCollectionItem);
  public
    function Add: TprocRefCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TprocRefCollectionItem;
    property Items[Index: Integer]: TprocRefCollectionItem read GetItem write SetItem; default;
  end;

  { TInfAdic }

  TInfAdic = class(TObject)
  private
    FinfAdFisco: string;
    FinfCpl: string;
    FobsCont: TobsContCollection;
    FobsFisco: TobsFiscoCollection;
    FprocRef: TprocRefCollection;

    procedure SetobsCont(Value: TobsContCollection);
    procedure SetobsFisco(Value: TobsFiscoCollection);
    procedure SetprocRef(Value: TprocRefCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TInfAdic);
    property infAdFisco: string read FinfAdFisco write FinfAdFisco;
    property infCpl: string read FinfCpl write FinfCpl;
    property obsCont: TobsContCollection read FobsCont write SetobsCont;
    property obsFisco: TobsFiscoCollection read FobsFisco write SetobsFisco;
    property procRef: TprocRefCollection read FprocRef write SetprocRef;
  end;

  { TExporta }

  TExporta = class(TObject)
  private
    FUFembarq: string;
    FxLocEmbarq: string;
    // Versao 3.10
    FUFSaidaPais: string;
    FxLocExporta: string;
    FxLocDespacho: string;
  public
    procedure Assign(Source: TExporta);
    property UFembarq: string read FUFembarq write FUFembarq;
    property xLocEmbarq: string read FxLocEmbarq write FxLocEmbarq;
    // Versao 3.10
    property UFSaidaPais: string read FUFSaidaPais write FUFSaidaPais;
    property xLocExporta: string read FxLocExporta write FxLocExporta;
    property xLocDespacho: string read FxLocDespacho write FxLocDespacho;
  end;

  { TCompra }

  TCompra = class(TObject)
  private
    FxNEmp: string;
    FxPed: string;
    FxCont: string;
  public
    procedure Assign(Source: TCompra);
    property xNEmp: string read FxNEmp write FxNEmp;
    property xPed: string read FxPed write FxPed;
    property xCont: string read FxCont write FxCont;
  end;

  { TForDiaCollectionItem }

  TForDiaCollectionItem = class(TObject)
  private
    Fdia: Integer;
    Fqtde: Currency;
  public
    procedure Assign(Source: TForDiaCollectionItem);
    property dia: Integer read Fdia write Fdia;
    property qtde: Currency read Fqtde write Fqtde;
  end;

  { TForDiaCollection }

  TForDiaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TForDiaCollectionItem;
    procedure SetItem(Index: Integer; Value: TForDiaCollectionItem);
  public
    function Add: TForDiaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TForDiaCollectionItem;
    property Items[Index: Integer]: TForDiaCollectionItem read GetItem write SetItem; default;
  end;

  { TDeducCollectionItem }

  TDeducCollectionItem = class(TObject)
  private
    FxDed: string;
    FvDed: Currency;
  public
    procedure Assign(Source: TDeducCollectionItem);
    property xDed: string read FxDed write FxDed;
    property vDed: Currency read FvDed write FvDed;
  end;

  { TDeducCollection }

  TDeducCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDeducCollectionItem;
    procedure SetItem(Index: Integer; Value: TDeducCollectionItem);
  public
    function Add: TDeducCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TDeducCollectionItem;
    property Items[Index: Integer]: TDeducCollectionItem read GetItem write SetItem; default;
  end;

  { Tcana }

  Tcana = class(TObject)
  private
    Fsafra: string;
    Fref: string;
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
    property safra: string read Fsafra write Fsafra;
    property ref: string read Fref write Fref;
    property fordia: TForDiaCollection read Ffordia write SetForDia;
    property qTotMes: Double read FqTotMes write FqTotMes;
    property qTotAnt: Double read FqTotAnt write FqTotAnt;
    property qTotGer: Double read FqTotGer write FqTotGer;
    property deduc: TDeducCollection read Fdeduc write SetDeduc;
    property vFor: Currency read FvFor write FvFor;
    property vTotDed: Currency read FvTotDed write FvTotDed;
    property vLiqFor: Currency read FvLiqFor write FvLiqFor;
  end;

  { TautXMLCollectionItem }

  TautXMLCollectionItem = class(TObject)
  private
    FCNPJCPF: string;
  public
    procedure Assign(Source: TautXMLCollectionItem);
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
  end;

  { TautXMLCollection }

  TautXMLCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TautXMLCollectionItem;
    procedure SetItem(Index: Integer; Value: TautXMLCollectionItem);
  public
    function Add: TautXMLCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TautXMLCollectionItem;
    property Items[Index: Integer]: TautXMLCollectionItem read GetItem write SetItem; default;
  end;

  { TinfNFeSupl }

  TinfNFeSupl = class(TObject)
  private
    FqrCode: string;
    FurlChave: string;
  public
    procedure Assign(Source: TinfNFeSupl);
    property qrCode: string read FqrCode write FqrCode;
    property urlChave: string read FurlChave write FurlChave;
  end;

  { TinfRespTec }

  TinfRespTec = class(TObject)
  private
    FCNPJ: string;
    FxContato: string;
    Femail: string;
    Ffone: string;
    FidCSRT: Integer;
    FhashCSRT: string;
  public
    procedure Assign(Source: TinfRespTec);
    property CNPJ: string     read FCNPJ     write FCNPJ;
    property xContato: string read FxContato write FxContato;
    property email: string    read Femail    write Femail;
    property fone: string     read Ffone     write Ffone;
    property idCSRT: Integer  read FidCSRT   write FidCSRT;
    property hashCSRT: string read FhashCSRT write FhashCSRT;
  end;

  { TdefensivoCollectionItem }

  TdefensivoCollectionItem = class(TObject)
  private
    FnReceituario: string;
    FCPFRespTec: string;
  public
    procedure Assign(Source: TdefensivoCollectionItem);

    property nReceituario: string read FnReceituario write FnReceituario;
    property CPFRespTec: string read FCPFRespTec write FCPFRespTec;
  end;

  { TdefensivoCollection }

  TdefensivoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdefensivoCollectionItem;
    procedure SetItem(Index: Integer; Value: TdefensivoCollectionItem);
  public
    function Add: TdefensivoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TdefensivoCollectionItem;
    property Items[Index: Integer]: TdefensivoCollectionItem read GetItem write SetItem; default;
  end;

  { TguiaTransito }

  TguiaTransito = class(TObject)
  private
    FUFGuia: string;
    FtpGuia: TtpGuia;
    FserieGuia: string;
    FnGuia: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TguiaTransito);
    property UFGuia: string read FUFGuia write FUFGuia;
    property tpGuia: TtpGuia read FtpGuia write FtpGuia;
    property serieGuia: string read FserieGuia write FserieGuia;
    property nGuia: string read FnGuia write FnGuia;
  end;

  { Tagropecuario }

  Tagropecuario = class(TObject)
  private
    Fdefensivo: TdefensivoCollection;
    FguiaTransito: TguiaTransito;

    procedure Setdefensivo(const Value: TdefensivoCollection);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: Tagropecuario);

    property defensivo: TdefensivoCollection read Fdefensivo write Setdefensivo;
    property guiaTransito: TguiaTransito read FguiaTransito write FguiaTransito;
  end;

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
    Fagropecuario: Tagropecuario;

    procedure SetDet(Value: TDetCollection);
    procedure Setpag(Value: TpagCollection);
    procedure SetautXML(const Value: TautXMLCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TNFe);
//    procedure SetXMLString(const AValue : AnsiString);
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
    property agropecuario: Tagropecuario read Fagropecuario write Fagropecuario;
  end;

const
  CMUN_EXTERIOR = 9999999;
  XMUN_EXTERIOR = 'EXTERIOR';
  UF_EXTERIOR = 'EX';

implementation

uses
  ACBrUtil.Base;

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
  agropecuario.Assign(Source.agropecuario);
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
  FinfAdic := TinfAdic.Create;
  FExporta := TExporta.Create;
  FCompra  := TCompra.Create;
  FCana    := TCana.Create;
  FinfNFeSupl := TinfNFeSupl.Create;
  Fsignature := Tsignature.create;
  FProcNFe := TProcNFe.create;
  FinfRespTec := TinfRespTec.create;
  Fagropecuario := Tagropecuario.Create;

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
  FAgropecuario.Free;

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
  vItem := Source.vItem;
  DFeReferenciado.Assign(Source.DFeReferenciado);

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
  FDFeReferenciado := TDFeReferenciado.Create;
end;

destructor TDetCollectionItem.Destroy;
begin
  FProd.Free;
  FImposto.Free;
  FobsCont.Free;
  FobsFisco.Free;
  FDFeReferenciado.Free;

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
  cMunFGIBS := Source.cMunFGIBS;
  tpNFDebito := Source.tpNFDebito;
  tpNFCredito := Source.tpNFCredito;
  gCompraGov.Assign(Source.gCompraGov);
  gPagAntecipado.Assign(Source.gPagAntecipado);
end;

constructor TIde.Create;
begin
  inherited Create;

  FNFref := TNFrefCollection.Create();
  FgCompraGov := TgCompraGov.Create;
  FgPagAntecipado := TgPagAntecipadoCollection.Create();
end;

destructor TIde.Destroy;
begin
  FNFref.Free;
  FgCompraGov.Free;
  FgPagAntecipado.Free;

  inherited;
end;

procedure TIde.SetgPagAntecipado(const Value: TgPagAntecipadoCollection);
begin
  FgPagAntecipado := Value;
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
  FCredPresumido := TCredPresumidoCollection.Create;
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
  FCredPresumido.Free;

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
  CredPresumido.Assign(Source.CredPresumido);
  indBemMovelUsado := Source.indBemMovelUsado;
end;

procedure TProd.setCFOP(const Value: string);
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

procedure TProd.SetCredPresumido(const Value: TCredPresumidoCollection);
begin
  FCredPresumido := Value;
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
  ISel.Assign(Source.ISel);
  IBSCBS.Assign(Source.IBSCBS);
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
  FISel := TgIS.Create;
  FIBSCBS := TIBSCBS.Create;
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
  FISel.Free;
  FIBSCBS.Free;

  inherited;
end;

{ TTotal }

procedure TTotal.Assign(Source: TTotal);
begin
  ICMSTot.Assign(Source.ICMSTot);
  ISSQNtot.Assign(Source.ISSQNtot);
  retTrib.Assign(Source.retTrib);
  ISTot.Assign(Source.ISTot);
  IBSCBSTot.Assign(Source.IBSCBSTot);
  vNFTot := Source.vNFTot;
end;

constructor TTotal.Create();
begin
  inherited Create;
  FICMSTot := TICMSTot.Create;
  FISSQNtot := TISSQNtot.create;
  FretTrib := TretTrib.create;
  FISTot := TISTot.Create;
  FIBSCBSTot := TIBSCBSTot.Create;
end;

destructor TTotal.Destroy;
begin
  FICMSTot.Free;
  FISSQNtot.Free;
  FretTrib.Free;
  FISTot.Free;
  FIBSCBSTot.Free;

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

constructor TinfAdic.Create;
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

function TveicProd.getCombDescricao: string;
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
  cBenefRBC := Source.cBenefRBC;
end;

constructor TICMS.Create;
begin
  inherited Create;

  FindDeduzDeson := tieNenhum;
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

{ TCredPresumidoCollectionItem }

procedure TCredPresumidoCollectionItem.Assign(
  Source: TCredPresumidoCollectionItem);
begin
  cCredPresumido := Source.cCredPresumido;
  pCredPresumido := Source.pCredPresumido;
  vCredPresumido := Source.vCredPresumido;
end;

{ TCredPresumidoCollection }

function TCredPresumidoCollection.Add: TCredPresumidoCollectionItem;
begin
  Result := Self.New;
end;

function TCredPresumidoCollection.GetItem(
  Index: Integer): TCredPresumidoCollectionItem;
begin
  Result := TCredPresumidoCollectionItem(inherited Items[Index]);
end;

function TCredPresumidoCollection.New: TCredPresumidoCollectionItem;
begin
  Result := TCredPresumidoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TCredPresumidoCollection.SetItem(Index: Integer;
  Value: TCredPresumidoCollectionItem);
begin
  inherited Items[Index] := Value;
end;


{ TguiaTransito }

procedure TguiaTransito.Assign(Source: TguiaTransito);
begin
  FUFGuia := Source.UFGuia;
  FtpGuia := Source.tpGuia;
  FserieGuia := Source.serieGuia;
  FnGuia := Source.nGuia;
end;

constructor TguiaTransito.Create;
begin
  inherited Create;
  FtpGuia := tpgNenhum;
end;

destructor TguiaTransito.Destroy;
begin
  inherited;
end;

{ Tagropecuario }

procedure Tagropecuario.Assign(Source: Tagropecuario);
begin
  Fdefensivo.Assign(Source.defensivo);
  FguiaTransito.Assign(Source.guiaTransito);
end;

constructor Tagropecuario.Create;
begin
  inherited Create;

  Fdefensivo := TdefensivoCollection.Create;
  FguiaTransito := TguiaTransito.Create;
end;

destructor Tagropecuario.Destroy;
begin
  Fdefensivo.Free;
  FguiaTransito.Free;
  inherited;
end;

procedure Tagropecuario.Setdefensivo(const Value: TdefensivoCollection);
begin
  Fdefensivo := Value;
end;

{ TdefensivoCollectionItem }

procedure TdefensivoCollectionItem.Assign(Source: TdefensivoCollectionItem);
begin
  nReceituario := Source.nReceituario;
  CPFRespTec := Source.CPFRespTec;
end;

{ TdefensivoCollection }

function TdefensivoCollection.Add: TdefensivoCollectionItem;
begin
  Result := Self.New;
end;

function TdefensivoCollection.GetItem(Index: Integer): TdefensivoCollectionItem;
begin
  Result := TdefensivoCollectionItem(inherited Items[Index]);
end;

function TdefensivoCollection.New: TdefensivoCollectionItem;
begin
  Result := TdefensivoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TdefensivoCollection.SetItem(Index: Integer;
  Value: TdefensivoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgCompraGov }

procedure TgCompraGov.Assign(Source: TgCompraGov);
begin
  tpEnteGov := Source.tpEnteGov;
  pRedutor := Source.pRedutor;
  tpOperGov := Source.tpOperGov;
end;

{ TIBSCBS }

procedure TIBSCBS.Assign(Source: TIBSCBS);
begin
  CST := Source.CST;
  cClassTrib := Source.cClassTrib;
  gIBSCBS.Assign(Source.gIBSCBS);
  gIBSCBSMono.Assign(Source.gIBSCBSMono);
  gTransfCred.Assign(Source.gTransfCred);
  gCredPresIBSZFM.Assign(Source.gCredPresIBSZFM);
end;

constructor TIBSCBS.Create;
begin
  inherited Create;

  FgIBSCBS := TgIBSCBS.Create;
  FgIBSCBSMono := TgIBSCBSMono.Create;
  FgTransfCred := TgTransfCred.Create;
  FgCredPresIBSZFM := TCredPresIBSZFM.Create;
end;

destructor TIBSCBS.Destroy;
begin
  FgIBSCBS.Free;
  FgIBSCBSMono.Free;
  FgTransfCred.Free;
  FgCredPresIBSZFM.Free;

  inherited;
end;

{ TgIS }

procedure TgIS.Assign(Source: TgIS);
begin
  CSTIS := Source.CSTIS;
  cClassTribIS := Source.cClassTribIS;
  vBCIS := Source.vBCIS;
  pIS := Source.pIS;
  pISEspec := Source.pISEspec;
  uTrib := Source.uTrib;
  qTrib := Source.qTrib;
  vIS := Source.vIS;
end;

{ TgIBSCBS }

procedure TgIBSCBS.Assign(Source: TgIBSCBS);
begin
  vBC := Source.vBC;
  vIBS := Source.vIBS;
  gIBSUF.Assign(Source.gIBSUF);
  gIBSMun.Assign(Source.gIBSMun);
  gCBS.Assign(Source.gCBS);
  gTribRegular.Assign(Source.gTribRegular);
  gIBSCredPres.Assign(Source.gIBSCredPres);
  gCBSCredPres.Assign(Source.gCBSCredPres);
  gTribCompraGov.Assign(Source.gTribCompraGov);
end;

constructor TgIBSCBS.Create;
begin
  inherited Create;

  FgIBSUF := TgIBSUF.Create;
  FgIBSMun := TgIBSMun.Create;
  FgCBS := TgCBS.Create;
  FgTribRegular := TgTribRegular.Create;
  FgIBSCredPres := TgIBSCBSCredPres.Create;
  FgCBSCredPres := TgIBSCBSCredPres.Create;
  FgTribCompraGov := TgTribCompraGov.Create;
end;

destructor TgIBSCBS.Destroy;
begin
  FgIBSUF.Free;
  FgIBSMun.Free;
  FgCBS.Free;
  FgTribRegular.Free;
  FgIBSCredPres.Free;
  FgCBSCredPres.Free;
  FgTribCompraGov.Free;

  inherited;
end;

{ TgIBSUF }

procedure TgIBSUF.Assign(Source: TgIBSUF);
begin
  pIBSUF := Source.pIBSUF;
  gDif.Assign(Source.gDif);
  gDevTrib.Assign(Source.gDevTrib);
  gRed.Assign(Source.gRed);
  vIBSUF := Source.vIBSUF;
end;

constructor TgIBSUF.Create;
begin
  inherited Create;

  FgDif := TgDif.Create;
  FgDevTrib := TgDevTrib.Create;
  FgRed := TgRed.Create;
end;

destructor TgIBSUF.Destroy;
begin
  FgDif.Free;
  FgDevTrib.Free;
  FgRed.Free;

  inherited;
end;

{ TgIBSMun }

procedure TgIBSMun.Assign(Source: TgIBSMun);
begin
  pIBSMun := Source.pIBSMun;
  gDif.Assign(Source.gDif);
  gDevTrib.Assign(Source.gDevTrib);
  gRed.Assign(Source.gRed);
  vIBSMun := Source.vIBSMun;
end;

constructor TgIBSMun.Create;
begin
  inherited Create;

  FgDif := TgDif.Create;
  FgDevTrib := TgDevTrib.Create;
  FgRed := TgRed.Create;
end;

destructor TgIBSMun.Destroy;
begin
  FgDif.Free;
  FgDevTrib.Free;
  FgRed.Free;

  inherited;
end;

{ TgCBS }

procedure TgCBS.Assign(Source: TgCBS);
begin
  pCBS := Source.pCBS;
  gDif.Assign(Source.gDif);
  gDevTrib.Assign(Source.gDevTrib);
  gRed.Assign(Source.gRed);
  vCBS := Source.vCBS;
end;

constructor TgCBS.Create;
begin
  inherited Create;

  FgDif := TgDif.Create;
  FgDevTrib := TgDevTrib.Create;
  FgRed := TgRed.Create;
end;

destructor TgCBS.Destroy;
begin
  FgDif.Free;
  FgDevTrib.Free;
  FgRed.Free;

  inherited;
end;

{ TgIBSCBSMono }

procedure TgIBSCBSMono.Assign(Source: TgIBSCBSMono);
begin
  qBCMono := Source.qBCMono;
  adRemIBS := Source.adRemIBS;
  adRemCBS := Source.adRemCBS;
  vIBSMono := Source.vIBSMono;
  vCBSMono := Source.vCBSMono;

  qBCMonoReten := Source.qBCMonoReten;
  adRemIBSReten := Source.adRemIBSReten;
  vIBSMonoReten := Source.vIBSMonoReten;
  adRemCBSReten := Source.adRemCBSReten;
  vCBSMonoReten := Source.vCBSMonoReten;

  qBCMonoRet := Source.qBCMonoRet;
  adRemIBSRet := Source.adRemIBSRet;
  vIBSMonoRet := Source.vIBSMonoRet;
  adRemCBSRet := Source.adRemCBSRet;
  vCBSMonoRet := Source.vCBSMonoRet;

  pDifIBS := Source.pDifIBS;
  vIBSMonoDif := Source.vIBSMonoDif;
  pDifCBS := Source.pDifCBS;
  vCBSMonoDif := Source.vCBSMonoDif;

  vTotIBSMonoItem := Source.vTotIBSMonoItem;
  vTotCBSMonoItem := Source.vTotCBSMonoItem;
end;

{ TIBSCBSTot }

procedure TIBSCBSTot.Assign(Source: TIBSCBSTot);
begin
  vBCIBSCBS := Source.vBCIBSCBS;
  gIBS.Assign(Source.gIBS);
  gCBS.Assign(Source.gCBS);
  gMono.Assign(Source.gMono);
end;

constructor TIBSCBSTot.Create;
begin
  inherited Create;

  FgIBS := TgIBSTot.Create;
  FgCBS := TgCBSTot.Create;
  FgMono := TgMono.Create;
end;

destructor TIBSCBSTot.Destroy;
begin
  FgIBS.Free;
  FgCBS.Free;
  FgMono.Free;

  inherited;
end;

{ TgIBSTot }

procedure TgIBSTot.Assign(Source: TgIBSTot);
begin
  gIBSUFTot.Assign(Source.gIBSUFTot);
  gIBSMunTot.Assign(Source.gIBSMunTot);
  vIBS := Source.vIBS;
  vCredPres := Source.vCredPres;
  vCredPresCondSus := Source.vCredPresCondSus;
end;

constructor TgIBSTot.Create;
begin
  inherited Create;

  FgIBSUFTot := TgIBSUFTot.Create;
  FgIBSMunTot := TgIBSMunTot.Create;
end;

destructor TgIBSTot.Destroy;
begin
  FgIBSUFTot.Free;
  FgIBSMunTot.Free;

  inherited;
end;

{ TISTot }

procedure TISTot.Assign(Source: TISTot);
begin
  vIS := Source.vIS;
end;

{ TgTransfCred }

procedure TgTransfCred.Assign(Source: TgTransfCred);
begin
  vIBS := Source.vIBS;
  vCBS := Source.vCBS;
end;

{ TCredPresIBSZFM }

procedure TCredPresIBSZFM.Assign(Source: TCredPresIBSZFM);
begin
  tpCredPresIBSZFM := Source.tpCredPresIBSZFM;
  vCredPresIBSZFM := Source.vCredPresIBSZFM;
end;

{ TgPagAntecipadoCollectionItem }

procedure TgPagAntecipadoCollectionItem.Assign(
  Source: TgPagAntecipadoCollectionItem);
begin
  refNFe := Source.refNFe;
end;

{ TgPagAntecipadoCollection }

function TgPagAntecipadoCollection.Add: TgPagAntecipadoCollectionItem;
begin
  Result := Self.New;
end;

function TgPagAntecipadoCollection.GetItem(
  Index: Integer): TgPagAntecipadoCollectionItem;
begin
  Result := TgPagAntecipadoCollectionItem(inherited Items[Index]);
end;

function TgPagAntecipadoCollection.New: TgPagAntecipadoCollectionItem;
begin
  Result := TgPagAntecipadoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TgPagAntecipadoCollection.SetItem(Index: Integer;
  Value: TgPagAntecipadoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgDif }

procedure TgDif.Assign(Source: TgDif);
begin
  FpDif := Source.pDif;
  FvDif := Source.vDif;
end;

{ TgDevTrib }

procedure TgDevTrib.Assign(Source: TgDevTrib);
begin
  FvDevTrib := Source.vDevTrib; 
end;

{ TgRed }

procedure TgRed.Assign(Source: TgRed);
begin
  FpRedAliq := Source.pRedAliq;
  FpAliqEfet := Source.pAliqEfet;
end;

{ TgIBSCBSCredPres }

procedure TgIBSCBSCredPres.Assign(Source: TgIBSCBSCredPres);
begin
  FcCredPres := Source.cCredPres;
  FpCredPres := Source.pCredPres;
  FvCredPres := Source.vCredPres;
  FvCredPresCondSus := Source.vCredPresCondSus;
end;

{ TgTribRegular }

procedure TgTribRegular.Assign(Source: TgTribRegular);
begin
  CSTReg := Source.CSTReg;
  cClassTribReg := Source.cClassTribReg;
  pAliqEfetRegIBSUF := Source.pAliqEfetRegIBSUF;
  vTribRegIBSUF := Source.vTribRegIBSUF;
  pAliqEfetRegIBSMun := Source.pAliqEfetRegIBSMun;
  vTribRegIBSMun := Source.vTribRegIBSMun;
  pAliqEfetRegCBS := Source.pAliqEfetRegCBS;
  vTribRegCBS := Source.vTribRegCBS;
end;

{ TgTribCompraGov }

procedure TgTribCompraGov.Assign(Source: TgTribCompraGov);
begin
  pAliqIBSUF := Source.pAliqIBSUF;
  vTribIBSUF := Source.vTribIBSUF;
  pAliqIBSMun := Source.pAliqIBSMun;
  vTribIBSMun := Source.vTribIBSMun;
  pAliqCBS := Source.pAliqCBS;
  vTribCBS := Source.vTribCBS;
end;

{ TDFeReferenciado }

procedure TDFeReferenciado.Assign(Source: TDFeReferenciado);
begin
  FchaveAcesso := Source.chaveAcesso;
  FnItem := Source.nItem;
end;

{ TgIBSUFTot }

procedure TgIBSUFTot.Assign(Source: TgIBSUFTot);
begin
  FvDif := Source.vDif;
  FvDevTrib := Source.vDevTrib;
  FvIBSUF := Source.vIBSUF;
end;

{ TgIBSMunTot }

procedure TgIBSMunTot.Assign(Source: TgIBSMunTot);
begin
  FvDif := Source.vDif;
  FvDevTrib := Source.vDevTrib;
  FvIBSMun := Source.vIBSMun;
end;

{ TgCBSTot }

procedure TgCBSTot.Assign(Source: TgCBSTot);
begin
  FvDif := Source.vDif;
  FvDevTrib := Source.vDevTrib;
  FvCBS := Source.vCBS;
  FvCredPres := Source.vCredPres;
  FvCredPresCondSus := Source.vCredPresCondSus;
end;

{ TgMono }

procedure TgMono.Assign(Source: TgMono);
begin
  FvIBSMono := Source.vIBSMono;
  FvCBSMono := Source.vCBSMono;
  FvIBSMonoReten := Source.vIBSMonoReten;
  FvCBSMonoReten := Source.vCBSMonoReten;
  FvIBSMonoRet := Source.vIBSMonoRet;
  FvCBSMonoRet := Source.vCBSMonoRet;
end;

end.
