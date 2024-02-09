{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: André Ferreira de Moraes                        }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esse arquivo usa a classe  PCN (c) 2009 - Paulo Casagrande                  }
{  PCN - Projeto Cooperar NFe       (Found at URL:  www.projetocooperar.org)   }
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
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit pcnCFe;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}      
  pcnConversao, pcnSignature, ACBrBase;

type

  TCFe = class;
  TInfCFe = class;
  Tide = class;
  TEmit = class;
  TenderEmit = class;
  TDest = class;
  TEntrega = class;
  TDetCollection = class;
  TDetCollectionItem = class;
  TProd = class;
  TobsFiscoDetCollection = class;
  TobsFiscoDetCollectionItem = class;
  TImposto = class;
  TICMS = class;
  TPIS = class;
  TPISST = class;
  TCOFINS = class;
  TCOFINSST = class;
  TISSQN = class;
  TTotal = class;
  TICMSTot = class;
  TISSQNtot = class;
  TDescAcrEntr = class;
  TMPCollection = class;
  TMPCollectionItem = class;
  TInfAdic = class;
  TobsFiscoCollection = class;
  TobsFiscoCollectionItem = class;
  TLoteCFeCollection = class;

  { TCFe }

  TCFe = class
  private
    FIdentarXML: boolean;
    FinfCFe: TinfCFe;
    Fide: Tide;
    FEmit: TEmit;
    FDest: TDest;
    FEntrega: TEntrega;
    FDet: TDetCollection;
    FNomeArquivo: String;
    FRetirarAcentos: boolean;
    FRetirarEspacos: boolean;
    FTamanhoIdentacao: integer;
    FTotal: TTotal;
    fPagto: TMPCollection;
    FInfAdic: TInfAdic;
    FobsFisco: TobsFiscoCollection;
    FSignature: TSignature;
    FXMLOriginal: AnsiString;
    FAjustarTagNro:Boolean;

    function GetAsXMLString: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearSessao;

    function LoadFromFile(const AFileName : String): boolean;
    function LoadFromIni(const IniArquivoOuString : String): boolean;
    function SaveToFile(const AFileName : String): boolean;
    function GerarXML( ApenasTagsAplicacao: Boolean = false) : AnsiString;
    procedure SetXMLString(const AValue : AnsiString);

    property NomeArquivo: String read FNomeArquivo write FNomeArquivo;
    property AsXMLString : AnsiString read GetAsXMLString write SetXMLString;
    property XMLOriginal: AnsiString read FXMLOriginal;
    property infCFe: TinfCFe read FinfCFe;
    property ide: Tide read Fide;
    property Emit: TEmit read FEmit;
    property Dest: TDest read FDest;
    property Entrega: TEntrega read FEntrega;
    property Det: TDetCollection read FDet;
    property Total: TTotal read FTotal;
    property Pagto: TMPCollection read fPagto;
    property InfAdic: TInfAdic read FInfAdic;
    property obsFisco: TobsFiscoCollection read FobsFisco;
    property signature: Tsignature read Fsignature;

    property RetirarAcentos: boolean read FRetirarAcentos write FRetirarAcentos;
    property RetirarEspacos: boolean read FRetirarEspacos write FRetirarEspacos;
    property IdentarXML: boolean read FIdentarXML write FIdentarXML;
    property TamanhoIdentacao: integer read FTamanhoIdentacao write FTamanhoIdentacao;
    property AjustarTagNro: boolean read FAjustarTagNro write FAjustarTagNro;
  end;

  { TinfCFe }

  TinfCFe = class
  private
    Fversao : Currency;
    FversaoDadosEnt : Currency;
    FversaoSB : integer;
    FID: string;
  public
    constructor Create;
    procedure Clear;

    property versao: Currency read Fversao write Fversao;
    property versaoDadosEnt: Currency read FversaoDadosEnt write FversaoDadosEnt;
    property versaoSB: integer read FversaoSB write FversaoSB;
    property ID: string read FID write FID;
  end;

  { Tide }

  Tide = class
  private
    FcUF: integer;
    FcNF: integer;
    Fmodelo: integer;
    FnserieSAT: integer;
    FnCFe: integer;
    FdhEmi: TDateTime;
    FcDV: integer;
    FtpAmb: TpcnTipoAmbiente;
    FCNPJ: string;
    FsignAC: string;
    FassinaturaQRCODE: string;
    FnumeroCaixa: integer;
    function GetdEmi : TDateTime;
    function GethEmi : TDateTime;
    procedure SetdEmi(AValue : TDateTime);
    procedure SethEmi(AValue : TDateTime);
  public
    constructor Create;
    procedure Clear;
    procedure ClearSessao;

    property cUF: integer read FcUF write FcUF;
    property cNF: integer read FcNF write FcNF;
    property modelo: integer read Fmodelo write Fmodelo;
    property nserieSAT: integer read FnserieSAT write FnserieSAT;
    property nCFe: integer read FnCFe write FnCFe;
    property dEmi: TDateTime read GetdEmi write SetdEmi;
    property hEmi: TDateTime read GethEmi write SethEmi;
    property cDV: integer read FcDV write FcDV;
    property tpAmb: TpcnTipoAmbiente read FtpAmb write FtpAmb default taHomologacao;
    property CNPJ: string read FCNPJ write FCNPJ;
    property signAC: string read FsignAC write FsignAC;
    property assinaturaQRCODE: string read FassinaturaQRCODE write FassinaturaQRCODE;
    property numeroCaixa: integer read FnumeroCaixa write FnumeroCaixa;
  end;

  { TEmit }

  TEmit = class
  private
    FCNPJ: string;
    FxNome: string;
    FxFant: string;
    FEnderEmit: TenderEmit;
    FIE: string;
    FIM: string;
    FcRegTrib: TpcnRegTrib;
    FcRegTribISSQN: TpcnRegTribISSQN;
    FindRatISSQN: TpcnindRatISSQN;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    property CNPJ: string read FCNPJ write FCNPJ;
    property xNome: string read FxNome write FxNome;
    property xFant: string read FxFant write FxFant;
    property EnderEmit: TEnderEmit read FEnderEmit;
    property IE: string read FIE write FIE;
    property IM: string read FIM write FIM;
    property cRegTrib: TpcnRegTrib read FcRegTrib write FcRegTrib;
    property cRegTribISSQN: TpcnRegTribISSQN read FcRegTribISSQN write FcRegTribISSQN;
    property indRatISSQN: TpcnindRatISSQN read FindRatISSQN write FindRatISSQN;
  end;

  { TenderEmit }

  TenderEmit = class
  private
    FxLgr: string;
    Fnro: string;
    fxCpl: string;
    FxBairro: string;
    FxMun: string;
    FCEP: integer;
  public
    constructor Create;
    procedure Clear;

    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property xMun: string read FxMun write FxMun;
    property CEP: integer read FCEP write FCEP;
  end;

  { TDest }

  TDest = class
  private
    FCNPJCPF: string;
    FxNome: string;
  public
    constructor Create;
    procedure Clear;

    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property xNome: string read FxNome write FxNome;
  end;

  { TEntrega }

  TEntrega = class
  private
    FxLgr: string;
    Fnro: string;
    fxCpl: string;
    FxBairro: string;
    FxMun: string;
    FUF: string;
  public
    constructor Create;
    procedure Clear;

    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property xMun: string read FxMun write FxMun;
    property UF: string read FUF write FUF;
  end;

  TDetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDetCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetCollectionItem);
  public
    function Add: TDetCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDetCollectionItem;
    property Items[Index: Integer]: TDetCollectionItem read GetItem write SetItem; default;
  end;

  { TDetCollectionItem }

  TDetCollectionItem = class(TObject)
  private
    FnItem: integer;
    FProd: TProd;
    FImposto: TImposto;
    FinfAdProd: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    property nItem: integer read FnItem write FnItem;
    property Prod: TProd read FProd;
    property Imposto: TImposto read FImposto;
    property infAdProd: string read FinfAdProd write FinfAdProd;
  end;

  { TProd }

  TProd = class
  private
    FcProd: string;
    FcEAN: string;
    FEhCombustivel: Boolean;
    FxProd: string;
    FNCM: string;
    FCEST: string;
    FCFOP: string;
    FuCom: string;
    FqCom: currency;
    FvUnCom: double;
    FvProd: currency;
    FindRegra: TpcnindRegra;
    FvDesc: currency;
    FvOutro: currency;
    FvItem: currency;
    FvRatDesc: currency;
    FvRatAcr: currency;
    FobsFiscoDet: TobsFiscoDetCollection;
  public
    constructor Create(AOwner: TDetcollectionItem);
    destructor Destroy; override;
    procedure Clear;

    property cProd: string read FcProd write FcProd;
    property cEAN: string read FcEAN write FcEAN;
    property xProd: string read FxProd write FxProd;
    property NCM: string read FNCM write FNCM;
    property CEST: string read FCEST write FCEST;
    property CFOP: string read FCFOP write FCFOP;
    property uCom: string read FuCom write FuCom;
    property EhCombustivel: Boolean read FEhCombustivel write FEhCombustivel;
    property qCom: currency read FqCom write FqCom;
    property vUnCom: double read FvUnCom write FvUnCom;
    property vProd: currency read FvProd write FvProd;
    property indRegra : TpcnindRegra read FindRegra write FindRegra;
    property vDesc: currency read FvDesc write FvDesc;
    property vOutro: currency read FvOutro write FvOutro;
    property vItem: currency read FvItem write FvItem;
    property vRatDesc: currency read FvRatDesc write FvRatDesc;
    property vRatAcr: currency read FvRatAcr write FvRatAcr;
    property obsFiscoDet: TobsFiscoDetCollection read FobsFiscoDet;
  end;

  { TobsFiscoDetCollection }

  TobsFiscoDetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TobsFiscoDetCollectionItem;
    procedure SetItem(Index: Integer; Value: TobsFiscoDetCollectionItem);
  public
    destructor Destroy; override;

    function Add: TobsFiscoDetCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TobsFiscoDetCollectionItem;
    property Items[Index: Integer]: TobsFiscoDetCollectionItem read GetItem write SetItem; default;
  end;

  { TobsFiscoDetCollectionItem }

  TobsFiscoDetCollectionItem = class(TObject)
  private
    FxCampoDet: string;
    FxTextoDet: string;
  public
    property xCampoDet: string read FxCampoDet write FxCampoDet;
    property xTextoDet: string read FxTextoDet write FxTextoDet;
  end;

  { TImposto }

  TImposto = class
  private
    FvItem12741: currency;
    FICMS: TICMS;
    FPIS: TPIS;
    FPISST: TPISST;
    FCOFINS: TCOFINS;
    FCOFINSST: TCOFINSST;
    FISSQN: TISSQN;
  public
    constructor Create(AOwner: TDetcollectionItem);
    destructor Destroy; override;
    procedure Clear;

    property vItem12741: currency read FvItem12741 write FvItem12741;
    property ICMS: TICMS read FICMS;
    property PIS: TPIS read FPIS;
    property PISST: TPISST read FPISST;
    property COFINS: TCOFINS read FCOFINS;
    property COFINSST: TCOFINSST read FCOFINSST;
    property ISSQN: TISSQN read FISSQN;
  end;

  { TICMS }

  TICMS = class
  private
    Forig: TpcnOrigemMercadoria;
    FCST: TpcnCSTIcms;
    FCSOSN: TpcnCSOSNIcms;
    FpICMS: currency;
    FvICMS: currency;
  public
    constructor Create;
    procedure Clear;

    property orig: TpcnOrigemMercadoria read Forig write Forig default oeNacional;
    property CST: TpcnCSTIcms read FCST write FCST default cst00;
    property CSOSN: TpcnCSOSNIcms read FCSOSN write FCSOSN;
    property pICMS: currency read FpICMS write FpICMS;
    property vICMS: currency read FvICMS write FvICMS;
  end;

  { TPIS }

  TPIS = class
  private
    FCST: TpcnCstPis;
    FvBC: currency;
    FpPIS: currency;
    FvPIS: currency;
    FqBCProd: currency;
    FvAliqProd: currency;
  public
    constructor Create;
    procedure Clear;

    property CST: TpcnCstPis read FCST write FCST default pis01;
    property vBC: currency read FvBC write FvBC;
    property pPIS: currency read FpPIS write FpPIS;
    property vPIS: currency read FvPIS write FvPIS;
    property qBCProd: currency read FqBCProd write FqBCProd;
    property vAliqProd: currency read FvAliqProd write FvAliqProd;
  end;

  { TPISST }

  TPISST = class
  private
    FvBc: currency;
    FpPis: currency;
    FqBCProd: currency;
    FvAliqProd: currency;
    FvPIS: currency;
  public
    constructor Create;
    procedure Clear;

    property vBc: currency read FvBc write FvBc;
    property pPis: currency read FpPis write FpPis;
    property qBCProd: currency read FqBCProd write FqBCProd;
    property vAliqProd: currency read FvAliqProd write FvAliqProd;
    property vPIS: currency read FvPIS write FvPIS;
  end;

  { TCOFINS }

  TCOFINS = class
  private
    FCST: TpcnCstCofins;
    FvBC: currency;
    FpCOFINS: currency;
    FvCOFINS: currency;
    FvAliqProd: currency;
    FqBCProd: currency;
  public
    constructor Create;
    procedure Clear;

    property CST: TpcnCstCofins read FCST write FCST default cof01;
    property vBC: currency read FvBC write FvBC;
    property pCOFINS: currency read FpCOFINS write FpCOFINS;
    property vCOFINS: currency read FvCOFINS write FvCOFINS;
    property vAliqProd: currency read FvAliqProd write FvAliqProd;
    property qBCProd: currency read FqBCProd write FqBCProd;
  end;

  { TCOFINSST }

  TCOFINSST = class
  private
    FvBC: currency;
    FpCOFINS: currency;
    FqBCProd: currency;
    FvAliqProd: currency;
    FvCOFINS: currency;
  public
    constructor Create;
    procedure Clear;

    property vBC: currency read FvBC write FvBC;
    property pCOFINS: currency read FpCOFINS write FpCOFINS;
    property qBCProd: currency read FqBCProd write FqBCProd;
    property vAliqProd: currency read FvAliqProd write FvAliqProd;
    property vCOFINS: currency read FvCOFINS write FvCOFINS;
  end;

  { TISSQN }

  TISSQN = class
  private
    FvDeducISSQN: currency;
    FvBC: currency;
    FvAliq: currency;
    FvISSQN: currency;
    FcMunFG: integer;
    FcListServ: String;
    FcServTribMun: string;
    FcNatOp: integer;
    FindIncFisc: TpcnindIncentivo;
  public
    constructor Create;
    procedure Clear;

    property vDeducISSQN: currency read FvDeducISSQN write FvDeducISSQN;
    property vBC: currency read FvBC write FvBC;
    property vAliq: currency read FvAliq write FvAliq;
    property vISSQN: currency read FvISSQN write FvISSQN;
    property cMunFG: integer read FcMunFG write FcMunFG;
    property cListServ: String read FcListServ write FcListServ;
    property cServTribMun: string read FcServTribMun write FcServTribMun;
    property cNatOp: integer read FcNatOp write FcNatOp;
    property indIncFisc: TpcnindIncentivo read FindIncFisc write FindIncFisc;
  end;

  { TTotal }

  TTotal = class
  private
    FICMSTot: TICMSTot;
    FvCFe: Currency;
    FISSQNtot: TISSQNtot;
    FDescAcrEntr: TDescAcrEntr;
    FvCFeLei12741: Currency;
  public
    constructor Create(AOwner: TCFe);
    destructor Destroy; override;
    procedure Clear;

    property ICMSTot: TICMSTot read FICMSTot;
    property vCFe: Currency read FvCFe write FvCFe;
    property ISSQNtot: TISSQNtot read FISSQNtot;
    property DescAcrEntr: TDescAcrEntr read FDescAcrEntr;
    property vCFeLei12741: Currency read FvCFeLei12741 write FvCFeLei12741;
  end;

  { TICMSTot }

  TICMSTot = class
  private
    FvICMS: Currency;
    FvProd: Currency;
    FvDesc: Currency;
    FvPIS: Currency;
    FvCOFINS: Currency;
    FvPISST: Currency;
    FvCOFINSST: Currency;
    FvOutro: Currency;
  public
    constructor Create;
    procedure Clear;

    property vICMS: Currency read FvICMS write FvICMS;
    property vProd: Currency read FvProd write FvProd;
    property vDesc: Currency read FvDesc write FvDesc;
    property vPIS: Currency read FvPIS write FvPIS;
    property vCOFINS: Currency read FvCOFINS write FvCOFINS;
    property vPISST: Currency read FvPISST write FvPISST;
    property vCOFINSST: Currency read FvCOFINSST write FvCOFINSST;
    property vOutro: Currency read FvOutro write FvOutro;
  end;

  { TISSQNtot }

  TISSQNtot = class
  private
    FvBC: Currency;
    FvISS: Currency;
    FvPIS: Currency;
    FvCOFINS: Currency;
    FvPISST: Currency;
    FvCOFINSST: Currency;
  public
    constructor Create;
    procedure Clear;

    property vBC: Currency read FvBC write FvBC;
    property vISS: Currency read FvISS write FvISS;
    property vPIS: Currency read FvPIS write FvPIS;
    property vCOFINS: Currency read FvCOFINS write FvCOFINS;
    property vPISST: Currency read FvPIS write FvPIS;
    property vCOFINSST: Currency read FvCOFINS write FvCOFINS;
  end;

  { TDescAcrEntr }

  TDescAcrEntr = class
  private
    FvDescSubtot: Currency;
    FvAcresSubtot: Currency;
  public
    constructor Create;
    procedure Clear;

    property vDescSubtot: Currency read FvDescSubtot write FvDescSubtot;
    property vAcresSubtot: Currency read FvAcresSubtot write FvAcresSubtot;
  end;

  { TMPCollection }

  TMPCollection = class(TACBrObjectList)
  private
    FvTroco: currency;
    function GetItem(Index: Integer): TMPCollectionItem;
    procedure SetItem(Index: Integer; Value: TMPCollectionItem);
  public
    constructor Create();
    destructor Destroy; override;

    procedure Clear; override;
    function Add: TMPCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TMPCollectionItem;
    property Items[Index: Integer]: TMPCollectionItem read GetItem write SetItem; default;
    property vTroco: currency read FvTroco write FvTroco;
  end;

  { TMPCollectionItem }

  TMPCollectionItem = class(TObject)
  private
    FcMP: TpcnCodigoMP;
    FvMP: currency;
    FcAdmC: integer;
    FcAut: string;
  public
    property cMP: TpcnCodigoMP read FcMP write FcMP;
    property vMP: currency read FvMP write FvMP;
    property cAdmC: integer read FcAdmC write FcAdmC;
    property cAut: string read FcAut write FcAut;
  end;

  { TInfAdic }

  TInfAdic = class
  private
    FinfCpl: string;
    FobsFisco: TobsFiscoCollection;
  public
    constructor Create(AOwner: TCFe);
    destructor Destroy; override;
    procedure Clear;

    property infCpl: string read FinfCpl write FinfCpl;
    property obsFisco: TobsFiscoCollection read FobsFisco;
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

  { TobsFiscoCollectionItem }

  TobsFiscoCollectionItem = class(TObject)
  private
    FxCampo: string;
    FxTexto: string;
  public
    property xCampo: string read FxCampo write FxCampo;
    property xTexto: string read FxTexto write FxTexto;
  end;

  { TLoteCFeCollection }

  TLoteCFeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TCFe;
    procedure SetItem(Index: Integer; Value: TCFe);
  public
    function New: TCFe;
    property Items[Index: Integer]: TCFe read GetItem write SetItem; default;
  end;

implementation

uses
  dateutils, IniFiles,
  pcnCFeR, pcnCFeW,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.XMLHTML;

{ TDescAcrEntr }

constructor TDescAcrEntr.Create;
begin
  inherited Create;
  Clear;
end;

procedure TDescAcrEntr.Clear;
begin
  FvDescSubtot  := 0;
  FvAcresSubtot := 0;
end;

{ TISSQNtot }

constructor TISSQNtot.Create;
begin
  inherited Create;
  Clear;
end;

procedure TISSQNtot.Clear;
begin
  FvBC      := 0;
  FvISS     := 0;
  FvPIS     := 0;
  FvCOFINS  := 0;
  FvPISST   := 0;
  FvCOFINSST:= 0;
end;

{ TICMS }

constructor TICMS.Create;
begin
  inherited Create;
  Clear;
end;

procedure TICMS.Clear;
begin
  Forig   := oeNacional;
  FCST    := cst00;
  FCSOSN  := csosnVazio;
  FpICMS  := 0;
  FvICMS  := 0;
end;

{ TPIS }

constructor TPIS.Create;
begin
  inherited Create;
  Clear;
end;

procedure TPIS.Clear;
begin
  FCST       := pis01;
  FvBC       := 0;
  FpPIS      := 0;
  FvPIS      := 0;
  FqBCProd   := 0;
  FvAliqProd := 0;
end;

{ TPISST }

constructor TPISST.Create;
begin
  inherited Create;
  Clear;
end;

procedure TPISST.Clear;
begin
  FvBc       := 0;
  FpPis      := 0;
  FqBCProd   := 0;
  FvAliqProd := 0;
  FvPIS      := 0;
end;

{ TCOFINS }

constructor TCOFINS.Create;
begin
  inherited create;
  Clear;
end;

procedure TCOFINS.Clear;
begin
  FCST       := cof01;
  FvBC       := 0;
  FpCOFINS   := 0;
  FvCOFINS   := 0;
  FvAliqProd := 0;
  FqBCProd   := 0;
end;

{ TCOFINSST }

constructor TCOFINSST.Create;
begin
  inherited Create;
  Clear;
end;

procedure TCOFINSST.Clear;
begin
  FvBC      := 0;
  FpCOFINS  := 0;
  FqBCProd  := 0;
  FvAliqProd:= 0;
  FvCOFINS  := 0;
end;

{ TISSQN }

constructor TISSQN.Create;
begin
  inherited create;
  Clear;
end;

procedure TISSQN.Clear;
begin
  FvDeducISSQN := 0;
  FvBC         := 0;
  FvAliq       := 0;
  FvISSQN      := 0;
  FcMunFG      := 0;
  FcListServ   := '';
  FcServTribMun:= '';
  FcNatOp      := 0;
  FindIncFisc  := iiNao;
end;

{ TICMSTot }

constructor TICMSTot.Create;
begin
  inherited Create;
  Clear;
end;

procedure TICMSTot.Clear;
begin
  FvICMS     := 0;
  FvProd     := 0;
  FvDesc     := 0;
  FvPIS      := 0;
  FvCOFINS   := 0;
  FvPISST    := 0;
  FvCOFINSST := 0;
  FvOutro    := 0;
end;

{ TEntrega }

constructor TEntrega.Create;
begin
  inherited Create;
  Clear;
end;

procedure TEntrega.Clear;
begin
  FxLgr   := '';
  Fnro    := '';
  fxCpl   := '';
  FxBairro:= '';
  FxMun   := '';
  FUF     := '';
end;

{ TDest }

constructor TDest.Create;
begin
  inherited Create;
  Clear;
end;

procedure TDest.Clear;
begin
  FCNPJCPF := '';
  FxNome   := '';
end;

{ TenderEmit }

constructor TenderEmit.Create;
begin
  inherited Create;
  Clear;
end;

procedure TenderEmit.Clear;
begin
  FxLgr   := '';
  Fnro    := '';
  fxCpl   := '';
  FxBairro:= '';
  FxMun   := '';
  FCEP    := 0;
end;

{ Tide }

function Tide.GetdEmi : TDateTime;
begin
  Result := DateOf( FdhEmi );
end;

function Tide.GethEmi : TDateTime;
begin
  Result := TimeOf( FdhEmi );
end;

procedure Tide.SetdEmi(AValue : TDateTime);
begin
 FdhEmi := DateOf(AValue) + hEmi;
end;

procedure Tide.SethEmi(AValue : TDateTime);
begin
  FdhEmi := dEmi + TimeOf(AValue);
end;

constructor Tide.Create;
begin
  inherited Create;
  Clear;
end;

procedure Tide.Clear;
begin
  FcUF              := 0;
  Fmodelo           := 0;
  FnserieSAT        := 0;
  FtpAmb            := taHomologacao;
  FCNPJ             := '';
  FsignAC           := '';
  FnumeroCaixa      := 0;
  ClearSessao;
end;

procedure Tide.ClearSessao;
begin
  FcNF              := 0;
  FnCFe             := 0;
  FdhEmi            := 0;
  FcDV              := 0;
  FassinaturaQRCODE := '';
end;

{ TinfCFe }

constructor TinfCFe.Create;
begin
  inherited;
  Clear;
end;

procedure TinfCFe.Clear;
begin
  Fversao         := 0;
  FversaoDadosEnt := 0;
  FversaoSB       := 0;
  FID             := '';
end;

{ TobsFiscoCollection }

function TobsFiscoCollection.GetItem(
  Index: Integer): TobsFiscoCollectionItem;
begin
  Result := TobsFiscoCollectionItem(inherited Items[Index]);
end;

procedure TobsFiscoCollection.SetItem(Index: Integer;
  Value: TobsFiscoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TobsFiscoCollection.Add: TobsFiscoCollectionItem;
begin
  Result := Self.New;
end;

function TobsFiscoCollection.New: TobsFiscoCollectionItem;
begin
  Result := TobsFiscoCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfAdic }

constructor TInfAdic.Create(AOwner: TCFe);
begin
  inherited Create;
  FobsFisco := TobsFiscoCollection.Create;
  Clear;
end;

destructor TInfAdic.Destroy;
begin
  FobsFisco.Free;
  inherited;
end;

procedure TInfAdic.Clear;
begin
  FinfCpl  := '';
  FobsFisco.Clear;
end;

{ TTotal }

constructor TTotal.Create(AOwner: TCFe);
begin
  inherited Create;
  FICMSTot := TICMSTot.Create;
  FISSQNtot := TISSQNtot.Create;
  FDescAcrEntr := TDescAcrEntr.create;
end;

destructor TTotal.Destroy;
begin
  FICMSTot.Free;
  FISSQNtot.Free;
  FDescAcrEntr.Free;
  inherited;
end;

procedure TTotal.Clear;
begin
  FvCFe         := 0;
  FvCFeLei12741 := 0;

  FICMSTot.Clear;
  FISSQNtot.Clear;
  FDescAcrEntr.Clear;
end;

{ TImposto }

constructor TImposto.Create(AOwner: TDetcollectionItem);
begin
  inherited Create;
  FICMS := TICMS.Create;
  FPIS := TPIS.Create;
  FPISST := TPISST.Create;
  FCOFINS := TCOFINS.Create;
  FCOFINSST := TCOFINSST.Create;
  FISSQN := TISSQN.create;
end;

destructor TImposto.Destroy;
begin
  FICMS.Free;
  FPIS.Free;
  FPISST.Free;
  FCOFINS.Free;
  FCOFINSST.Free;
  FISSQN.Free;
  inherited;
end;

procedure TImposto.Clear;
begin
  FvItem12741 := 0;
  FICMS.Clear;
  FPIS.Clear;
  FPISST.Clear;
  FCOFINS.Clear;
  FCOFINSST.Clear;
  FISSQN.Clear;
end;

{ TProd }

constructor TProd.Create(AOwner: TDetcollectionItem);
begin
  inherited Create;
  FobsFiscoDet := TobsFiscoDetCollection.Create;
  Clear;
end;

destructor TProd.Destroy;
begin
  FobsFiscoDet.Free;
  inherited;
end;

procedure TProd.Clear;
begin
  FcProd    := '';
  FcEAN     := '';
  FxProd    := '';
  FNCM      := '';
  FCEST     := '';
  FCFOP     := '';
  FuCom     := '';
  FqCom     := 0;
  FvUnCom   := 0;
  FvProd    := 0;
  FindRegra := irArredondamento;
  FvDesc    := 0;
  FvOutro   := 0;
  FvItem    := 0;
  FvRatDesc := 0;
  FvRatAcr  := 0;
  FobsFiscoDet.Clear;
  FEhCombustivel := False;
end;

{ TobsFiscoDetCollection }

destructor TobsFiscoDetCollection.Destroy;
begin
  inherited Destroy;
end;

function TobsFiscoDetCollection.GetItem(
  Index: Integer): TobsFiscoDetCollectionItem;
begin
  Result := TobsFiscoDetCollectionItem(inherited Items[Index]);
end;

procedure TobsFiscoDetCollection.SetItem(Index: Integer;
  Value: TobsFiscoDetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TobsFiscoDetCollection.New: TobsFiscoDetCollectionItem;
begin
  Result := TobsFiscoDetCollectionItem.Create;
  Self.Add(Result);
end;

function TobsFiscoDetCollection.Add: TobsFiscoDetCollectionItem;
begin
  Result := Self.New;
end;

{ TDetCollection }

function TDetCollection.New: TDetCollectionItem;
begin
  Result := TDetCollectionItem.Create;
  Self.Add(Result);
end;

function TDetCollection.GetItem(Index: Integer): TDetCollectionItem;
begin
  Result := TDetCollectionItem(inherited Items[Index]);
end;

procedure TDetCollection.SetItem(Index: Integer;
  Value: TDetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDetCollection.Add: TDetCollectionItem;
begin
  Result := Self.New;
end;

{ TDetCollectionItem }

constructor TDetCollectionItem.Create;
begin
  inherited Create;
  FProd := TProd.Create(self);
  FImposto := TImposto.Create(self);
  Clear;
end;

destructor TDetCollectionItem.Destroy;
begin
  FProd.Free;
  FImposto.Free;

  inherited;
end;

procedure TDetCollectionItem.Clear;
begin
  FnItem     := 0;
  FinfAdProd := '';

  FProd.Clear;
  FImposto.Clear;
end;

{ TEmit }

constructor TEmit.Create;
begin
  inherited Create;
  FenderEmit := TenderEmit.Create;
end;

destructor TEmit.Destroy;
begin
  FEnderEmit.Free;
  inherited;
end;

procedure TEmit.Clear;
begin
  FCNPJ  := '';
  FxNome    := '';
  FxFant    := '';
  FIE       := '';
  FIM       := '';
  FenderEmit.Clear;
  FcRegTrib      := RTSimplesNacional;
  FcRegTribISSQN := RTISSNenhum;
  FindRatISSQN   := irSim;
end;

{ TMPCollection }

procedure TMPCollection.Clear;
begin
   inherited Clear;
   FvTroco := 0;
end;

constructor TMPCollection.Create();
begin
  inherited Create();
  FvTroco := 0;
end;

destructor TMPCollection.Destroy;
begin

  inherited;
end;

function TMPCollection.GetItem(Index: Integer): TMPCollectionItem;
begin
  Result := TMPCollectionItem(inherited Items[Index]);
end;

procedure TMPCollection.SetItem(Index: Integer; Value: TMPCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TMPCollection.New: TMPCollectionItem;
begin
  Result := TMPCollectionItem.Create();
  Self.Add(Result);
end;

function TMPCollection.Add: TMPCollectionItem;
begin
  Result := Self.New;
end;

{ TCFe }

constructor TCFe.Create;
begin
  FinfCFe  := TInfCFe.Create;
  FIde     := Tide.Create;
  FEmit    := TEmit.Create;
  FDest    := TDest.Create;
  FEntrega := TEntrega.Create;
  FDet     := TDetCollection.Create();
  FTotal   := TTotal.Create(self);
  fPagto   := TMPCollection.Create;
  FinfAdic := TinfAdic.Create(self);
  FobsFisco := TobsFiscoCollection.Create;
  Fsignature := Tsignature.Create;

  FRetirarAcentos := True;
  FRetirarEspacos := True;
  FIdentarXML := False;
  FTamanhoIdentacao := 3;
  FAjustarTagNro := True;

  Clear;
end;

destructor TCFe.Destroy;
begin
  FinfCFe.Free;
  Fide.Free;
  FEmit.Free;
  FDest.Free;
  FEntrega.Free;
  FDet.Free;
  FTotal.Free;
  fPagto.Free;
  FinfAdic.Free;
  FobsFisco.Free;
  Fsignature.Free;
  inherited Destroy;
end;

procedure TCFe.Clear;
begin
  FinfCFe.Clear;
  Fide.Clear;
  FEmit.Clear;
  ClearSessao;
end;

procedure TCFe.ClearSessao;
begin
  FXMLOriginal := '';
  FNomeArquivo := '';

  Fide.ClearSessao;
  FDest.Clear;
  FEntrega.Clear;
  FDet.Clear;
  FTotal.Clear;
  fPagto.Clear;
  FInfAdic.Clear;
  FobsFisco.Clear;
  FSignature.Clear;
end;

function TCFe.LoadFromFile(const AFileName : String) : boolean;
var
  SL: TStringList;
begin
  Result := False;

  SL := TStringList.Create;
  try
    SL.LoadFromFile( AFileName );
    AsXMLString := SL.Text;
    FNomeArquivo := AFileName;
    Result := True;
  finally
    SL.Free;
  end;
end;

function TCFe.LoadFromIni(const IniArquivoOuString : String): boolean;
var
  INIRec : TMemIniFile;
  OK     : Boolean;
  I, J   : Integer;
  sSecao, sFim, sCodPro : String;
begin
  INIRec := TMemIniFile.Create(' ');
  LerIniArquivoOuString(IniArquivoOuString, INIRec);
  try
    try
      infCFe.versaoDadosEnt := StringToFloatDef(INIRec.ReadString('infCFe','versao',''), infCFe.versaoDadosEnt);
      Ide.cUF        := INIRec.ReadInteger( 'Identificacao','cUF' ,UFparaCodigoUF(INIRec.ReadString(  'Emitente','UF', CodigoUFparaUF(Ide.cUF))));
      Ide.cNF        := INIRec.ReadInteger( 'Identificacao','cNF' ,INIRec.ReadInteger( 'Identificacao','Codigo' ,Ide.cNF));
      Ide.modelo     := INIRec.ReadInteger( 'Identificacao','mod' ,INIRec.ReadInteger( 'Identificacao','Modelo' ,Ide.modelo));
      Ide.nserieSAT  := INIRec.ReadInteger( 'Identificacao','nserieSAT'  ,Ide.nserieSAT);
      Ide.nCFe       := INIRec.ReadInteger( 'Identificacao','nCFe' ,INIRec.ReadInteger( 'Identificacao','nNF' ,Ide.nCFe));
      Ide.dEmi := StrToDateDef(INIRec.ReadString( 'Identificacao','dhEmi',INIRec.ReadString( 'Identificacao','dEmi',INIRec.ReadString( 'Identificacao','Emissao',''))),Ide.dEmi);

      if (INIRec.ValueExists('Identificacao', 'dhEmi')) then
        Ide.hEmi     := StrToDateDef(INIRec.ReadString('Identificacao','dhEmi', ''), Ide.hEmi)
      else
        Ide.hEmi     := StrToTimeDef(INIRec.ReadString('Identificacao','hEmi', ''), Ide.hEmi);

      Ide.cDV        := INIRec.ReadInteger( 'Identificacao','cDV' , Ide.cDV);
      Ide.tpAmb      := StrToTpAmb(OK,INIRec.ReadString( 'Identificacao','tpAmb',TpAmbToStr(Ide.tpAmb)));
      Ide.CNPJ       := INIRec.ReadString(  'Identificacao','CNPJ' , Ide.CNPJ);
      Ide.signAC     := INIRec.ReadString(  'Identificacao','signAC' , Ide.signAC);
      Ide.assinaturaQRCODE := INIRec.ReadString(  'Identificacao','assinaturaQRCODE' ,Ide.assinaturaQRCODE );
      Ide.numeroCaixa := INIRec.ReadInteger( 'Identificacao','numeroCaixa' , Ide.numeroCaixa);

      Emit.CNPJ              := INIRec.ReadString(  'Emitente','CNPJ',INIRec.ReadString(  'Emitente','CNPJCPF', Emit.CNPJ ));
      Emit.xNome             := INIRec.ReadString(  'Emitente','xNome',INIRec.ReadString(  'Emitente','Razao', Emit.xNome));
      Emit.xFant             := INIRec.ReadString(  'Emitente','xFant',INIRec.ReadString(  'Emitente','Fantasia', Emit.xFant));
      Emit.IE                := INIRec.ReadString(  'Emitente','IE', Emit.IE );
      Emit.IM                := INIRec.ReadString(  'Emitente','IM', Emit.IM );

      Emit.cRegTrib          := StrToRegTrib(      ok, INIRec.ReadString( 'Emitente','cRegTrib',      RegTribToStr(Emit.cRegTrib)));
      Emit.cRegTribISSQN     := StrToRegTribISSQN( ok, INIRec.ReadString( 'Emitente','cRegTribISSQN', RegTribISSQNToStr(Emit.cRegTribISSQN)));
      Emit.indRatISSQN       := StrToindRatISSQN(  ok, INIRec.ReadString( 'Emitente','indRatISSQN',   indRatISSQNToStr(Emit.indRatISSQN)));

      Emit.EnderEmit.xLgr    := INIRec.ReadString(  'Emitente','xLgr' ,INIRec.ReadString(  'Emitente','Logradouro', Emit.EnderEmit.xLgr));
      Emit.EnderEmit.nro     := INIRec.ReadString(  'Emitente','nro'  ,INIRec.ReadString(  'Emitente','Numero', Emit.EnderEmit.nro));
      Emit.EnderEmit.xCpl    := INIRec.ReadString(  'Emitente','xCpl',INIRec.ReadString(  'Emitente','Complemento', Emit.EnderEmit.xCpl));
      Emit.EnderEmit.xBairro := INIRec.ReadString(  'Emitente','xBairro',INIRec.ReadString(  'Emitente','Bairro', Emit.EnderEmit.xBairro));
      Emit.EnderEmit.xMun    := INIRec.ReadString(  'Emitente','xMun',INIRec.ReadString(  'Emitente','Cidade', Emit.EnderEmit.xMun));
      Emit.EnderEmit.CEP     := INIRec.ReadInteger( 'Emitente','CEP', Emit.EnderEmit.CEP);

      Dest.CNPJCPF           := INIRec.ReadString(  'Destinatario','CNPJ',INIRec.ReadString(  'Destinatario','CNPJCPF',INIRec.ReadString(  'Destinatario','CPF','')));
      Dest.xNome             := INIRec.ReadString(  'Destinatario','xNome',INIRec.ReadString(  'Destinatario','NomeRazao'  ,''));

      if INIRec.ReadString(  'Entrega','xLgr','') <> '' then
      begin
        Entrega.xLgr    := INIRec.ReadString(  'Entrega','xLgr','');
        Entrega.nro     := INIRec.ReadString(  'Entrega','nro' ,'');
        Entrega.xCpl    := INIRec.ReadString(  'Entrega','xCpl','');
        Entrega.xBairro := INIRec.ReadString(  'Entrega','xBairro','');
        Entrega.xMun    := INIRec.ReadString(  'Entrega','xMun','');
        Entrega.UF      := INIRec.ReadString(  'Entrega','UF','');
      end;

      I := 1;
      while true do
      begin
        sSecao  := 'Produto'+IntToStrZero(I,3);
        sCodPro := INIRec.ReadString(sSecao,'Codigo',INIRec.ReadString( sSecao,'cProd','FIM'));
        if sCodPro = 'FIM' then break;

        with Det.New do
        begin
          nItem := I;
          infAdProd     := INIRec.ReadString(sSecao,'infAdProd','');

          Prod.cProd    := INIRec.ReadString( sSecao,'cProd',INIRec.ReadString( sSecao,'Codigo',''));
          Prod.cEAN     := INIRec.ReadString( sSecao,'cEAN' ,INIRec.ReadString( sSecao,'EAN',''));
          Prod.xProd    := INIRec.ReadString( sSecao,'xProd',INIRec.ReadString( sSecao,'Descricao',''));
          Prod.NCM      := INIRec.ReadString( sSecao,'NCM'      ,'');
          Prod.CEST     := INIRec.ReadString( sSecao,'CEST'     ,'');
          Prod.CFOP     := INIRec.ReadString( sSecao,'CFOP'     ,'');
          Prod.uCom     := INIRec.ReadString( sSecao,'uCom'  ,INIRec.ReadString( sSecao,'Unidade' ,''));
          Prod.EhCombustivel := (INIRec.ReadInteger( sSecao,'Combustivel',0)=1);
          Prod.qCom     := StringToFloatDef( INIRec.ReadString(sSecao,'qCom'   ,INIRec.ReadString(sSecao,'Quantidade'  ,'')) ,0);
          Prod.vUnCom   := StringToFloatDef( INIRec.ReadString(sSecao,'vUnCom',INIRec.ReadString(sSecao,'ValorUnitario','')) ,0);
          Prod.vProd    := StringToFloatDef( INIRec.ReadString(sSecao,'vProd'   ,INIRec.ReadString(sSecao,'ValorTotal' ,'')) ,0);
          Prod.indRegra := StrToindRegra(ok, INIRec.ReadString(sSecao,'indRegra','A'));
          Prod.vDesc    := StringToFloatDef( INIRec.ReadString(sSecao,'vDesc',INIRec.ReadString(sSecao,'ValorDesconto','')) ,0);
          Prod.vOutro   := StringToFloatDef( INIRec.ReadString(sSecao,'vOutro','') ,0);
          Prod.vItem    := StringToFloatDef( INIRec.ReadString(sSecao,'vItem','') ,0);
          Prod.vRatDesc := StringToFloatDef( INIRec.ReadString(sSecao,'vRatDesc','') ,0);
          Prod.vRatAcr  := StringToFloatDef( INIRec.ReadString(sSecao,'vRatAcr','') ,0);

          Imposto.vItem12741 := StringToFloatDef( INIRec.ReadString(sSecao,'vItem12741',INIRec.ReadString(sSecao,'vTotTrib','')) ,0);

          J := 1;
          while true do
          begin
            sSecao := 'OBSFISCODET'+IntToStrZero(I,3)+IntToStrZero(J,3);
            sFim   := INIRec.ReadString(sSecao,'xCampoDet','');
            if (sFim <> '') then
            begin
              with Prod.obsFiscoDet.New do
              begin
                xCampoDet := sFim;
                xTextoDet := INIRec.ReadString(sSecao,'xTextoDet','');;
              end;
            end
            else
              Break;
            Inc(J);
          end;

          with Imposto do
          begin
            sSecao := 'ICMS'+IntToStrZero(I,3);
            sFim   := INIRec.ReadString( sSecao,'CST',INIRec.ReadString(sSecao,'CSOSN','FIM'));
            if (sFim <> 'FIM') then
            begin
              with ICMS do
              begin
                ICMS.orig  := StrToOrig(     OK, INIRec.ReadString(sSecao,'orig'    ,INIRec.ReadString(sSecao,'Origem'    ,'0' ) ));
                CST        := StrToCSTICMS(  OK, INIRec.ReadString(sSecao,'CST'       ,'00'));
                CSOSN      := StrToCSOSNIcms(OK, INIRec.ReadString(sSecao,'CSOSN'     ,''  ));     //NFe2
                ICMS.pICMS := StringToFloatDef( INIRec.ReadString(sSecao,'pICMS' ,INIRec.ReadString(sSecao,'Aliquota','')) ,0);
                ICMS.vICMS := StringToFloatDef( INIRec.ReadString(sSecao,'vICMS'    ,INIRec.ReadString(sSecao,'Valor','')) ,0);
              end;
            end;

            sSecao := 'PIS'+IntToStrZero(I,3);
            sFim   := INIRec.ReadString( sSecao,'CST','FIM');
            if (sFim <> 'FIM') then
            begin
              with PIS do
              begin
                CST :=  StrToCSTPIS(OK, INIRec.ReadString( sSecao,'CST','01'));

                PIS.vBC       := StringToFloatDef( INIRec.ReadString(sSecao,'vBC'      ,INIRec.ReadString(sSecao,'ValorBase'   ,'')) ,0);
                PIS.pPIS      := StringToFloatDef( INIRec.ReadString(sSecao,'pPIS'     ,INIRec.ReadString(sSecao,'Aliquota'    ,'')) ,0);
                PIS.qBCProd   := StringToFloatDef( INIRec.ReadString(sSecao,'qBCProd'  ,INIRec.ReadString(sSecao,'Quantidade'  ,'')) ,0);
                PIS.vAliqProd := StringToFloatDef( INIRec.ReadString(sSecao,'vAliqProd',INIRec.ReadString(sSecao,'ValorAliquota','')) ,0);
                PIS.vPIS      := StringToFloatDef( INIRec.ReadString(sSecao,'vPIS'     ,INIRec.ReadString(sSecao,'Valor'        ,'')) ,0);
              end;
            end;

            sSecao := 'PISST'+IntToStrZero(I,3);
            sFim   := INIRec.ReadString( sSecao,'ValorBase','F')+ INIRec.ReadString( sSecao,'Quantidade','IM');
            if (sFim = 'FIM') then
              sFim := INIRec.ReadString( sSecao,'vBC','F')+ INIRec.ReadString( sSecao,'qBCProd','IM');

            if (sFim <> 'FIM') then
            begin
              with PISST do
              begin
                vBc       := StringToFloatDef( INIRec.ReadString(sSecao,'vBC'  ,INIRec.ReadString(sSecao,'ValorBase'      ,'')) ,0);
                pPis      := StringToFloatDef( INIRec.ReadString(sSecao,'pPis' ,INIRec.ReadString(sSecao,'AliquotaPerc'   ,'')) ,0);
                qBCProd   := StringToFloatDef( INIRec.ReadString(sSecao,'qBCProd' ,INIRec.ReadString(sSecao,'Quantidade'  ,'')) ,0);
                vAliqProd := StringToFloatDef( INIRec.ReadString(sSecao,'vAliqProd',INIRec.ReadString(sSecao,'AliquotaValor','')) ,0);
                vPIS      := StringToFloatDef( INIRec.ReadString(sSecao,'vPIS'   ,INIRec.ReadString(sSecao,'ValorPISST'     ,'')) ,0);
              end;
            end;

            sSecao := 'COFINS'+IntToStrZero(I,3);
            sFim   := INIRec.ReadString( sSecao,'CST','FIM');
            if (sFim <> 'FIM') then
            begin
              with COFINS do
              begin
                CST := StrToCSTCOFINS(OK, INIRec.ReadString( sSecao,'CST','01'));

                COFINS.vBC       := StringToFloatDef( INIRec.ReadString(sSecao,'vBC'      ,INIRec.ReadString(sSecao,'ValorBase','')) ,0);
                COFINS.pCOFINS   := StringToFloatDef( INIRec.ReadString(sSecao,'pCOFINS'  ,INIRec.ReadString(sSecao,'Aliquota' ,'')) ,0);
                COFINS.qBCProd   := StringToFloatDef( INIRec.ReadString(sSecao,'qBCProd'  ,INIRec.ReadString(sSecao,'Quantidade','')) ,0);
                COFINS.vAliqProd := StringToFloatDef( INIRec.ReadString(sSecao,'vAliqProd',INIRec.ReadString(sSecao,'ValorAliquota','')) ,0);
                COFINS.vCOFINS   := StringToFloatDef( INIRec.ReadString(sSecao,'vCOFINS'  ,INIRec.ReadString(sSecao,'Valor'    ,'')) ,0);
              end;
            end;

            sSecao := 'COFINSST'+IntToStrZero(I,3);
            sFim   := INIRec.ReadString( sSecao,'ValorBase','F')+ INIRec.ReadString( sSecao,'Quantidade','IM');
            if (sFim = 'FIM') then
              sFim := INIRec.ReadString( sSecao,'vBC','F')+ INIRec.ReadString( sSecao,'qBCProd','IM');

            if (sFim <> 'FIM') then
            begin
              with COFINSST do
              begin
                vBC       := StringToFloatDef( INIRec.ReadString(sSecao,'vBC'    ,INIRec.ReadString(sSecao,'ValorBase'      ,'')) ,0);
                pCOFINS   := StringToFloatDef( INIRec.ReadString(sSecao,'pCOFINS' ,INIRec.ReadString(sSecao,'AliquotaPerc'  ,'')) ,0);
                qBCProd   := StringToFloatDef( INIRec.ReadString(sSecao,'qBCProd'   ,INIRec.ReadString(sSecao,'Quantidade'  ,'')) ,0);
                vAliqProd := StringToFloatDef( INIRec.ReadString(sSecao,'vAliqProd',INIRec.ReadString(sSecao,'AliquotaValor','')) ,0);
                vCOFINS   := StringToFloatDef( INIRec.ReadString(sSecao,'vCOFINS',INIRec.ReadString(sSecao,'ValorCOFINSST'  ,'')) ,0);
              end;
            end;

            sSecao := 'ISSQN'+IntToStrZero(I,3);
            if INIRec.SectionExists(sSecao) then
            begin
              with ISSQN do
              begin
                vDeducISSQN  := StringToFloatDef( INIRec.ReadString(sSecao,'vDeducISSQN','') ,0);
                vBC          := StringToFloatDef( INIRec.ReadString(sSecao,'vBC',INIRec.ReadString(sSecao,'ValorBase'   ,'')) ,0);
                vAliq        := StringToFloatDef( INIRec.ReadString(sSecao,'vAliq',INIRec.ReadString(sSecao,'Aliquota' ,'')) ,0);
                vISSQN       := StringToFloatDef( INIRec.ReadString(sSecao,'vISSQN',INIRec.ReadString(sSecao,'ValorISSQN','')) ,0);
                cMunFG       := INIRec.ReadInteger(sSecao,'cMunFG', INIRec.ReadInteger(sSecao,'MunicipioFatoGerador',0));
                cListServ    := INIRec.ReadString(sSecao,'cListServ',INIRec.ReadString(sSecao,'CodigoServico',''));
                cServTribMun := INIRec.ReadString(sSecao,'cServTribMun','');
                cNatOp       := INIRec.ReadInteger(sSecao,'cNatOp',0);
                indIncFisc   := StrToindIncentivo(OK,INIRec.ReadString(sSecao,'indIncFisc','0'));
              end;
            end;
          end;
        end;

        Inc(I);
      end;

      Total.ICMSTot.vICMS     := StringToFloatDef( INIRec.ReadString('Total','vICMS'    ,INIRec.ReadString('Total','ValorICMS'   ,'')) ,0);
      Total.ICMSTot.vProd     := StringToFloatDef( INIRec.ReadString('Total','vProd' ,INIRec.ReadString('Total','ValorProduto'  ,'')) ,0);
      Total.ICMSTot.vDesc     := StringToFloatDef( INIRec.ReadString('Total','vDesc',INIRec.ReadString('Total','ValorDesconto'  ,'')) ,0);
      Total.ICMSTot.vPIS      := StringToFloatDef( INIRec.ReadString('Total','vPIS'     ,INIRec.ReadString('Total','ValorPIS'   ,'')) ,0);
      Total.ICMSTot.vCOFINS   := StringToFloatDef( INIRec.ReadString('Total','vCOFINS'  ,INIRec.ReadString('Total','ValorCOFINS','')) ,0);
      Total.ICMSTot.vPISST    := StringToFloatDef( INIRec.ReadString('Total','vPISST'     ,INIRec.ReadString('Total','ValorPISST'   ,'')) ,0);
      Total.ICMSTot.vCOFINSST := StringToFloatDef( INIRec.ReadString('Total','vCOFINSST'  ,INIRec.ReadString('Total','ValorCOFINSST','')) ,0);
      Total.ICMSTot.vOutro    := StringToFloatDef( INIRec.ReadString('Total','vOutro',INIRec.ReadString('Total','ValorOutrasDespesas','')) ,0);

      Total.vCFe         := StringToFloatDef( INIRec.ReadString('Total','vCFe'    ,INIRec.ReadString('Total','ValorNota'    ,'')) ,0);
      Total.vCFeLei12741 := StringToFloatDef( INIRec.ReadString('Total','vCFeLei12741'     ,INIRec.ReadString('Total','vTotTrib','')),0);

      Total.ISSQNTot.vBC       := StringToFloatDef( INIRec.ReadString('Total','vBC' ,INIRec.ReadString('ISSQNtot','ValorBaseISS'  ,'')) ,0);
      Total.ISSQNTot.vISS      := StringToFloatDef( INIRec.ReadString('Total','vISS'   ,INIRec.ReadString('ISSQNtot','ValorISSQN' ,'')) ,0);
      Total.ISSQNTot.vPIS      := StringToFloatDef( INIRec.ReadString('Total','vPIS'  ,INIRec.ReadString('ISSQNtot','ValorPISISS' ,'')) ,0);
      Total.ISSQNTot.vCOFINS   := StringToFloatDef( INIRec.ReadString('Total','vCOFINS',INIRec.ReadString('ISSQNtot','ValorCONFINSISS','')) ,0);
      Total.ISSQNTot.vPISST    := StringToFloatDef( INIRec.ReadString('Total','vPISST'  ,INIRec.ReadString('ISSQNtot','ValorPISISSST' ,'')) ,0);
      Total.ISSQNTot.vCOFINSST := StringToFloatDef( INIRec.ReadString('Total','vCOFINSST',INIRec.ReadString('ISSQNtot','ValorCONFINSISSST','')) ,0);

      Total.DescAcrEntr.vAcresSubtot := StringToFloatDef( INIRec.ReadString('Total','vAcresSubtot',INIRec.ReadString('DescAcrEntr','vAcresSubtot','')) ,0);
      Total.DescAcrEntr.vDescSubtot  := StringToFloatDef( INIRec.ReadString('Total','vDescSubtot',INIRec.ReadString('DescAcrEntr','vDescSubtot','')) ,0);

      Pagto.vTroco :=  StringToFloatDef( INIRec.ReadString('Total','vTroco','') ,0);

      I := 1;
      while true do
      begin
        sSecao := 'pag'+IntToStrZero(I,3);
        sFim   := INIRec.ReadString(sSecao,'cMP','FIM');
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
        begin
          sSecao := 'Pagto'+IntToStrZero(I,3);
          sFim   := INIRec.ReadString(sSecao,'cMP','FIM');
          if (sFim = 'FIM') or (Length(sFim) <= 0) then break;
        end;

        with Pagto.New do
        begin
          cMP   := StrToCodigoMP(OK,INIRec.ReadString(sSecao,'cMP',INIRec.ReadString(sSecao,'tpag','01')));
          vMP   := StringToFloatDef( INIRec.ReadString(sSecao,'vMP',INIRec.ReadString(sSecao,'vPag','')) ,0);
          cAdmC := INIRec.ReadInteger(sSecao,'cAdmC',0);
          if infCFe.versaoDadosEnt >= 0.09  then 
          	cAut  := INIRec.ReadString(sSecao,'cAut','');
        end;
        Inc(I);
      end;

      InfAdic.infCpl :=  INIRec.ReadString( 'DadosAdicionais','infCpl',INIRec.ReadString( 'DadosAdicionais','Complemento'    ,''));

      I := 1;
      while true do
      begin
        sSecao := 'ObsFisco'+IntToStrZero(I,3);
        sFim   := INIRec.ReadString(sSecao,'xCampo',INIRec.ReadString(sSecao,'Campo','FIM'));
        if (sFim = 'FIM') or (Length(sFim) <= 0) then break;

        if infCFe.versaoDadosEnt <= 0.07  then
        begin
          with InfAdic.obsFisco.New do
          begin
            xCampo := sFim;
            xTexto := INIRec.ReadString( sSecao,'xTexto',INIRec.ReadString( sSecao,'Texto',''));
          end;
        end
        else
        begin
          with obsFisco.New do
          begin
            xCampo := sFim;
            xTexto := INIRec.ReadString( sSecao,'xTexto',INIRec.ReadString( sSecao,'Texto',''));
          end;
        end;

        Inc(I);
      end;

      Result := True;
    finally
      INIRec.Free;
    end;
  except
    Clear;
    Result := False;
  end;
end;

function TCFe.SaveToFile(const AFileName: String): boolean;
begin
  WriteToTXT(AFileName, AsXMLString, False, False);
  FNomeArquivo := AFileName;
  Result := True;
end;

function TCFe.GetAsXMLString: AnsiString;
begin
  if FXMLOriginal = '' then
    Result := GerarXML( false )
  else
    Result := FXMLOriginal;
end;

function TCFe.GerarXML(ApenasTagsAplicacao: Boolean): AnsiString;
var
  LocCFeW: TCFeW;
begin
  LocCFeW := TCFeW.Create(Self);
  try
    LocCFeW.Gerador.Opcoes.RetirarAcentos   := FRetirarAcentos;
    LocCFeW.Gerador.Opcoes.RetirarEspacos   := FRetirarEspacos;
    LocCFeW.Gerador.Opcoes.IdentarXML       := FIdentarXML;
    LocCFeW.Gerador.Opcoes.TamanhoIdentacao := FTamanhoIdentacao;   
    LocCFeW.Opcoes.AjustarTagNro            := FAjustarTagNro;   

    LocCFeW.GerarXml( ApenasTagsAplicacao );
    FXMLOriginal := LocCFeW.Gerador.ArquivoFormatoXML;
  finally
    LocCFeW.Free;
  end;

  FXMLOriginal := ConverteXMLtoUTF8(FXMLOriginal);
  Result := FXMLOriginal;
end;

procedure TCFe.SetXMLString(const AValue : AnsiString);
var
  LocCFeR: TCFeR;
  XMLStr: String;
begin
  LocCFeR := TCFeR.Create(Self);
  try
    // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
    XMLStr := ConverteXMLtoNativeString(AValue);

    LocCFeR.Leitor.Arquivo := XMLStr;
    LocCFeR.LerXml;
  finally
    LocCFeR.Free
  end;

  FXMLOriginal := AValue;
end;

{ TLoteCFeCollection }

function TLoteCFeCollection.GetItem(Index: Integer): TCFe;
begin
  Result := TCFe(inherited Items[Index]);
end;

function TLoteCFeCollection.New: TCFe;
begin
  Result := TCFe.Create;
  Self.Add(Result);
end;

procedure TLoteCFeCollection.SetItem(Index: Integer; Value: TCFe);
begin
  inherited Items[Index] := Value;
end;

end.

