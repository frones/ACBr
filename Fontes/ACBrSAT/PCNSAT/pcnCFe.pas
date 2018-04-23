{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit pcnCFe;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnSignature;

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
    FSignature: TSignature;
    FXMLOriginal: AnsiString;
    FAjustarTagNro:Boolean;

    function GetAsXMLString: AnsiString;
    procedure SetDet(Value: TDetCollection);
    procedure SetPagto(Value: TMPCollection);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear ;
    procedure ClearSessao ;

    function LoadFromFile(AFileName : String): boolean;
    function SaveToFile(AFileName : String): boolean;
    function GerarXML( ApenasTagsAplicacao: Boolean = false) : AnsiString ;
    procedure SetXMLString(AValue : AnsiString) ;

    property NomeArquivo: String read FNomeArquivo write FNomeArquivo;
    property AsXMLString : AnsiString read GetAsXMLString write SetXMLString ;
    property XMLOriginal: AnsiString read FXMLOriginal;
  published
    property infCFe: TinfCFe read FinfCFe write FinfCFe;
    property ide: Tide read Fide write Fide;
    property Emit: TEmit read FEmit write FEmit;
    property Dest: TDest read FDest write FDest;
    property Entrega: TEntrega read FEntrega write FEntrega;
    property Det: TDetCollection read FDet write SetDet;
    property Total: TTotal read FTotal write FTotal;
    property Pagto: TMPCollection read fPagto write fPagto;
    property InfAdic: TInfAdic read FInfAdic write FInfAdic;
    property signature: Tsignature read Fsignature write Fsignature;

    property RetirarAcentos: boolean read FRetirarAcentos write FRetirarAcentos;
    property RetirarEspacos: boolean read FRetirarEspacos write FRetirarEspacos;
    property IdentarXML: boolean read FIdentarXML write FIdentarXML;
    property TamanhoIdentacao: integer read FTamanhoIdentacao write FTamanhoIdentacao;
    property AjustarTagNro: boolean read FAjustarTagNro write FAjustarTagNro;
  end;

  { TinfCFe }

  TinfCFe = class
  private
    Fversao : Real;
    FversaoDadosEnt : Real;
    FversaoSB : integer;
    FID: string;
  public
    constructor Create;
    procedure Clear ;
  published
    property versao: Real read Fversao write Fversao;
    property versaoDadosEnt: Real read FversaoDadosEnt write FversaoDadosEnt;
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
    function GetdEmi : TDateTime ;
    function GethEmi : TDateTime ;
    procedure SetdEmi(AValue : TDateTime) ;
    procedure SethEmi(AValue : TDateTime) ;
  public
    constructor Create;
    procedure Clear ;
    procedure ClearSessao ;
  published
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
  published
    property CNPJ: string read FCNPJ write FCNPJ;
    property xNome: string read FxNome write FxNome;
    property xFant: string read FxFant write FxFant;
    property EnderEmit: TEnderEmit read FEnderEmit write FEnderEmit;
    property IE: string read FIE write FIE ;
    property IM: string read FIM write FIM ;
    property cRegTrib: TpcnRegTrib read FcRegTrib write FcRegTrib ;
    property cRegTribISSQN: TpcnRegTribISSQN read FcRegTribISSQN write FcRegTribISSQN ;
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
  published
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
  published
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
  published
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property xMun: string read FxMun write FxMun;
    property UF: string read FUF write FUF;
  end;

  TDetCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDetCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetCollectionItem);
  public
    constructor Create(AOwner: TCFe);
    function Add: TDetCollectionItem;
    property Items[Index: Integer]: TDetCollectionItem read GetItem write SetItem; default;
  end;

  { TDetCollectionItem }

  TDetCollectionItem = class(TCollectionItem)
  private
    FnItem: integer;
    FProd: TProd;
    FImposto: TImposto;
    FinfAdProd: string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Clear;
  published
    property nItem: integer read FnItem write FnItem;
    property Prod: TProd read FProd write FProd;
    property Imposto: TImposto read FImposto write FImposto;
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
    FindRegra: TpcnindRegra ;
    FvDesc: currency;
    FvOutro: currency;
    FvItem: currency;
    FvRatDesc: currency;
    FvRatAcr: currency;
    FobsFiscoDet: TobsFiscoDetCollection;
    procedure SetobsFiscoDet(Value: TobsFiscoDetCollection);
  public
    constructor Create(AOwner: TDetcollectionItem);
    destructor Destroy; override;
    procedure Clear;
  published
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
    property obsFiscoDet: TobsFiscoDetCollection read FobsFiscoDet write SetobsFiscoDet;
  end;

  TobsFiscoDetCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TobsFiscoDetCollectionItem;
    procedure SetItem(Index: Integer; Value: TobsFiscoDetCollectionItem);
  public
    constructor Create(AOwner: TProd);
    destructor Destroy; override;
    function Add: TobsFiscoDetCollectionItem;
    property Items[Index: Integer]: TobsFiscoDetCollectionItem read GetItem write SetItem; default;
  end;

  TobsFiscoDetCollectionItem = class(TCollectionItem)
  private
    FxCampoDet: string;
    FxTextoDet: string;
  published
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
  published
    property vItem12741: currency read FvItem12741 write FvItem12741;
    property ICMS: TICMS read FICMS write FICMS;
    property PIS: TPIS read FPIS write FPIS;
    property PISST: TPISST read FPISST write FPISST;
    property COFINS: TCOFINS read FCOFINS write FCOFINS;
    property COFINSST: TCOFINSST read FCOFINSST write FCOFINSST;
    property ISSQN: TISSQN read FISSQN write FISSQN;
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
  published
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
  published
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
  published
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
  published
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
  published
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
  published
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
  published
    property ICMSTot: TICMSTot read FICMSTot write FICMSTot;
    property vCFe: Currency read FvCFe write FvCFe;    
    property ISSQNtot: TISSQNtot read FISSQNtot write FISSQNtot;
    property DescAcrEntr: TDescAcrEntr read FDescAcrEntr write FDescAcrEntr;
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
  published
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
  published
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
  published
    property vDescSubtot: Currency read FvDescSubtot write FvDescSubtot;
    property vAcresSubtot: Currency read FvAcresSubtot write FvAcresSubtot;
  end;

  TMPCollection = class(TCollection)
  private
    FvTroco: currency;
    function GetItem(Index: Integer): TMPCollectionItem;
    procedure SetItem(Index: Integer; Value: TMPCollectionItem);
  public
    constructor Create(AOwner: TCFe);
    destructor Destroy; override;
    procedure Clear;
    function Add: TMPCollectionItem;
    property Items[Index: Integer]: TMPCollectionItem read GetItem write SetItem; default;
  published
    property vTroco: currency read FvTroco write FvTroco;
  end;

  TMPCollectionItem = class(TCollectionItem)
  private
    FcMP: TpcnCodigoMP;
    FvMP: currency;
    FcAdmC: integer;
  published
    property cMP: TpcnCodigoMP read FcMP write FcMP;
    property vMP: currency read FvMP write FvMP;
    property cAdmC: integer read FcAdmC write FcAdmC;
  end;

  { TInfAdic }

  TInfAdic = class
  private
    FinfCpl: string;
    FobsFisco: TobsFiscoCollection;
    procedure SetobsFisco(Value: TobsFiscoCollection);
  public
    constructor Create(AOwner: TCFe);
    destructor Destroy; override;
    procedure Clear;
  published
    property infCpl: string read FinfCpl write FinfCpl;
    property obsFisco: TobsFiscoCollection read FobsFisco write SetobsFisco;
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
    FxCampo: string;
    FxTexto: string;
  published
    property xCampo: string read FxCampo write FxCampo;
    property xTexto: string read FxTexto write FxTexto;
  end;


implementation

Uses dateutils,
  pcnCFeR, pcnCFeW,
  ACBrUtil;

{ TDescAcrEntr }

constructor TDescAcrEntr.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure TDescAcrEntr.Clear ;
begin
  FvDescSubtot  := 0;
  FvAcresSubtot := 0;
end ;

{ TISSQNtot }

constructor TISSQNtot.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure TISSQNtot.Clear ;
begin
  FvBC      := 0;
  FvISS     := 0;
  FvPIS     := 0;
  FvCOFINS  := 0;
  FvPISST   := 0;
  FvCOFINSST:= 0;
end ;

{ TICMS }

constructor TICMS.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure TICMS.Clear ;
begin
  Forig   := oeNacional;
  FCST    := cst00;
  FCSOSN  := csosnVazio;
  FpICMS  := 0;
  FvICMS  := 0;
end ;

{ TPIS }

constructor TPIS.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure TPIS.Clear ;
begin
  FCST       := pis01;
  FvBC       := 0;
  FpPIS      := 0;
  FvPIS      := 0;
  FqBCProd   := 0;
  FvAliqProd := 0;
end ;

{ TPISST }

constructor TPISST.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure TPISST.Clear ;
begin
  FvBc       := 0;
  FpPis      := 0;
  FqBCProd   := 0;
  FvAliqProd := 0;
  FvPIS      := 0;
end ;

{ TCOFINS }

constructor TCOFINS.Create ;
begin
  inherited create;
  Clear;
end ;

procedure TCOFINS.Clear ;
begin
  FCST       := cof01;
  FvBC       := 0;
  FpCOFINS   := 0;
  FvCOFINS   := 0;
  FvAliqProd := 0;
  FqBCProd   := 0;
end ;

{ TCOFINSST }

constructor TCOFINSST.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure TCOFINSST.Clear ;
begin
  FvBC      := 0;
  FpCOFINS  := 0;
  FqBCProd  := 0;
  FvAliqProd:= 0;
  FvCOFINS  := 0;
end ;

{ TISSQN }

constructor TISSQN.Create ;
begin
  inherited create;
  Clear;
end ;

procedure TISSQN.Clear ;
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
end ;

{ TICMSTot }

constructor TICMSTot.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure TICMSTot.Clear ;
begin
  FvICMS     := 0;
  FvProd     := 0;
  FvDesc     := 0;
  FvPIS      := 0;
  FvCOFINS   := 0;
  FvPISST    := 0;
  FvCOFINSST := 0;
  FvOutro    := 0;
end ;

{ TEntrega }

constructor TEntrega.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure TEntrega.Clear ;
begin
  FxLgr   := '';
  Fnro    := '';
  fxCpl   := '';
  FxBairro:= '';
  FxMun   := '';
  FUF     := '';
end ;

{ TDest }

constructor TDest.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure TDest.Clear ;
begin
  FCNPJCPF := '' ;
  FxNome   := '' ;
end ;

{ TenderEmit }

constructor TenderEmit.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure TenderEmit.Clear ;
begin
  FxLgr   := '';
  Fnro    := '';
  fxCpl   := '';
  FxBairro:= '';
  FxMun   := '';
  FCEP    := 0 ;
end ;

{ Tide }

function Tide.GetdEmi : TDateTime ;
begin
  Result := DateOf( FdhEmi );
end;

function Tide.GethEmi : TDateTime ;
begin
  Result := TimeOf( FdhEmi );
end;

procedure Tide.SetdEmi(AValue : TDateTime) ;
begin
 FdhEmi := DateOf(AValue) + hEmi;
end;

procedure Tide.SethEmi(AValue : TDateTime) ;
begin
  FdhEmi := dEmi + TimeOf(AValue);
end;

constructor Tide.Create ;
begin
  inherited Create;
  Clear;
end ;

procedure Tide.Clear ;
begin
  FcUF              := 0;
  Fmodelo           := 0;
  FnserieSAT        := 0 ;
  FtpAmb            := taHomologacao;
  FCNPJ             := '';
  FsignAC           := '';
  FnumeroCaixa      := 0 ;
  ClearSessao;
end ;

procedure Tide.ClearSessao ;
begin
  FcNF              := 0;
  FnCFe             := 0;
  FdhEmi            := 0;
  FcDV              := 0;
  FassinaturaQRCODE := '';
end ;

{ TinfCFe }

constructor TinfCFe.Create ;
begin
  inherited ;
  Clear;
end ;

procedure TinfCFe.Clear ;
begin
  Fversao         := 0 ;
  FversaoDadosEnt := 0;
  FversaoSB       := 0;
  FID             := '';
end ;

{ TobsFiscoCollection }

function TobsFiscoCollection.Add: TobsFiscoCollectionItem;
begin
  Result := TobsFiscoCollectionItem(inherited Add);
end;

constructor TobsFiscoCollection.Create(AOwner: TinfAdic);
begin
  inherited Create(TobsFiscoCollectionItem);
end;

function TobsFiscoCollection.GetItem(
  Index: Integer): TobsFiscoCollectionItem;
begin
  Result := TobsFiscoCollectionItem(inherited GetItem(Index));
end;

procedure TobsFiscoCollection.SetItem(Index: Integer;
  Value: TobsFiscoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfAdic }

constructor TInfAdic.Create(AOwner: TCFe);
begin
  inherited Create;
  FobsFisco := TobsFiscoCollection.Create(Self);
  Clear;
end;

destructor TInfAdic.Destroy;
begin
  FobsFisco.Free;
  inherited;
end;

procedure TInfAdic.Clear ;
begin
  FinfCpl  := '';
  FobsFisco.Clear;
end ;

procedure TInfAdic.SetobsFisco(Value: TobsFiscoCollection);
begin
  FobsFisco.Assign(Value);
end;

{ TTotal }

constructor TTotal.Create(AOwner: TCFe);
begin
  inherited Create;
  FICMSTot := TICMSTot.Create;
  FISSQNtot := TISSQNtot.create;
  FDescAcrEntr := TDescAcrEntr.create;
end;

destructor TTotal.Destroy;
begin
  FICMSTot.Free;
  FISSQNtot.Free;
  FDescAcrEntr.Free;
  inherited;
end;

procedure TTotal.Clear ;
begin
  FvCFe         := 0;
  FvCFeLei12741 := 0;

  FICMSTot.Clear;
  FISSQNtot.Clear;
  FDescAcrEntr.Clear;
end ;

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

procedure TImposto.Clear ;
begin
  FvItem12741 := 0;
  FICMS.Clear;
  FPIS.Clear;
  FPISST.Clear;
  FCOFINS.Clear;
  FCOFINSST.Clear;
  FISSQN.Clear;
end ;

{ TProd }

constructor TProd.Create(AOwner: TDetcollectionItem);
begin
  inherited Create;
  FobsFiscoDet := TobsFiscoDetCollection.Create(Self);
  Clear;
end;

destructor TProd.Destroy;
begin
  FobsFiscoDet.Free;
  inherited;
end;

procedure TProd.Clear ;
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
  FindRegra := irArredondamento ;
  FvDesc    := 0;
  FvOutro   := 0;
  FvItem    := 0;
  FvRatDesc := 0;
  FvRatAcr  := 0;
  FobsFiscoDet.Clear;
  FEhCombustivel := False;
end ;

procedure TProd.SetobsFiscoDet(Value: TobsFiscoDetCollection);
begin
  FobsFiscoDet.Assign(Value);
end;

{ TobsFiscoDetCollection }

function TobsFiscoDetCollection.Add: TobsFiscoDetCollectionItem;
begin
  Result := TobsFiscoDetCollectionItem(inherited Add);
end;

constructor TobsFiscoDetCollection.Create(AOwner: TProd);
begin
  inherited Create(TobsFiscoDetCollectionItem);
end;

destructor TobsFiscoDetCollection.Destroy;
begin
  inherited Destroy;
end;

function TobsFiscoDetCollection.GetItem(
  Index: Integer): TobsFiscoDetCollectionItem;
begin
  Result := TobsFiscoDetCollectionItem(inherited GetItem(Index));
end;

procedure TobsFiscoDetCollection.SetItem(Index: Integer;
  Value: TobsFiscoDetCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDetCollection }

function TDetCollection.Add: TDetCollectionItem;
begin
  Result := TDetCollectionItem(inherited Add);
  Result.create;
end;

constructor TDetCollection.Create(AOwner: TCFe);
begin
  inherited Create(TDetCollectionItem);
end;

function TDetCollection.GetItem(Index: Integer): TDetCollectionItem;
begin
  Result := TDetCollectionItem(inherited GetItem(Index));
end;

procedure TDetCollection.SetItem(Index: Integer;
  Value: TDetCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDetCollectionItem }

constructor TDetCollectionItem.Create;
begin
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

procedure TDetCollectionItem.Clear ;
begin
  FnItem     := 0;
  FinfAdProd := '';

  FProd.Clear;
  FImposto.Clear;
end ;

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

procedure TEmit.Clear ;
begin
  FCNPJ  := '';
  FxNome    := '';
  FxFant    := '';
  FIE       := '' ;
  FIM       := '' ;
  FenderEmit.Clear;
  FcRegTrib      := RTSimplesNacional;
  FcRegTribISSQN := RTISSNenhum;
  FindRatISSQN   := irSim;
end ;

{ TMPCollection }

function TMPCollection.Add: TMPCollectionItem;
begin
  Result := TMPCollectionItem(inherited Add);
end;

procedure TMPCollection.Clear;
begin
   inherited Clear;
   FvTroco := 0;
end;

constructor TMPCollection.Create(AOwner: TCFe);
begin
  inherited Create(TMPCollectionItem);
  FvTroco := 0;
end;

destructor TMPCollection.Destroy;
begin

  inherited;
end;

function TMPCollection.GetItem(Index: Integer): TMPCollectionItem;
begin
  Result := TMPCollectionItem(inherited GetItem(Index));
end;

procedure TMPCollection.SetItem(Index: Integer; Value: TMPCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCFe }

constructor TCFe.Create;
begin
  FinfCFe  := TInfCFe.Create;
  FIde     := Tide.Create;
  FEmit    := TEmit.Create;
  FDest    := TDest.Create;
  FEntrega := TEntrega.Create;
  FDet     := TDetCollection.Create(Self);
  FTotal   := TTotal.Create(self);
  fPagto   := TMPCollection.Create(self);
  FinfAdic := TinfAdic.Create(self);
  Fsignature := Tsignature.create;

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
  Fsignature.Free;
  inherited Destroy;
end;

procedure TCFe.Clear ;
begin
  FinfCFe.Clear;
  Fide.Clear;
  FEmit.Clear;
  ClearSessao;
end ;

procedure TCFe.ClearSessao ;
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
  FSignature.Clear;
end ;

function TCFe.LoadFromFile(AFileName : String) : boolean ;
var
  SL : TStringList;
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
end ;

function TCFe.SaveToFile(AFileName: String): boolean;
begin
  WriteToTXT(AFileName, AsXMLString, False, False);
  FNomeArquivo := AFileName;
  Result := True;
end ;

procedure TCFe.SetDet(Value: TDetCollection);
begin
  FDet.Assign(Value);
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
  LocCFeW : TCFeW ;
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
  end ;

  FXMLOriginal := ConverteXMLtoUTF8(FXMLOriginal);
  Result := FXMLOriginal;
end;

procedure TCFe.SetXMLString(AValue : AnsiString) ;
var
  LocCFeR : TCFeR;
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

procedure TCFe.SetPagto(Value: TMPCollection);
begin
  fPagto.Assign(Value);
end;

end.
 
