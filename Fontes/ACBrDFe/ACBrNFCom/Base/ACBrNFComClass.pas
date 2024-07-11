{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrNFComClass;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  ACBrXmlBase,
//  ACBrDFeConversao,
//  ACBrDFeComum.SignatureClass,
  pcnSignature,
  ACBrNFComConversao,
  ACBrNFComProc;

type
  { TinfNFCom }

  TinfNFCom = class(TObject)
  private
    FID: string;
    FVersao: Double;

//    function GetVersaoStr: string;
//    function GetVersao: Real;
    function GetID: string;
  public
    procedure Assign(Source: TinfNFCom);

    property ID: string read GetID write FID;
    property Versao: Double read FVersao write FVersao;
//    property VersaoStr: string read GetVersaoStr;
  end;

  { TIde }

  TIde = class(TObject)
  private
    FcUF: Integer;
    FtpAmb: TACBrTipoAmbiente;
    Fmodelo: Integer;
    Fserie: Integer;
    FnNF: Integer;
    FcNF: Integer;
    FcDV: Integer;
    FdhEmi: TDateTime;
    FtpEmis: TACBrTipoEmissao;
    FnSiteAutoriz: TSiteAutorizador;
    FcMunFG: Integer;
    FfinNFCom: TFinalidadeNFCom;
    FtpFat: TTipoFaturamento;
    FverProc: string;
    FindPrePago: TIndicador;
    FindCessaoMeiosRede: TIndicador;
    FindNotaEntrada: TIndicador;
    FdhCont: TDateTime;
    FxJust: string;
  public
    procedure Assign(Source: TIde);

    property cUF: Integer read FcUF write FcUF;
    property tpAmb: TACBrTipoAmbiente read FtpAmb write FtpAmb;
    property modelo: Integer read Fmodelo write Fmodelo;
    property serie: Integer read Fserie write Fserie;
    property nNF: Integer read FnNF write FnNF;
    property cNF: Integer read FcNF write FcNF;
    property cDV: Integer read FcDV write FcDV;
    property dhEmi: TDateTime read FdhEmi write FdhEmi;
    property tpEmis: TACBrTipoEmissao read FtpEmis write FtpEmis;
    property nSiteAutoriz: TSiteAutorizador read FnSiteAutoriz write FnSiteAutoriz default sa0;
    property cMunFG: Integer read FcMunFG write FcMunFG;
    property finNFCom: TFinalidadeNFCom read FfinNFCom write FfinNFCom default fnNormal;
    property tpFat: TTipoFaturamento read FtpFat write FtpFat default tfNormal;
    property verProc: string read FverProc write FverProc;
    property indPrePago: TIndicador read FindPrePago write FindPrePago;
    property indCessaoMeiosRede: TIndicador read FindCessaoMeiosRede write FindCessaoMeiosRede;
    property indNotaEntrada: TIndicador read FindNotaEntrada write FindNotaEntrada;
    property dhCont: TDateTime read FdhCont write FdhCont;
    property xJust: string read FxJust write FxJust;
  end;

  { TEndereco }

  TEndereco = class(TObject)
  private
    FxLgr: string;
    Fnro: string;
    fxCpl: string;
    FxBairro: string;
    FcMun: Integer;
    FxMun: string;
    FCEP: Integer;
    FUF: string;
    Ffone: string;
    Femail: string;
  public
    procedure Assign(Source: TEndereco);

    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property CEP: Integer read FCEP write FCEP;
    property UF: string read FUF write FUF;
    property fone: string read Ffone write Ffone;
    property email: string read Femail write Femail;
  end;

  { TEmit }

  TEmit = class(TObject)
  private
    FCNPJ: string;
    FIE: string;
    FIEUFDest: string;
    FCRT: TCRT;
    FxNome: string;
    FxFant: string;
    FenderEmit: TEndereco;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TEmit);

    property CNPJ: string read FCNPJ write FCNPJ;
    property IE: string read FIE write FIE;
    property IEUFDest: string read FIEUFDest write FIEUFDest;
    property CRT: TCRT read FCRT write FCRT;
    property xNome: string read FxNome write FxNome;
    property xFant: string read FxFant write FxFant;
    property EnderEmit: TEndereco read FEnderEmit write FEnderEmit;
  end;

  { TDest }

  TDest = class(TObject)
  private
    FxNome: string;
    FCNPJCPF: string;
    FidOutros: string;
    FindIEDest: TindIEDest;
    FIE: string;
    FIM: string;
    FEnderDest: TEndereco;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TDest);

    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property idOutros: string read FidOutros write FidOutros;
    property xNome: string read FxNome write FxNome;
    property indIEDest: TindIEDest read FindIEDest write FindIEDest;
    property IE: string read FIE write FIE;
    property IM: string read FIM write FIM;
    property EnderDest: TEndereco read FEnderDest write FEnderDest;
  end;

  { TTermAdicCollectionItem }

  TTermAdicCollectionItem = class(TObject)
  private
    FNroTermAdic: string;
    FcUFAdic: Integer;
  public
    procedure Assign(Source: TTermAdicCollectionItem);

    property NroTermAdic: string read FNroTermAdic write FNroTermAdic;
    property cUFAdic: Integer read FcUFAdic write FcUFAdic;
  end;

  { TTermAdicCollection }

  TTermAdicCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TTermAdicCollectionItem;
    procedure SetItem(Index: Integer; Value: TTermAdicCollectionItem);
  public
    function New: TTermAdicCollectionItem;
    property Items[Index: Integer]: TTermAdicCollectionItem read GetItem write SetItem; default;
  end;

  { Tassinante }

  Tassinante = class(TObject)
  private
    FiCodAssinante: string;
    FtpAssinante: TtpAssinante;
    FtpServUtil: TtpServUtil;
    FnContrato: string;
    FdContratoIni: TDateTime;
    FdContratoFim: TDateTime;
    FNroTermPrinc: string;
    FcUFPrinc: Integer;
    FTermAdic: TTermAdicCollection;

    procedure SetTermAdic(const Value: TTermAdicCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: Tassinante);

    property iCodAssinante: string read FiCodAssinante write FiCodAssinante;
    property tpAssinante: TtpAssinante read FtpAssinante write FtpAssinante;
    property tpServUtil: TtpServUtil read FtpServUtil write FtpServUtil;
    property nContrato: string read FnContrato write FnContrato;
    property dContratoIni: TDateTime read FdContratoIni write FdContratoIni;
    property dContratoFim: TDateTime read FdContratoFim write FdContratoFim;
    property NroTermPrinc: string read FNroTermPrinc write FNroTermPrinc;
    property cUFPrinc: Integer read FcUFPrinc write FcUFPrinc;
    property TermAdic: TTermAdicCollection read FTermAdic write SetTermAdic;
  end;

  { TgNF }

  TgNF = class(TObject)
  private
    FCNPJ: string;
    FModelo: Integer;
    Fserie: string;
    FnNF: Integer;
    FCompetEmis: TDateTime;
    Fhash115: string;
  public
    procedure Assign(Source: TgNF);

    property CNPJ: string read FCNPJ write FCNPJ;
    property Modelo: Integer read FModelo write FModelo;
    property serie: string read Fserie write Fserie;
    property nNF: Integer read FnNF write FnNF;
    property CompetEmis: TDateTime read FCompetEmis write FCompetEmis;
    property hash115: string read Fhash115 write Fhash115;
  end;

  { TgSub }

  TgSub = class(TObject)
  private
    FchNFCom: string;
    FgNF: TgNF;
    FmotSub: TmotSub;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgSub);

    property chNFCom: string read FchNFCom write FchNFCom;
    property gNF: TgNF read FgNF write FgNF;
    property motSub: TmotSub read FmotSub write FmotSub;
  end;

  { TgCofat }

  TgCofat = class(TObject)
  private
    FchNFComLocal: string;
    FgNF: TgNF;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgCofat);

    property chNFComLocal: string read FchNFComLocal write FchNFComLocal;
    property gNF: TgNF read FgNF write FgNF;
  end;

  { TProd }

  TProd = class(TObject)
  private
    FcProd: string;
    FxProd: string;
    FcClass: string;
    FCFOP: Integer;
    FCNPJLD: string;
    FuMed: TuMed;
    FqFaturada: Double;
    FvItem: Double;
    FvDesc: Double;
    FvOutro: Double;
    FvProd: Double;
    FdExpiracao: TDateTime;
    FindDevolucao: TIndicador;
  public
    procedure Assign(Source: TProd);

    property cProd: string read FcProd write FcProd;
    property xProd: string read FxProd write FxProd;
    property cClass: string read FcClass write FcClass;
    property CFOP: Integer read FCFOP write FCFOP;
    property CNPJLD: string read FCNPJLD write FCNPJLD;
    property uMed: TuMed read FuMed write FuMed;
    property qFaturada: Double read FqFaturada write FqFaturada;
    property vItem: Double read FvItem write FvItem;
    property vDesc: Double read FvDesc write FvDesc;
    property vOutro: Double read FvOutro write FvOutro;
    property vProd: Double read FvProd write FvProd;
    property dExpiracao: TDateTime read FdExpiracao write FdExpiracao;
    property indDevolucao: TIndicador read FindDevolucao write FindDevolucao;
  end;

  { TICMS }

  TICMS = class(TObject)
  private
    FCST: TCSTIcms;
    FvBC: Double;
    FpICMS: Double;
    FvICMS: Double;
    FpFCP: Double;
    FvFCP: Double;
    FpRedBC: Double;
    FvICMSDeson: Double;
    FcBenef: string;
    FindSN: TIndicador;
  public
    procedure Assign(Source: TICMS);

    property CST: TCSTIcms read FCST write FCST default cst00;
    property vBC: Double read FvBC write FvBC;
    property pICMS: Double read FpICMS write FpICMS;
    property vICMS: Double read FvICMS write FvICMS;
    property pFCP: Double read FpFCP write FpFCP;
    property vFCP: Double read FvFCP write FvFCP;
    property pRedBC: Double read FpRedBC write FpRedBC;
    property vICMSDeson: Double read FvICMSDeson write FvICMSDeson;
    property cBenef: string read FcBenef write FcBenef;
    property indSN: TIndicador read FindSN write FindSN;
  end;

   { TICMSUFDestCollectionItem }

 TICMSUFDestCollectionItem = class(TObject)
  private
    FcUFDest: Integer;
    FvBCUFDest: Double;
    FpFCPUFDest: Double;
    FpICMSUFDest: Double;
    FpICMSInter: Double;
    FvFCPUFDest: Double;
    FvICMSUFDest: Double;
    FvICMSUFEmi: Double;
    FcBenefUFDest: string;
  public
    procedure Assign(Source: TICMSUFDestCollectionItem);

    property cUFDest: Integer read FcUFDest write FcUFDest;
    property vBCUFDest: Double read FvBCUFDest write FvBCUFDest;
    property pFCPUFDest: Double read FpFCPUFDest write FpFCPUFDest;
    property pICMSUFDest: Double read FpICMSUFDest write FpICMSUFDest;
    property pICMSInter: Double read FpICMSInter write FpICMSInter;
    property vFCPUFDest: Double read FvFCPUFDest write FvFCPUFDest;
    property vICMSUFDest: Double read FvICMSUFDest write FvICMSUFDest;
    property vICMSUFEmi: Double read FvICMSUFEmi write FvICMSUFEmi;
    property cBenefUFDest: string read FcBenefUFDest write FcBenefUFDest;
  end;

  { TICMSUFDestCollection }

  TICMSUFDestCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TICMSUFDestCollectionItem;
    procedure SetItem(Index: Integer; Value: TICMSUFDestCollectionItem);
  public
    function New: TICMSUFDestCollectionItem;
    property Items[Index: Integer]: TICMSUFDestCollectionItem read GetItem write SetItem; default;
  end;

  { TPIS }

  TPIS = class(TObject)
  private
    FCST: TCSTPis;
    FvBC: Double;
    FpPIS: Double;
    FvPIS: Double;
  public
    procedure Assign(Source: TPIS);

    property CST: TCSTPis read FCST write FCST default pis01;
    property vBC: Double read FvBC write FvBC;
    property pPIS: Double read FpPIS write FpPIS;
    property vPIS: Double read FvPIS write FvPIS;
  end;

  { TCOFINS }

  TCOFINS = class(TObject)
  private
    FCST: TCSTCofins;
    FvBC: Double;
    FpCOFINS: Double;
    FvCOFINS: Double;
  public
    procedure Assign(Source: TCOFINS);

    property CST: TCSTCofins read FCST write FCST default cof01;
    property vBC: Double read FvBC write FvBC;
    property pCOFINS: Double read FpCOFINS write FpCOFINS;
    property vCOFINS: Double read FvCOFINS write FvCOFINS;
  end;

  { TFUST }

  TFUST = class(TObject)
  private
    FvBC: Double;
    FpFUST: Double;
    FvFUST: Double;
  public
    procedure Assign(Source: TFUST);

    property vBC: Double read FvBC write FvBC;
    property pFUST: Double read FpFUST write FpFUST;
    property vFUST: Double read FvFUST write FvFUST;
  end;

  { TFUNTTEL }

  TFUNTTEL = class(TObject)
  private
    FvBC: Double;
    FpFUNTTEL: Double;
    FvFUNTTEL: Double;
  public
    procedure Assign(Source: TFUNTTEL);

    property vBC: Double read FvBC write FvBC;
    property pFUNTTEL: Double read FpFUNTTEL write FpFUNTTEL;
    property vFUNTTEL: Double read FvFUNTTEL write FvFUNTTEL;
  end;

  { TretTrib }

  TretTrib = class(TObject)
  private
    FvRetPIS: Double;
    FvRetCOFINS: Double;
    FvRetCSLL: Double;
    FvBCIRRF: Double;
    FvIRRF: Double;
  public
    procedure Assign(Source: TretTrib);

    property vRetPIS: Double read FvRetPIS write FvRetPIS;
    property vRetCOFINS: Double read FvRetCOFINS write FvRetCOFINS;
    property vRetCSLL: Double read FvRetCSLL write FvRetCSLL;
    property vBCIRRF: Double read FvBCIRRF write FvBCIRRF;
    property vIRRF: Double read FvIRRF write FvIRRF;
  end;

  { TImposto }

  TImposto = class(TObject)
  private
    FICMS: TICMS;
    FICMSUFDest: TICMSUFDestCollection;
    FindSemCST: TIndicador;
    FPIS: TPIS;
    FCOFINS: TCOFINS;
    FFUST: TFUST;
    FFUNTTEL: TFUNTTEL;
    FretTrib: TretTrib;

    procedure SetICMSUFDest(const Value: TICMSUFDestCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TImposto);

    property ICMS: TICMS read FICMS write FICMS;
    property ICMSUFDest: TICMSUFDestCollection read FICMSUFDest write SetICMSUFDest;
    property indSemCST: TIndicador read FindSemCST write FindSemCST;
    property PIS: TPIS read FPIS write FPIS;
    property COFINS: TCOFINS read FCOFINS write FCOFINS;
    property FUST: TFUST read FFUST write FFUST;
    property FUNTTEL: TFUNTTEL read FFUNTTEL write FFUNTTEL;
    property retTrib: TretTrib read FretTrib write FretTrib;
  end;

  { TgProcCollectionItem }

  TgProcCollectionItem = class(TObject)
  private
    FtpProc: TtpProc;
    FnProcesso: string;
  public
    procedure Assign(Source: TgProcCollectionItem);

    property tpProc: TtpProc read FtpProc write FtpProc;
    property nProcesso: string read FnProcesso write FnProcesso;
  end;

  { TgProcCollection }

  TgProcCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TgProcCollectionItem;
    procedure SetItem(Index: Integer; Value: TgProcCollectionItem);
  public
    function New: TgProcCollectionItem;
    property Items[Index: Integer]: TgProcCollectionItem read GetItem write SetItem; default;
  end;

  { TgProRef }

  TgProcRef = class(TObject)
  private
    FvItem: Double;
    FqFaturada: Integer;
    FvProd: Double;
    FvDesc: Double;
    FvOutro: Double;
    FindDevolucao: TIndicador;
    FvBC: Double;
    FpICMS: Double;
    FvICMS: Double;
    FvPIS: Double;
    FvCOFINS: Double;
    FgProc: TgProcCollection;

    procedure SetgProc(const Value: TgProcCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgProcRef);

    property vItem: Double read FvItem write FvItem;
    property qFaturada: Integer read FqFaturada write FqFaturada;
    property vProd: Double read FvProd write FvProd;
    property vDesc: Double read FvDesc write FvDesc;
    property vOutro: Double read FvOutro write FvOutro;
    property indDevolucao: TIndicador read FindDevolucao write FindDevolucao;
    property vBC: Double read FvBC write FvBC;
    property pICMS: Double read FpICMS write FpICMS;
    property vICMS: Double read FvICMS write FvICMS;
    property vPIS: Double read FvPIS write FvPIS;
    property vCOFINS: Double read FvCOFINS write FvCOFINS;
    property gProc: TgProcCollection read FgProc write SetgProc;
  end;

  { TgRessarc }

  TgRessarc = class(TObject)
  private
    FtpRessarc: TtpRessarc;
    FdRef: TDateTime;
    FnProcesso: string;
    FnProtReclama: string;
    FxObs: string;

  public
    procedure Assign(Source: TgRessarc);

    property tpRessarc: TtpRessarc read FtpRessarc write FtpRessarc;
    property dRef: TDateTime read FdRef write FdRef;
    property nProcesso: string read FnProcesso write FnProcesso;
    property nProtReclama: string read FnProtReclama write FnProtReclama;
    property xObs: string read FxObs write FxObs;
  end;

  { TDetCollectionItem }

  TDetCollectionItem = class(TObject)
  private
    FnItem: Integer;
    FchNFComAnt: string;
    FnItemAnt: Integer;
    FProd: TProd;
    FImposto: TImposto;
    FgProcRef: TgProcRef;
    FgRessarc: TgRessarc;
    FinfAdProd: string;
    FindNFComAntPapelFatCentral: TIndicador;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TDetCollectionItem);

    property nItem: Integer read FnItem write FnItem;
    property chNFComAnt: string read FchNFComAnt write FchNFComAnt;
    property nItemAnt: Integer read FnItemAnt write FnItemAnt;
    property Prod: TProd read FProd write FProd;
    property Imposto: TImposto read FImposto write FImposto;
    property gProcRef: TgProcRef read FgProcRef write FgProcRef;
    property gRessarc: TgRessarc read FgRessarc write FgRessarc;
    property infAdProd: string read FinfAdProd write FinfAdProd;
    property indNFComAntPapelFatCentral: TIndicador read FindNFComAntPapelFatCentral write FindNFComAntPapelFatCentral default tiNao;
  end;

  { TDetCollection }

  TDetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDetCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetCollectionItem);
  public
    function New: TDetCollectionItem;
    property Items[Index: Integer]: TDetCollectionItem read GetItem write SetItem; default;
  end;

  { TTotal }

  TTotal = class(TObject)
  private
    FvProd: Double;
    FvBC: Double;
    FvICMS: Double;
    FvICMSDeson: Double;
    FvFCP: Double;
    FvCOFINS: Double;
    FvPIS: Double;
    FvFUNTTEL: Double;
    FvFUST: Double;
    FvRetPIS: Double;
    FvRetCOFINS: Double;
    FvRetCSLL: Double;
    FvIRRF: Double;
    FvDesc: Double;
    FvOutro: Double;
    FvNF: Double;

  public
    procedure Assign(Source: TTotal);

    property vProd: Double read FvProd write FvProd;
    property vBC: Double read FvBC write FvBC;
    property vICMS: Double read FvICMS write FvICMS;
    property vICMSDeson: Double read FvICMSDeson write FvICMSDeson;
    property vFCP: Double read FvFCP write FvFCP;
    property vCOFINS: Double read FvCOFINS write FvCOFINS;
    property vPIS: Double read FvPIS write FvPIS;
    property vFUNTTEL: Double read FvFUNTTEL write FvFUNTTEL;
    property vFUST: Double read FvFUST write FvFUST;
    property vRetPIS: Double read FvRetPIS write FvRetPIS;
    property vRetCOFINS: Double read FvRetCOFINS write FvRetCOFINS;
    property vRetCSLL: Double read FvRetCSLL write FvRetCSLL;
    property vIRRF: Double read FvIRRF write FvIRRF;
    property vDesc: Double read FvDesc write FvDesc;
    property vOutro: Double read FvOutro write FvOutro;
    property vNF: Double read FvNF write FvNF;
  end;

  { TgFidelidade }

  TgFidelidade = class(TObject)
  private
    FqtdSaldoPts: string;
    FdRefSaldoPts: TDateTime;
    FqtdPtsResg: string;
    FdRefResgPts: TDateTime;
  public
    procedure Assign(Source: TgFidelidade);

    property qtdSaldoPts: string read FqtdSaldoPts write FqtdSaldoPts;
    property dRefSaldoPts: TDateTime read FdRefSaldoPts write FdRefSaldoPts;
    property qtdPtsResg: string read FqtdPtsResg write FqtdPtsResg;
    property dRefResgPts: TDateTime read FdRefResgPts write FdRefResgPts;
  end;

  { TgPIX }

  TgPIX = class(TObject)
  private
    FurlQRCodePIX: string;
  public
    procedure Assign(Source: TgPIX);

    property urlQRCodePIX: string read FurlQRCodePIX write FurlQRCodePIX;
  end;

  { TgFat }

  TgFat = class(TObject)
  private
    FCompetFat: TDateTime;
    FdVencFat: TDateTime;
    FdPerUsoIni: TDateTime;
    FdPerUsoFim: TDateTime;
    FcodBarras: string;
    FcodDebAuto: string;
    FcodBanco: string;
    FcodAgencia: string;
    FenderCorresp: TEndereco;
    FgPIX: TgPIX;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgFat);

    property CompetFat: TDateTime read FCompetFat write FCompetFat;
    property dVencFat: TDateTime read FdVencFat write FdVencFat;
    property dPerUsoIni: TDateTime read FdPerUsoIni write FdPerUsoIni;
    property dPerUsoFim: TDateTime read FdPerUsoFim write FdPerUsoFim;
    property codBarras: string read FcodBarras write FcodBarras;
    property codDebAuto: string read FcodDebAuto write FcodDebAuto;
    property codBanco: string read FcodBanco write FcodBanco;
    property codAgencia: string read FcodAgencia write FcodAgencia;
    property enderCorresp: TEndereco read FenderCorresp write FenderCorresp;
    property gPIX: TgPIX read FgPIX write FgPIX;
  end;

  { TgFatCentral }

  TgFatCentral = class(TObject)
  private
    FCNPJ: string;
    FcUF: Integer;
  public
    procedure Assign(Source: TgFatCentral);

    property CNPJ: string read FCNPJ write FCNPJ;
    property cUF: Integer read FcUF write FcUF;
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
    function New: TautXMLCollectionItem;
    property Items[Index: Integer]: TautXMLCollectionItem read GetItem write SetItem; default;
  end;

  { TInfAdic }

  TInfAdic = class(TObject)
  private
    FinfAdFisco: string;
    FinfCpl: string;
  public
    procedure Assign(Source: TInfAdic);

    property infAdFisco: string read FinfAdFisco write FinfAdFisco;
    // o campo abaixo precisa ser alterado pois ele pode aparecer até 5 vezes
    // no XML portanto é uma lista
    property infCpl: string read FinfCpl write FinfCpl;
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

    property CNPJ: string read FCNPJ write FCNPJ;
    property xContato: string read FxContato write FxContato;
    property email: string read Femail write Femail;
    property fone: string read Ffone write Ffone;
    property idCSRT: Integer read FidCSRT write FidCSRT;
    property hashCSRT: string read FhashCSRT write FhashCSRT;
  end;

  { TinfNFComSupl }

  TinfNFComSupl = class(TObject)
  private
    FqrCodNFCom: string;
  public
    procedure Assign(Source: TinfNFComSupl);

    property qrCodNFCom: string read FqrCodNFCom write FqrCodNFCom;
  end;

  { TNFCom }

  TNFCom = class(TObject)
  private
    FinfNFCom: TinfNFCom;
    FIde: TIde;
    FEmit: TEmit;
    FDest: TDest;
    Fassinante: Tassinante;
    FgSub: TgSub;
    FgCofat: TgCofat;
    FDet: TDetCollection;
    FTotal: TTotal;
    FgFidelidade: TgFidelidade;
    FgFat: TgFat;
    FgFatCentral: TgFatCentral;
    FautXML: TautXMLCollection;
    FinfAdic: TInfAdic;
    FinfRespTec: TinfRespTec;
    FinfNFComSupl: TinfNFComSupl;
    FSignature: TSignature;
    FprocNFCom: TProcNFCom;

    procedure SetautXML(const Value: TautXMLCollection);
    procedure SetDet(const Value: TDetCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TNFCom);

    property infNFCom: TinfNFCom read FinfNFCom write FinfNFCom;
    property Ide: TIde read FIde write FIde;
    property Emit: TEmit read FEmit write FEmit;
    property Dest: TDest read FDest write FDest;
    property assinante: Tassinante read Fassinante write Fassinante;
    property gSub: TgSub read FgSub write FgSub;
    property gCofat: TgCofat read FgCofat write FgCofat;
    property Det: TDetCollection read FDet write SetDet;
    property Total: TTotal read FTotal write FTotal;
    property gFidelidade: TgFidelidade read FgFidelidade write FgFidelidade;
    property gFat: TgFat read FgFat write FgFat;
    property gFatCentral: TgFatCentral read FgFatCentral write FgFatCentral;
    property autXML: TautXMLCollection read FautXML write SetautXML;
    property infAdic: TInfAdic read FinfAdic write FinfAdic;
    property infRespTec: TinfRespTec read FinfRespTec write FinfRespTec;
    property infNFComSupl: TinfNFComSupl read FinfNFComSupl write FinfNFComSupl;
    property Signature: TSignature read FSignature write FSignature;
    property procNFCom: TProcNFCom read FprocNFCom write FprocNFCom;
  end;

implementation

uses
  ACBrUtil.Base;

{ TICMS }

procedure TICMS.Assign(Source: TICMS);
begin
  CST := Source.CST;
  vBC := Source.vBC;
  pICMS := Source.pICMS;
  vICMS := Source.vICMS;
  pFCP := Source.pFCP;
  vFCP := Source.vFCP;
  pRedBC := Source.pRedBC;
  vICMSDeson := Source.vICMSDeson;
  cBenef := Source.cBenef;
  indSN := Source.indSN;
end;

{ TPIS }

procedure TPIS.Assign(Source: TPIS);
begin
  CST := Source.CST;
  vBC := Source.vBC;
  pPIS := Source.pPIS;
  vPIS := Source.vPIS;
end;

{ TCOFINS }

procedure TCOFINS.Assign(Source: TCOFINS);
begin
  CST := Source.CST;
  vBC := Source.vBC;
  pCOFINS := Source.pCOFINS;
  vCOFINS := Source.vCOFINS;
end;

{ TFUST }

procedure TFUST.Assign(Source: TFUST);
begin
  vBC := Source.vBC;
  pFUST := Source.pFUST;
  vFUST := Source.vFUST;
end;

{ TFUNTTEL }

procedure TFUNTTEL.Assign(Source: TFUNTTEL);
begin
  vBC := Source.vBC;
  pFUNTTEL := Source.pFUNTTEL;
  vFUNTTEL := Source.vFUNTTEL;
end;

{ TImposto }

procedure TImposto.Assign(Source: TImposto);
begin
  ICMS.Assign(Source.ICMS);
  ICMSUFDest.Assign(Source.ICMSUFDest);
  indSemCST := Source.indSemCST;
  PIS.Assign(Source.PIS);
  COFINS.Assign(Source.COFINS);
  FUST.Assign(Source.FUST);
  FUNTTEL.Assign(Source.FUNTTEL);
  retTrib.Assign(Source.retTrib);
end;

constructor TImposto.Create;
begin
  inherited Create;

  FICMS := TICMS.Create;
  FICMSUFDest := TICMSUFDestCollection.Create;
  FPIS := TPIS.Create;
  FCOFINS := TCOFINS.Create;
  FFUST := TFUST.Create;
  FFUNTTEL := TFUNTTEL.Create;
  FretTrib := TretTrib.Create;
end;

destructor TImposto.Destroy;
begin
  FICMS.Free;
  FICMSUFDest.Free;
  FPIS.Free;
  FCOFINS.Free;
  FFUST.Free;
  FFUNTTEL.Free;
  FretTrib.Free;

  inherited Destroy;
end;

procedure TImposto.SetICMSUFDest(const Value: TICMSUFDestCollection);
begin
  FICMSUFDest := Value;
end;

{ TProd }

procedure TProd.Assign(Source: TProd);
begin
  cProd := Source.cProd;
  xProd := Source.xProd;
  cClass := Source.cClass;
  CFOP := Source.CFOP;
  CNPJLD := Source.CNPJLD;
  uMed := Source.uMed;
  qFaturada := Source.qFaturada;
  vItem := Source.vItem;
  vDesc := Source.vDesc;
  vOutro := Source.vOutro;
  vProd := Source.vProd;
  dExpiracao := Source.dExpiracao;
  indDevolucao := Source.indDevolucao;
end;

{ TDetCollectionItem }

procedure TDetCollectionItem.Assign(Source: TDetCollectionItem);
begin
  nItem := Source.nItem;
  chNFComAnt := Source.chNFComAnt;
  nItemAnt := Source.nItemAnt;
  infAdProd := Source.infAdProd;
  indNFComAntPapelFatCentral := Source.indNFComAntPapelFatCentral;

  Prod.Assign(Source.Prod);
  Imposto.Assign(Source.Imposto);
  gProcRef.Assign(Source.gProcRef);
  gRessarc.Assign(Source.gRessarc);
end;

constructor TDetCollectionItem.Create;
begin
  inherited Create;

  FProd := TProd.Create;
  FImposto := TImposto.Create;
  FgProcRef := TgProcRef.Create;
  FgRessarc := TgRessarc.Create;

  indNFComAntPapelFatCentral := tiNao;
end;

destructor TDetCollectionItem.Destroy;
begin
  FProd.Free;
  FImposto.Free;
  FgProcRef.Free;
  FgRessarc.Free;

  inherited Destroy;
end;

{ TDetCollection }

function TDetCollection.GetItem(Index: Integer): TDetCollectionItem;
begin
  Result := TDetCollectionItem(inherited Items[Index]);
end;

function TDetCollection.New: TDetCollectionItem;
begin
  Result := TDetCollectionItem.Create;
  Self.Add(Result);
end;

procedure TDetCollection.SetItem(Index: Integer; Value: TDetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgCofat }

procedure TgCofat.Assign(Source: TgCofat);
begin
  chNFComLocal := Source.chNFComLocal;

  gNF.Assign(Source.gNF);
end;

constructor TgCofat.Create;
begin
  inherited Create;

  FgNF := TgNF.Create;
end;

destructor TgCofat.Destroy;
begin
  FgNF.Free;

  inherited;
end;

{ TgSub }

procedure TgSub.Assign(Source: TgSub);
begin
  chNFCom := Source.chNFCom;
  motSub := Source.motSub;

  gNF.Assign(Source.gNF);
end;

constructor TgSub.Create;
begin
  inherited Create;

  FgNF := TgNF.Create;
end;

destructor TgSub.Destroy;
begin
  FgNF.Free;

  inherited;
end;

{ Tassinante }

procedure Tassinante.Assign(Source: Tassinante);
begin
  iCodAssinante := Source.iCodAssinante;
  tpAssinante := Source.tpAssinante;
  tpServUtil := Source.tpServUtil;
  nContrato := Source.nContrato;
  dContratoIni := Source.dContratoIni;
  dContratoFim := Source.dContratoFim;
  NroTermPrinc := Source.NroTermPrinc;
  cUFPrinc := Source.cUFPrinc;

  TermAdic.Assign(Source.TermAdic);
end;

constructor Tassinante.Create;
begin
  inherited Create;

  FTermAdic := TTermAdicCollection.Create;
end;

destructor Tassinante.Destroy;
begin
  FTermAdic.Free;

  inherited;
end;

procedure Tassinante.SetTermAdic(const Value: TTermAdicCollection);
begin
  FTermAdic := Value;
end;

{ TEndereco }

procedure TEndereco.Assign(Source: TEndereco);
begin
  xLgr := Source.xLgr;
  nro := Source.nro;
  xCpl := Source.xCpl;
  xBairro := Source.xBairro;
  cMun := Source.cMun;
  xMun := Source.xMun;
  UF := Source.UF;
  CEP := Source.CEP;
  fone := Source.fone;
  email := Source.email;
end;

{ TDest }

procedure TDest.Assign(Source: TDest);
begin
  CNPJCPF := Source.CNPJCPF;
  idOutros := Source.idOutros;
  xNome := Source.xNome;
  indIEDest := Source.indIEDest;
  IE := Source.IE;
  IM := Source.IM;

  EnderDest.Assign(Source.EnderDest);
end;

constructor TDest.Create;
begin
  inherited Create;

  FEnderDest := TEndereco.Create;
end;

destructor TDest.Destroy;
begin
  FEnderDest.Free;

  inherited;
end;

{ TEmit }

procedure TEmit.Assign(Source: TEmit);
begin
  CNPJ := Source.CNPJ;
  IE := Source.IE;
  IEUFDest := Source.IEUFDest;
  CRT := Source.CRT;
  xNome := Source.xNome;
  xFant := Source.xFant;

  EnderEmit.Assign(Source.EnderEmit);
end;

constructor TEmit.Create;
begin
  inherited Create;

  FEnderEmit := TEndereco.Create;
end;

destructor TEmit.Destroy;
begin
  FEnderEmit.Free;

  inherited;
end;

{ TIde }

procedure TIde.Assign(Source: TIde);
begin
  cUF := Source.cUF;
  cNF := Source.cNF;
  modelo := Source.modelo;
  serie := Source.serie;
  nNF := Source.nNF;
  dhEmi := Source.dhEmi;
  cMunFG := Source.cMunFG;
  tpEmis := Source.tpEmis;
  cDV := Source.cDV;
  tpAmb := Source.tpAmb;
  finNFCom := Source.finNFCom;
  tpFat := Source.tpFat;
  verProc := Source.verProc;
  indPrePago := Source.indPrePago;
  indCessaoMeiosRede := Source.indCessaoMeiosRede;
  indNotaEntrada := Source.indNotaEntrada;
  dhCont := Source.dhCont;
  xJust := Source.xJust;
  nSiteAutoriz := Source.nSiteAutoriz;
end;

{ TinfNFCom }

procedure TinfNFCom.Assign(Source: TinfNFCom);
begin
  ID := Source.ID;
  Versao := Source.Versao;
end;

function TinfNFCom.GetID: string;
begin
  Result := Copy(FID, 6, 44);
end;
{
function TinfNFCom.GetVersao: Real;
begin
  if FVersao <= 0 then
     Result := 1
  else
     Result := FVersao;
end;

function TinfNFCom.GetVersaoStr: string;
begin
  if FVersao <= 0 then
    FVersao := 1;

  Result := 'versao="' + FloatToString(FVersao, '.', '#0.00') + '"';
end;
}
{ TNFCom }

procedure TNFCom.Assign(Source: TNFCom);
begin
  infNFCom.Assign(Source.infNFCom);
  Ide.Assign(Source.Ide);
  Emit.Assign(Source.Emit);
  Dest.Assign(Source.Dest);
  assinante.Assign(Source.assinante);
  gSub.Assign(Source.gSub);
  gCofat.Assign(Source.gCofat);
  Det.Assign(Source.Det);
  Total.Assign(Source.Total);
  gFidelidade.Assign(Source.gFidelidade);
  gFat.Assign(Source.gFat);
  gFatCentral.Assign(Source.gFatCentral);
  autXML.Assign(Source.autXML);
  infAdic.Assign(Source.infAdic);
  infRespTec.Assign(Source.infRespTec);
  infNFComSupl.Assign(Source.infNFComSupl);
  Signature.Assign(Source.Signature);
  procNFCom.Assign(Source.procNFCom);
end;

constructor TNFCom.Create;
begin
  inherited Create;

  FinfNFCom := TinfNFCom.Create;
  FIde := TIde.Create;
  FEmit := TEmit.Create;
  FDest := TDest.Create;
  Fassinante := Tassinante.Create;
  FgSub := TgSub.Create;
  FgCofat := TgCofat.Create;
  FDet := TDetCollection.Create;
  FTotal := TTotal.Create;
  FgFidelidade := TgFidelidade.Create;
  FgFat := TgFat.Create;
  FgFatCentral := TgFatCentral.Create;
  FautXML := TautXMLCollection.Create;
  FinfAdic := TinfAdic.Create;
  FinfRespTec := TinfRespTec.Create;
  FinfNFComSupl := TinfNFComSupl.Create;
  FSignature := TSignature.Create;
  FprocNFCom := TProcNFCom.Create;

//  FinfNFCom.Versao := 0;
  FIde.nSiteAutoriz := sa0;
end;

destructor TNFCom.Destroy;
begin
  FinfNFCom.Free;
  FIde.Free;
  FEmit.Free;
  FDest.Free;
  Fassinante.Free;
  FgSub.Free;
  FgCofat.Free;
  FDet.Free;
  FTotal.Free;
  FgFidelidade.Free;
  FgFat.Free;
  FgFatCentral.Free;
  FautXML.Free;
  FinfAdic.Free;
  FinfRespTec.Free;
  FinfNFComSupl.Free;
  FSignature.Free;
  FprocNFCom.Free;

  inherited Destroy;
end;

procedure TNFCom.SetautXML(const Value: TautXMLCollection);
begin
  FautXML := Value;
end;

procedure TNFCom.SetDet(const Value: TDetCollection);
begin
  FDet := Value;
end;

{ TretTrib }

procedure TretTrib.Assign(Source: TretTrib);
begin
  vRetPIS := Source.vRetPIS;
  vRetCOFINS := Source.vRetCOFINS;
  vRetCSLL := Source.vRetCSLL;
  vBCIRRF := Source.vBCIRRF;
  vIRRF := Source.vIRRF;
end;

{ TTermAdicItem }

procedure TTermAdicCollectionItem.Assign(Source: TTermAdicCollectionItem);
begin
  NroTermAdic := Source.NroTermAdic;
  cUFAdic := Source.cUFAdic;
end;

{ TTermAdicCollection }

function TTermAdicCollection.GetItem(Index: Integer): TTermAdicCollectionItem;
begin
  Result := TTermAdicCollectionItem(inherited Items[Index]);
end;

function TTermAdicCollection.New: TTermAdicCollectionItem;
begin
  Result := TTermAdicCollectionItem.Create;
  Self.Add(Result);
end;

procedure TTermAdicCollection.SetItem(Index: Integer; Value: TTermAdicCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TICMSUFDestCollectionItem }

procedure TICMSUFDestCollectionItem.Assign(Source: TICMSUFDestCollectionItem);
begin
  cUFDest := Source.cUFDest;
  vBCUFDest := Source.vBCUFDest;
  pFCPUFDest := Source.pFCPUFDest;
  pICMSUFDest := Source.pICMSUFDest;
  pICMSInter := Source.pICMSInter;
  vFCPUFDest := Source.vFCPUFDest;
  vICMSUFDest := Source.vICMSUFDest;
  vICMSUFEmi := Source.vICMSUFEmi;
  cBenefUFDest := Source.cBenefUFDest;
end;

{ TgNF }

procedure TgNF.Assign(Source: TgNF);
begin
  CNPJ := Source.CNPJ;
  Modelo := Source.Modelo;
  serie := Source.serie;
  nNF := Source.nNF;
  CompetEmis := Source.CompetEmis;
  hash115 := Source.hash115;
end;

{ TgProcRef }

procedure TgProcRef.Assign(Source: TgProcRef);
begin
  vItem := Source.vItem;
  qFaturada := Source.qFaturada;
  vProd := Source.vProd;
  vDesc := Source.vDesc;
  vOutro := Source.vOutro;
  indDevolucao := Source.indDevolucao;
  vBC := Source.vBC;
  pICMS := Source.pICMS;
  vICMS := Source.vICMS;
  vPIS := Source.vPIS;
  vCOFINS := Source.vCOFINS;

  gProc.Assign(Source.gProc);
end;

constructor TgProcRef.Create;
begin
  inherited Create;

  FgProc := TgProcCollection.Create;
end;

destructor TgProcRef.Destroy;
begin
  FgProc.Free;

  inherited Destroy;
end;

procedure TgProcRef.SetgProc(const Value: TgProcCollection);
begin
  FgProc := Value;
end;

{ TgProcCollectionItem }

procedure TgProcCollectionItem.Assign(Source: TgProcCollectionItem);
begin
  tpProc := Source.tpProc;
  nProcesso := Source.nProcesso;
end;

{ TgProcCollection }

function TgProcCollection.GetItem(Index: Integer): TgProcCollectionItem;
begin
  Result := TgProcCollectionItem(inherited Items[Index]);
end;

function TgProcCollection.New: TgProcCollectionItem;
begin
  Result := TgProcCollectionItem.Create;
  Self.Add(Result);
end;

procedure TgProcCollection.SetItem(Index: Integer; Value: TgProcCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgRessarc }

procedure TgRessarc.Assign(Source: TgRessarc);
begin
  tpRessarc := Source.tpRessarc;
  dRef := Source.dRef;
  nProcesso := Source.nProcesso;
  nProtReclama := Source.nProtReclama;
  xObs := Source.xObs;
end;

{ TTotal }

procedure TTotal.Assign(Source: TTotal);
begin
  vProd := Source.vProd;
  vBC := Source.vBC;
  vICMS := Source.vICMS;
  vICMSDeson := Source.vICMSDeson;
  vFCP := Source.vFCP;
  vCOFINS := Source.vCOFINS;
  vPIS := Source.vPIS;
  vFUNTTEL := Source.vFUNTTEL;
  vFUST := Source.vFUST;
  vRetPIS := Source.vRetPIS;
  vRetCOFINS := Source.vRetCOFINS;
  vRetCSLL := Source.vRetCSLL;
  vIRRF := Source.vIRRF;
  vDesc := Source.vDesc;
  vOutro := Source.vOutro;
  vNF := Source.vNF;
end;

{ TgFidelidade }

procedure TgFidelidade.Assign(Source: TgFidelidade);
begin
  qtdSaldoPts := Source.qtdSaldoPts;
  dRefSaldoPts := Source.dRefSaldoPts;
  qtdPtsResg := Source.qtdPtsResg;
  dRefResgPts := Source.dRefResgPts;
end;

{ TgFat }

procedure TgFat.Assign(Source: TgFat);
begin
  CompetFat := Source.CompetFat;
  dVencFat := Source.dVencFat;
  dPerUsoIni := Source.dPerUsoIni;
  dPerUsoFim := Source.dPerUsoFim;
  codBarras := Source.codBarras;
  codDebAuto := Source.codDebAuto;
  codBanco := Source.codBanco;
  codAgencia := Source.codAgencia;

  enderCorresp.Assign(Source.enderCorresp);
  gPIX.Assign(Source.gPIX);
end;

constructor TgFat.Create;
begin
  inherited Create;

  FenderCorresp := TEndereco.Create;
  FgPIX := TgPIX.Create;
end;

destructor TgFat.Destroy;
begin
  FenderCorresp.Free;
  FgPIX.Free;

  inherited Destroy;
end;

{ TgPIX }

procedure TgPIX.Assign(Source: TgPIX);
begin
  urlQRCodePIX := Source.urlQRCodePIX;
end;

{ TgFatCentral }

procedure TgFatCentral.Assign(Source: TgFatCentral);
begin
  CNPJ := Source.CNPJ;
  cUF := Source.cUF;
end;

{ TautXMLCollectionItem }

procedure TautXMLCollectionItem.Assign(Source: TautXMLCollectionItem);
begin
  CNPJCPF := Source.CNPJCPF;
end;

{ TautXMLCollection }

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

{ TInfAdic }

procedure TInfAdic.Assign(Source: TInfAdic);
begin
  infAdFisco := Source.infAdFisco;
  infCpl := Source.infCpl;
end;

{ TinfRespTec }

procedure TinfRespTec.Assign(Source: TinfRespTec);
begin
  CNPJ := Source.CNPJ;
  xContato := Source.xContato;
  email := Source.email;
  fone := Source.fone;
  idCSRT := Source.idCSRT;
  hashCSRT := Source.hashCSRT;
end;

{ TinfNFComSupl }

procedure TinfNFComSupl.Assign(Source: TinfNFComSupl);
begin
  qrCodNFCom := Source.qrCodNFCom;
end;

{ TICMSUFDestCollection }

function TICMSUFDestCollection.GetItem(
  Index: Integer): TICMSUFDestCollectionItem;
begin
  Result := TICMSUFDestCollectionItem(inherited Items[Index]);
end;

function TICMSUFDestCollection.New: TICMSUFDestCollectionItem;
begin
  Result := TICMSUFDestCollectionItem.Create;
  Self.Add(Result);
end;

procedure TICMSUFDestCollection.SetItem(Index: Integer;
  Value: TICMSUFDestCollectionItem);
begin
  inherited Items[Index] := Value;
end;

end.
