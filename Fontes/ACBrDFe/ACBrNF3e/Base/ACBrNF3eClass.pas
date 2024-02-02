{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrNF3eClass;

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
  ACBrNF3eConversao,
  ACBrNF3eProc,
  pcnSignature;

type
  { TinfNF3eSupl }

  TinfNF3eSupl = class(TObject)
  private
    FqrCodNF3e: String;
  public
    procedure Assign(Source: TinfNF3eSupl);

    property qrCodNF3e: String read FqrCodNF3e write FqrCodNF3e;
  end;

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

  { TInfAdic }

  TInfAdic = class(TObject)
  private
    FinfAdFisco: String;
    FinfCpl: String;
  public
    procedure Assign(Source: TInfAdic);

    property infAdFisco: String read FinfAdFisco write FinfAdFisco;
    // o campo abaixo precisa ser alterado pois ele pode aparecer até 5 vezes
    // no XML portanto é uma lista
    property infCpl: String read FinfCpl write FinfCpl;
  end;

  { TautXMLCollectionItem }

  TautXMLCollectionItem = class(TObject)
  private
    FCNPJCPF: String;
  public
    procedure Assign(Source: TautXMLCollectionItem);

    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
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

  { TTotal }

  TTotal = class(TObject)
  private
    FvProd: Double;
    FvBC: Double;
    FvICMS: Double;
    FvICMSDeson: Double;
    FvFCP: Double;
    FvBCST: Double;
    FvST: Double;
    FvFCPST: Double;
    FvCOFINS: Double;
    FvPIS: Double;
    FvNF: Double;
    FvRetCSLL: Double;
    FvRetPIS: Double;
    FvRetCOFINS: Double;
    FvPISEfet: Double;
    FvCOFINSEfet: Double;
    FvIRRF: Double;
  public
    procedure Assign(Source: TTotal);

    property vProd: Double       read FvProd       write FvProd;
    property vBC: Double         read FvBC         write FvBC;
    property vICMS: Double       read FvICMS       write FvICMS;
    property vICMSDeson: Double  read FvICMSDeson  write FvICMSDeson;
    property vFCP: Double        read FvFCP        write FvFCP;
    property vBCST: Double       read FvBCST       write FvBCST;
    property vST: Double         read FvST         write FvST;
    property vFCPST: Double      read FvFCPST      write FvFCPST;
    property vCOFINS: Double     read FvCOFINS     write FvCOFINS;
    property vPIS: Double        read FvPIS        write FvPIS;
    property vNF: Double         read FvNF         write FvNF;
    property vRetPIS: Double     read FvRetPIS     write FvRetPIS;
    property vRetCOFINS: Double  read FvRetCOFINS  write FvRetCOFINS;
    property vRetCSLL: Double    read FvRetCSLL    write FvRetCSLL;
    property vIRRF: Double       read FvIRRF       write FvIRRF;
    property vCOFINSEfet: Double read FvCOFINSEfet write FvCOFINSEfet;
    property vPISEfet: Double    read FvPISEfet    write FvPISEfet;
  end;

  { TEndereco }

  TEndereco = class(TObject)
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
    Femail: String;
  public
    procedure Assign(Source: TEndereco);

    property xLgr: String    read FxLgr    write FxLgr;
    property nro: String     read Fnro     write Fnro;
    property xCpl: String    read FxCpl    write FxCpl;
    property xBairro: String read FxBairro write FxBairro;
    property cMun: Integer   read FcMun    write FcMun;
    property xMun: String    read FxMun    write FxMun;
    property UF: String      read FUF      write FUF;
    property CEP: Integer    read FCEP     write FCEP;
    property fone: String    read Ffone    write Ffone;
    property email: String   read Femail   write Femail;
  end;

  { TgPIX }

  TgPIX = class(TObject)
  private
    FurlQRCodePIX: String;
  public
    procedure Assign(Source: TgPIX);

    property urlQRCodePIX: String read FurlQRCodePIX write FurlQRCodePIX;
  end;

  { TgFat }

  TgFat = class(TObject)
  private
    FCompetFat: TDateTime;
    FdVencFat: TDateTime;
    FdApresFat: TDateTime;
    FdProxLeitura: TDateTime;
    FnFat: String;
    FcodBarras: String;
    FcodDebAuto: String;
    FcodBanco: String;
    FcodAgencia: String;
    FenderCorresp: TEndereco;
    FgPIX: TgPIX;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgFat);

    property CompetFat: TDateTime    read FCompetFat    write FCompetFat;
    property dVencFat: TDateTime     read FdVencFat     write FdVencFat;
    property dApresFat: TDateTime    read FdApresFat    write FdApresFat;
    property dProxLeitura: TDateTime read FdProxLeitura write FdProxLeitura;
    property nFat: String            read FnFat         write FnFat;
    property codBarras: String       read FcodBarras    write FcodBarras;
    property codDebAuto: String      read FcodDebAuto   write FcodDebAuto;
    property codBanco: String        read FcodBanco     write FcodBanco;
    property codAgencia: String      read FcodAgencia   write FcodAgencia;
    property enderCorresp: TEndereco read FenderCorresp write FenderCorresp;
    property gPIX: TgPIX             read FgPIX         write FgPIX;
  end;

  { TgGrandFatCollectionItem }

  TgGrandFatCollectionItem = class(TObject)
  private
    FCompetFat: TDateTime;
    FvFat: Double;
    FuMed: TuMedFat;
    FqtdDias: Integer;
  public
    procedure Assign(Source: TgGrandFatCollectionItem);

    property CompetFat: TDateTime read FCompetFat write FCompetFat;
    property vFat: Double         read FvFat      write FvFat;
    property uMed: TuMedFat       read FuMed      write FuMed;
    property qtdDias: Integer     read FqtdDias   write FqtdDias;
  end;

  { TgGrandFatCollection }

  TgGrandFatCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TgGrandFatCollectionItem;
    procedure SetItem(Index: Integer; Value: TgGrandFatCollectionItem);
  public
    function New: TgGrandFatCollectionItem;
    property Items[Index: Integer]: TgGrandFatCollectionItem read GetItem write SetItem; default;
  end;

  { TgHistFatCollectionItem }

  TgHistFatCollectionItem = class(TObject)
  private
    FxGrandFat: String;
    FgGrandFat: TgGrandFatCollection;
    procedure SetgGrandFat(const Value: TgGrandFatCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgHistFatCollectionItem);

    property xGrandFat: String read FxGrandFat write FxGrandFat;
    property gGrandFat: TgGrandFatCollection read FgGrandFat write SetgGrandFat;
  end;

  { TgHistFatCollection }

  TgHistFatCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TgHistFatCollectionItem;
    procedure SetItem(Index: Integer; Value: TgHistFatCollectionItem);
  public
    function New: TgHistFatCollectionItem;
    property Items[Index: Integer]: TgHistFatCollectionItem read GetItem write SetItem; default;
  end;

  { TgANEEL }

  TgANEEL = class(TObject)
  private
    FgHistFat: TgHistFatCollection;
    procedure SetgHistFat(const Value: TgHistFatCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgANEEL);

    property gHistFat: TgHistFatCollection read FgHistFat write SetgHistFat;
  end;

  { TgContabCollectionItem }

  TgContabCollectionItem = class(TObject)
  private
    FcContab: String;
    FxContab: String;
    FvContab: Double;
    FtpLanc: TtpLanc;
  public
    procedure Assign(Source: TgContabCollectionItem);

    property cContab: String read FcContab write FcContab;
    property xContab: String read FxContab write FxContab;
    property vContab: Double read FvContab write FvContab;
    property tpLanc: TtpLanc read FtpLanc  write FtpLanc;
  end;

  { TgContabCollection }

  TgContabCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TgContabCollectionItem;
    procedure SetItem(Index: Integer; Value: TgContabCollectionItem);
  public
    function New: TgContabCollectionItem;
    property Items[Index: Integer]: TgContabCollectionItem read GetItem write SetItem; default;
  end;

  { TgProcCollectionItem }

  TgProcCollectionItem = class(TObject)
  private
    FtpProc: TtpProc;
    FnProcesso: String;
  public
    procedure Assign(Source: TgProcCollectionItem);

    property tpProc: TtpProc   read FtpProc    write FtpProc;
    property nProcesso: String read FnProcesso write FnProcesso;
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
    FindDevolucao: TIndicador;
    FvBC: Double;
    FpICMS: Double;
    FvICMS: Double;
    FvPIS: Double;
    FvCOFINS: Double;
    FgProc: TgProcCollection;
    FpFCP: Double;
    FvFCP: Double;
    FvBCST: Double;
    FpICMSST: Double;
    FvICMSST: Double;
    FpFCPST: Double;
    FvFCPST: Double;
    FvPISEfet: Double;
    FvCOFINSEfet: Double;

    procedure SetgProc(const Value: TgProcCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TgProcRef);

    property vItem: Double            read FvItem        write FvItem;
    property qFaturada: Integer       read FqFaturada    write FqFaturada;
    property vProd: Double            read FvProd        write FvProd;
    property indDevolucao: TIndicador read FindDevolucao write FindDevolucao;
    property vBC: Double              read FvBC          write FvBC;
    property pICMS: Double            read FpICMS        write FpICMS;
    property vICMS: Double            read FvICMS        write FvICMS;
    property vPIS: Double             read FvPIS         write FvPIS;
    property vCOFINS: Double          read FvCOFINS      write FvCOFINS;
    property gProc: TgProcCollection  read FgProc        write SetgProc;
    property pFCP: Double             read FpFCP         write FpFCP;
    property vFCP: Double             read FvFCP         write FvFCP;
    property vBCST: Double            read FvBCST        write FvBCST;
    property pICMSST: Double          read FpICMSST      write FpICMSST;
    property vICMSST: Double          read FvICMSST      write FvICMSST;
    property pFCPST: Double           read FpFCPST       write FpFCPST;
    property vFCPST: Double           read FvFCPST       write FvFCPST;
    property vPISEfet: Double         read FvPISEfet     write FvPISEfet;
    property vCOFINSEfet: Double      read FvCOFINSEfet  write FvCOFINSEfet;
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

    property CST: TCSTCofins    read FCST     write FCST default cof01;
    property vBC: Double        read FvBC     write FvBC;
    property pCOFINS: Double    read FpCOFINS write FpCOFINS;
    property vCOFINS: Double    read FvCOFINS write FvCOFINS;
  end;

  { TCOFINSEfet }

  TCOFINSEfet = class(TObject)
  private
    FvBCCOFINSEfet: Double;
    FpCOFINSEfet: Double;
    FvCOFINSEfet: Double;
  public
    procedure Assign(Source: TCOFINSEfet);

    property vBCCOFINSEfet: Double read FvBCCOFINSEfet write FvBCCOFINSEfet;
    property pCOFINSEfet: Double   read FpCOFINSEfet   write FpCOFINSEfet;
    property vCOFINSEfet: Double   read FvCOFINSEfet   write FvCOFINSEfet;
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

    property vRetPIS: Double    read FvRetPIS    write FvRetPIS;
    property vRetCOFINS: Double read FvRetCOFINS write FvRetCOFINS;
    property vRetCSLL: Double   read FvRetCSLL   write FvRetCSLL;
    property vBCIRRF: Double    read FvBCIRRF    write FvBCIRRF;
    property vIRRF: Double      read FvIRRF      write FvIRRF;
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

    property CST: TCSTPis read FCST  write FCST default pis01;
    property vBC: Double  read FvBC  write FvBC;
    property pPIS: Double read FpPIS write FpPIS;
    property vPIS: Double read FvPIS write FvPIS;
  end;

  { TPISEfet }

  TPISEfet = class(TObject)
  private
    FvBCPISEfet: Double;
    FpPISEfet: Double;
    FvPISEfet: Double;
  public
    procedure Assign(Source: TPISEfet);

    property vBCPISEfet: Double read FvBCPISEfet write FvBCPISEfet;
    property pPISEfet: Double   read FpPISEfet   write FpPISEfet;
    property vPISEfet: Double   read FvPISEfet   write FvPISEfet;
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
    FvBCST: Double;
    FpICMSST: Double;
    FvICMSST: Double;
    FpFCPST: Double;
    FvFCPST: Double;
    FpRedBC: Double;
    FvICMSDeson: Double;
    FcBenef: String;
    FindSemCST: TIndicador;
  public
    procedure Assign(Source: TICMS);

    property CST: TCSTIcms      read FCST        write FCST default cst00;
    property vBC: Double        read FvBC        write FvBC;
    property pICMS: Double      read FpICMS      write FpICMS;
    property vICMS: Double      read FvICMS      write FvICMS;
    property pFCP: Double       read FpFCP       write FpFCP;
    property vFCP: Double       read FvFCP       write FvFCP;
    property vBCST: Double      read FvBCST      write FvBCST;
    property pICMSST: Double    read FpICMSST    write FpICMSST;
    property vICMSST: Double    read FvICMSST    write FvICMSST;
    property pFCPST: Double     read FpFCPST     write FpFCPST;
    property vFCPST: Double     read FvFCPST     write FvFCPST;
    property pRedBC: Double     read FpRedBC     write FpRedBC;
    property vICMSDeson: Double read FvICMSDeson write FvICMSDeson;
    property cBenef: String     read FcBenef     write FcBenef;
    property indSemCST: TIndicador read FindSemCST write FindSemCST;
  end;

  { TImposto }

  TImposto = class(TObject)
  private
    FICMS: TICMS;
    FPIS: TPIS;
    FPISEfet: TPISEfet;
    FCOFINS: TCOFINS;
    FCOFINSEfet: TCOFINSEfet;
    FretTrib: TretTrib;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TImposto);

    property ICMS: TICMS             read FICMS       write FICMS;
    property PIS: TPIS               read FPIS        write FPIS;
    property PISEfet: TPISEfet       read FPISEfet    write FPISEfet;
    property COFINS: TCOFINS         read FCOFINS     write FCOFINS;
    property COFINSEfet: TCOFINSEfet read FCOFINSEfet write FCOFINSEfet;
    property retTrib: TretTrib       read FretTrib    write FretTrib;
  end;

  { TgMedicao }

  TgMedicao = class(TObject)
  private
    FnMed: Integer;
    FnContrat: Integer;
    FtpGrMed: TtpGrMed;
    FcPosTarif: TcPosTarif;
    FuMed: TuMedFat;
    FvMedAnt: Double;
    FvMedAtu: Double;
    FvConst: Double;
    FvMed: Double;
    FpPerdaTran: Double;
    FvMedPerdaTran: Double;
    FvMedPerdaTec: Double;
    FtpMotNaoLeitura: TtpMotNaoLeitura;
  public
    procedure Assign(Source: TgMedicao);

    property nMed: Integer                     read FnMed            write FnMed;
    property nContrat: Integer                 read FnContrat        write FnContrat;
    property tpGrMed: TtpGrMed                 read FtpGrMed         write FtpGrMed;
    property cPosTarif: TcPosTarif             read FcPosTarif       write FcPosTarif;
    property uMed: TuMedFat                    read FuMed            write FuMed;
    property vMedAnt: Double                   read FvMedAnt         write FvMedAnt;
    property vMedAtu: Double                   read FvMedAtu         write FvMedAtu;
    property vConst: Double                    read FvConst          write FvConst;
    property vMed: Double                      read FvMed            write FvMed;
    property pPerdaTran: Double                read FpPerdaTran      write FpPerdaTran;
    property vMedPerdaTran: Double             read FvMedPerdaTran   write FvMedPerdaTran;
    property vMedPerdaTec: Double              read FvMedPerdaTec    write FvMedPerdaTec;
    property tpMotNaoLeitura: TtpMotNaoLeitura read FtpMotNaoLeitura write FtpMotNaoLeitura;
  end;

  { TProd }

  TProd = class(TObject)
  private
    FindOrigemQtd: TindOrigemQtd;
    FgMedicao: TgMedicao;
    FcProd: string;
    FxProd: string;
    FcClass: Integer;
    FCFOP: Integer;
    FuMed: TuMedFat;
    FqFaturada: Integer;
    FvItem: Double;
    FvProd: Double;
    FindDevolucao: TIndicador;
    FindPrecoACL: TIndicador;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TProd);

    property indOrigemQtd: TindOrigemQtd read FindOrigemQtd write FindOrigemQtd;
    property gMedicao: TgMedicao         read FgMedicao     write FgMedicao;
    property cProd: string               read FcProd        write FcProd;
    property xProd: string               read FxProd        write FxProd;
    property cClass: Integer             read FcClass       write FcClass;
    property CFOP: Integer               read FCFOP         write FCFOP;
    property uMed: TuMedFat              read FuMed         write FuMed;
    property qFaturada: Integer          read FqFaturada    write FqFaturada;
    property vItem: Double               read FvItem        write FvItem;
    property vProd: Double               read FvProd        write FvProd;
    property indDevolucao: TIndicador    read FindDevolucao write FindDevolucao;
    property indPrecoACL: TIndicador     read FindPrecoACL  write FindPrecoACL;
  end;

  { TgAdBandCollectionItem }

  TgAdBandCollectionItem = class(TObject)
  private
    FdIniAdBand: TDateTime;
    FdFimAdBand: TDateTime;
    FtpBand: TtpBand;
    FvAdBand: Double;
    FvAdBandAplic: Double;
    FmotDifBand: TmotDifBand;
  public
    procedure Assign(Source: TgAdBandCollectionItem);

    property dIniAdBand: TDateTime   read FdIniAdBand   write FdIniAdBand;
    property dFimAdBand: TDateTime   read FdFimAdBand   write FdFimAdBand;
    property tpBand: TtpBand         read FtpBand       write FtpBand;
    property vAdBand: Double         read FvAdBand      write FvAdBand;
    property vAdBandAplic: Double    read FvAdBandAplic write FvAdBandAplic;
    property motDifBand: TmotDifBand read FmotDifBand   write FmotDifBand;
  end;

  { TgAdBandCollection }

  TgAdBandCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TgAdBandCollectionItem;
    procedure SetItem(Index: Integer; Value: TgAdBandCollectionItem);
  public
    function New: TgAdBandCollectionItem;
    property Items[Index: Integer]: TgAdBandCollectionItem read GetItem write SetItem; default;
  end;

  { TgTarifCollectionItem }

  TgTarifCollectionItem = class(TObject)
  private
    FdIniTarif: TDateTime;
    FdFimTarif: TDateTime;
    FtpAto: TtpAto;
    FnAto: String;
    FanoAto: Integer;
    FtpTarif: TtpTarif;
    FcPosTarif: TcPosTarif;
    FuMed: TuMed;
    FvTarifHom: Double;
    FvTarifAplic: Double;
    FmotDifTarif: TmotDifTarif;
  public
    procedure Assign(Source: TgTarifCollectionItem);

    property dIniTarif: TDateTime      read FdIniTarif     write FdIniTarif;
    property dFimTarif: TDateTime      read FdFimTarif     write FdFimTarif;
    property tpAto: TtpAto             read FtpAto         write FtpAto;
    property nAto: String              read FnAto          write FnAto;
    property anoAto: Integer           read FanoAto        write FanoAto;
    property tpTarif: TtpTarif         read FtpTarif      write FtpTarif;
    property cPosTarif: TcPosTarif     read FcPosTarif     write FcPosTarif;
    property uMed: TuMed               read FuMed          write FuMed;
    property vTarifHom: Double         read FvTarifHom     write FvTarifHom;
    property vTarifAplic: Double       read FvTarifAplic   write FvTarifAplic;
    property motDifTarif: TmotDifTarif read FmotDifTarif   write FmotDifTarif;
  end;

  { TgTarifCollection }

  TgTarifCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TgTarifCollectionItem;
    procedure SetItem(Index: Integer; Value: TgTarifCollectionItem);
  public
    function New: TgTarifCollectionItem;
    property Items[Index: Integer]: TgTarifCollectionItem read GetItem write SetItem; default;
  end;

  { TdetItem }

  TdetItem = class(TObject)
  private
    FnItemAnt: Integer;
    FgTarif: TgTarifCollection;
    FgAdBand: TgAdBandCollection;
    FProd: TProd;
    FImposto: TImposto;
    FinfAdProd: String;
    FgProcRef: TgProcRef;
    FgContab: TgContabCollection;

    procedure SetgTarif(const Value: TgTarifCollection);
    procedure SetgAdBand(const Value: TgAdBandCollection);
    procedure SetgContab(const Value: TgContabCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TdetItem);

    property nItemAnt: Integer           read FnItemAnt  write FnItemAnt;
    property gTarif: TgTarifCollection   read FgTarif    write SetgTarif;
    property gAdBand: TgAdBandCollection read FgAdBand   write SetgAdBand;
    property Prod: TProd                 read FProd      write FProd;
    property Imposto: TImposto           read FImposto   write FImposto;
    property gProcRef: TgProcRef         read FgProcRef  write FgProcRef;
    property gContab: TgContabCollection read FgContab   write SetgContab;
    property infAdProd: String           read FinfAdProd write FinfAdProd;
  end;

  { TdetItemAnt }

  TdetItemAnt = class(TObject)
  private
    FnItemAnt: Integer;
    FvItem: Double;
    FqFaturada: Double;
    FvProd: Double;
    FcClass: Integer;
    FvBC: Double;
    FpICMS: Double;
    FvICMS: Double;
    FvPIS: Double;
    FvCOFINS: Double;
    FretTrib: TretTrib;
    FvFCP: Double;
    FvBCST: Double;
    FvICMSST: Double;
    FvFCPST: Double;
    FvPISEfet: Double;
    FvCOFINSEfet: Double;
  public
    procedure Assign(Source: TdetItemAnt);

    property nItemAnt: Integer   read FnItemAnt    write FnItemAnt;
    property vItem: Double       read FvItem       write FvItem;
    property qFaturada: Double   read FqFaturada   write FqFaturada;
    property vProd: Double       read FvProd       write FvProd;
    property cClass: Integer     read FcClass      write FcClass;
    property vBC: Double         read FvBC         write FvBC;
    property pICMS: Double       read FpICMS       write FpICMS;
    property vICMS: Double       read FvICMS       write FvICMS;
    property vPIS: Double        read FvPIS        write FvPIS;
    property vCOFINS: Double     read FvCOFINS     write FvCOFINS;
    property retTrib: TretTrib   read FretTrib     write FretTrib;
    property vFCP: Double        read FvFCP        write FvFCP;
    property vBCST: Double       read FvBCST       write FvBCST;
    property vICMSST: Double     read FvICMSST     write FvICMSST;
    property vFCPST: Double      read FvFCPST      write FvFCPST;
    property vPISEfet: Double    read FvPISEfet    write FvPISEfet;
    property vCOFINSEfet: Double read FvCOFINSEfet write FvCOFINSEfet;
  end;

  { TgAjusteNF3eAnt }

  TgAjusteNF3eAnt = class(TObject)
  private
    FtpAjuste: TtpAjuste;
    FmotAjuste: TmotAjuste;
  public
    procedure Assign(Source: TgAjusteNF3eAnt);

    property tpAjuste: TtpAjuste   read FtpAjuste  write FtpAjuste;
    property motAjuste: TmotAjuste read FmotAjuste write FmotAjuste;
  end;

  { TDetCollectionItem }

  TDetCollectionItem = class(TObject)
  private
    FnItem: Integer;
    FgAjusteNF3eAnt: TgAjusteNF3eAnt;
    FdetItemAnt: TdetItemAnt;
    FdetItem: TdetItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TDetCollectionItem);

    property nItem: Integer read FnItem write FnItem;

    property gAjusteNF3eAnt: TgAjusteNF3eAnt read FgAjusteNF3eAnt write FgAjusteNF3eAnt;
    property detItemAnt: TdetItemAnt         read FdetItemAnt     write FdetItemAnt;
    property detItem: TdetItem               read FdetItem        write FdetItem;
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

  { TNFDetCollectionItem }

  TNFDetCollectionItem = class(TObject)
  private
    FchNF3eAnt: String;
    Fmod6HashAnt: String;
    FDet: TDetCollection;

    procedure SetDet(const Value: TDetCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TNFDetCollectionItem);

    property chNF3eAnt: String   read FchNF3eAnt   write FchNF3eAnt;
    property mod6HashAnt: String read Fmod6HashAnt write Fmod6HashAnt;
    property Det: TDetCollection read FDet         write SetDet;
  end;

  { TNFDetCollection }

  TNFDetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TNFDetCollectionItem;
    procedure SetItem(Index: Integer; Value: TNFDetCollectionItem);
  public
    function New: TNFDetCollectionItem;
    property Items[Index: Integer]: TNFDetCollectionItem read GetItem write SetItem; default;
  end;

  { TgSaldoCredCollectionItem }

  TgSaldoCredCollectionItem = class(TObject)
  private
    FtpPosTar: TtpPosTar;
    FvSaldAnt: Double;
    FvCredExpirado: Double;
    FvSaldAtual: Double;
    FvCredExpirar: Double;
    FCompetExpirar: TDateTime;
  public
    procedure Assign(Source: TgSaldoCredCollectionItem);

    property tpPosTar: TtpPosTar      read FtpPosTar      write FtpPosTar;
    property vSaldAnt: Double         read FvSaldAnt      write FvSaldAnt;
    property vCredExpirado: Double   read FvCredExpirado write FvCredExpirado;
    property vSaldAtual: Double      read FvSaldAtual    write FvSaldAtual;
    property vCredExpirar: Double    read FvCredExpirar  write FvCredExpirar;
    property CompetExpirar: TDateTime read FCompetExpirar write FCompetExpirar;
  end;

  { TgSaldoCredCollection }

  TgSaldoCredCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TgSaldoCredCollectionItem;
    procedure SetItem(Index: Integer; Value: TgSaldoCredCollectionItem);
  public
    function New: TgSaldoCredCollectionItem;
    property Items[Index: Integer]: TgSaldoCredCollectionItem read GetItem write SetItem; default;
  end;

  { TgConsumidorCollectionItem }

  TgConsumidorCollectionItem = class(TObject)
  private
    FidAcessGer: String;
    FvPotInst: Double;
    FtpFonteEnergia: TtpFonteEnergia;
    FenerAloc: Double;
    FtpPosTar: TtpPosTar;
  public
    procedure Assign(Source: TgConsumidorCollectionItem);

    property idAcessGer: String  read FidAcessGer write FidAcessGer;
    property vPotInst: Double    read FvPotInst   write FvPotInst;
    property tpFonteEnergia: TtpFonteEnergia read FtpFonteEnergia write FtpFonteEnergia;
    property enerAloc: Double    read FenerAloc   write FenerAloc;
    property tpPosTar: TtpPosTar read FtpPosTar   write FtpPosTar;
  end;

  { TgConsumidorCollection }

  TgConsumidorCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TgConsumidorCollectionItem;
    procedure SetItem(Index: Integer; Value: TgConsumidorCollectionItem);
  public
    function New: TgConsumidorCollectionItem;
    property Items[Index: Integer]: TgConsumidorCollectionItem read GetItem write SetItem; default;
  end;

  { TgJudic }

  TgSCEE = class(TObject)
  private
    FtpPartComp: TtpPartComp;
    FgConsumidor: TgConsumidorCollection;
    FgSaldoCred: TgSaldoCredCollection;

    procedure SetgConsumidor(const Value: TgConsumidorCollection);
    procedure SetgSaldoCred(const Value: TgSaldoCredCollection);
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TgSCEE);

    property tpPartComp: TtpPartComp             read FtpPartComp  write FtpPartComp;
    property gConsumidor: TgConsumidorCollection read FgConsumidor write SetgConsumidor;
    property gSaldoCred: TgSaldoCredCollection   read FgSaldoCred  write SetgSaldoCred;
  end;

  { TgMedCollectionItem }

  TgMedCollectionItem = class(TObject)
  private
    FnMed: Integer;
    FidMedidor: String;
    FdMedAnt: TDateTime;
    FdMedAtu: TDateTime;
  public
    procedure Assign(Source: TgMedCollectionItem);

    property nMed: Integer      read FnMed      write FnMed;
    property idMedidor: String  read FidMedidor write FidMedidor;
    property dMedAnt: TDateTime read FdMedAnt   write FdMedAnt;
    property dMedAtu: TDateTime read FdMedAtu   write FdMedAtu;
  end;

  { TgMedCollection }

  TgMedCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TgMedCollectionItem;
    procedure SetItem(Index: Integer; Value: TgMedCollectionItem);
  public
    function New: TgMedCollectionItem;
    property Items[Index: Integer]: TgMedCollectionItem read GetItem write SetItem; default;
  end;

  { TgGrContratCollectionItem }

  TgGrContratCollectionItem = class(TObject)
  private
    FnContrat: Integer;
    FtpGrContrat: TtpGrContrat;
    FtpPosTar: TtpPosTar;
    FqUnidContrat: Double;
  public
    procedure Assign(Source: TgGrContratCollectionItem);

    property nContrat: Integer         read FnContrat     write FnContrat;
    property tpGrContrat: TtpGrContrat read FtpGrContrat  write FtpGrContrat;
    property tpPosTar: TtpPosTar       read FtpPosTar     write FtpPosTar;
    property qUnidContrat: Double      read FqUnidContrat write FqUnidContrat;
  end;

  { TgGrContratCollection }

  TgGrContratCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TgGrContratCollectionItem;
    procedure SetItem(Index: Integer; Value: TgGrContratCollectionItem);
  public
    function New: TgGrContratCollectionItem;
    property Items[Index: Integer]: TgGrContratCollectionItem read GetItem write SetItem; default;
  end;

  { TgJudic }

  TgJudic = class(TObject)
  private
    FchNF3e: String;
  public
    procedure Assign(Source: TgJudic);

    property chNF3e: String read FchNF3e write FchNF3e;
  end;

  { TgSub }

  TgSub = class(TObject)
  private
    FchNF3e: String;
    FCNPJ: String;
    Fserie: String;
    FnNF: Integer;
    FCompetEmis: TDateTime;
    FCompetApur: TDateTime;
    Fhash115: String;
    FmotSub: TmotSub;
  public
    procedure Assign(Source: TgSub);

    property chNF3e: String        read FchNF3e     write FchNF3e;
    property CNPJ: String          read FCNPJ       write FCNPJ;
    property serie: String         read Fserie      write Fserie;
    property nNF: Integer          read FnNF        write FnNF;
    property CompetEmis: TDateTime read FCompetEmis write FCompetEmis;
    property CompetApur: TDateTime read FCompetApur write FCompetApur;
    property hash115: String       read Fhash115    write Fhash115;
    property motSub: TmotSub       read FmotSub     write FmotSub;
  end;

  { Tacessante }

  Tacessante = class(TObject)
  private
    FidAcesso: String;
    FidCodCliente: String;
    FtpAcesso: TtpAcesso;
    FxNomeUC: String;
    FtpClasse: TtpClasse;
    FtpSubClasse: TtpSubClasse;
    FtpFase: TtpFase;
    FtpGrpTensao: TtpGrpTensao;
    FtpModTar: TtpModTar;
    FlatGPS: String;
    FlongGPS: String;
    FcodRoteiroLeitura: String;
  public
    procedure Assign(Source: Tacessante);

    property idAcesso: String          read FidAcesso          write FidAcesso;
    property idCodCliente: String      read FidCodCliente      write FidCodCliente;
    property tpAcesso: TtpAcesso       read FtpAcesso          write FtpAcesso;
    property xNomeUC: String           read FxNomeUC           write FxNomeUC;
    property tpClasse: TtpClasse       read FtpClasse          write FtpClasse;
    property tpSubClasse: TtpSubClasse read FtpSubClasse       write FtpSubClasse;
    property tpFase: TtpFase           read FtpFase            write FtpFase;
    property tpGrpTensao: TtpGrpTensao read FtpGrpTensao       write FtpGrpTensao;
    property tpModTar: TtpModTar       read FtpModTar          write FtpModTar;
    property latGPS: String            read FlatGPS            write FlatGPS;
    property longGPS: String           read FlongGPS           write FlongGPS;
    property codRoteiroLeitura: String read FcodRoteiroLeitura write FcodRoteiroLeitura;
  end;

  { TEmit }

  TDest = class(TObject)
  private
    FxNome: String;
    FCNPJCPF: String;
    FidOutros: String;
    FindIEDest: TindIEDest;
    FIE: String;
    FIM: String;
    FcNIS: String;
    FNB: String;
    FxNomeAdicional: String;
    FEnderDest: TEndereco;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TDest);

    property CNPJCPF: String        read FCNPJCPF        write FCNPJCPF;
    property idOutros: String       read FidOutros       write FidOutros;
    property xNome: String          read FxNome          write FxNome;
    property indIEDest: TindIEDest  read FindIEDest      write FindIEDest;
    property IE: String             read FIE             write FIE;
    property cNIS: String           read FcNIS           write FcNIS;
    property NB: String             read FNB             write FNB;
    property IM: String             read FIM             write FIM;
    property xNomeAdicional: String read FxNomeAdicional write FxNomeAdicional;
    property EnderDest: TEndereco   read FEnderDest      write FEnderDest;
  end;

  { TEmit }

  TEmit = class(TObject)
  private
    FCNPJ: String;
    FIE: String;
    FxNome: String;
    FxFant: String;
    FenderEmit: TEndereco;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Assign(Source: TEmit);

    property CNPJ: String         read FCNPJ write FCNPJ;
    property IE: String           read FIE write FIE;
    property xNome: String        read FxNome write FxNome;
    property xFant: String        read FxFant write FxFant;
    property EnderEmit: TEndereco read FEnderEmit write FEnderEmit;
  end;

  { TIde }

  TIde = class(TObject)
  private
    FxJust: String;
    FfinNF3e: TpcnFinalidadeNF3e;
    FtpEmis: TACBrTipoEmissao;
    FnSiteAutoriz: TSiteAutorizador;
    FdhEmi: TDateTime;
    FcMunFG: Integer;
    Fserie: Integer;
    FtpAmb: TACBrTipoAmbiente;
    Fmodelo: Integer;
    FcDV: Integer;
    FnNF: Integer;
    FcUF: Integer;
    FcNF: Integer;
    FdhCont: TDateTime;
    FverProc: String;

  public
    procedure Assign(Source: TIde);

    property cUF: Integer                read FcUF     write FcUF;
    property tpAmb: TACBrTipoAmbiente    read FtpAmb   write FtpAmb default taHomologacao;
    property modelo: Integer             read Fmodelo  write Fmodelo;
    property serie: Integer              read Fserie   write Fserie;
    property nNF: Integer                read FnNF     write FnNF;
    property cNF: Integer                read FcNF     write FcNF;
    property cDV: Integer                read FcDV     write FcDV;
    property dhEmi: TDateTime            read FdhEmi   write FdhEmi;
    property tpEmis: TACBrTipoEmissao    read FtpEmis  write FtpEmis default teNormal;
    property nSiteAutoriz: TSiteAutorizador read FnSiteAutoriz write FnSiteAutoriz default sa0;
    property cMunFG: Integer             read FcMunFG  write FcMunFG;
    property finNF3e: TpcnFinalidadeNF3e read FfinNF3e write FfinNF3e default fnNormal;
    property verProc: String             read FverProc write FverProc;
    property dhCont: TDateTime           read FdhCont  write FdhCont;
    property xJust: String               read FxJust   write FxJust;
  end;

  { TinfNF3e }

  TinfNF3e = class(TObject)
  private
    FID: String;
    FVersao: Real;

    function GetVersaoStr: String;
    function GetVersao: Real;
    function GetID: String;
  public
    procedure Assign(Source: TinfNF3e);

    property ID: String        read GetID       write FID;
    property Versao: Real      read GetVersao write FVersao;
    property VersaoStr: String read GetVersaoStr;
  end;

  { TNF3e }

  TNF3e = class(TObject)
  private
    FinfNF3e: TinfNF3e;
    FIde: TIde;
    FEmit: TEmit;
    FDest: TDest;
    Facessante: Tacessante;
    FgSub: TgSub;
    FgJudic: TgJudic;
    FgGrContrat: TgGrContratCollection;
    FgMed: TgMedCollection;
    FgSCEE: TgSCEE;

    FTotal: TTotal;
    FgFat: TgFat;
    FgANEEL: TgANEEL;
    FautXML: TautXMLCollection;
    FinfAdic: TInfAdic;
    FinfRespTec: TinfRespTec;
    FinfNF3eSupl: TinfNF3eSupl;
    FSignature: TSignature;
    FprocNF3e: TProcNF3e;
    FNFDet: TNFDetCollection;

    procedure SetgGrContrat(const Value: TgGrContratCollection);
    procedure SetgMed(const Value: TgMedCollection);

    procedure SetautXML(const Value: TautXMLCollection);
    procedure SetNFDet(const Value: TNFDetCollection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TNF3e);

    property infNF3e: TinfNF3e                 read FinfNF3e     write FinfNF3e;
    property Ide: TIde                         read FIde         write FIde;
    property Emit: TEmit                       read FEmit        write FEmit;
    property Dest: TDest                       read FDest        write FDest;
    property acessante: Tacessante             read Facessante   write Facessante;
    property gSub: TgSub                       read FgSub        write FgSub;
    property gJudic: TgJudic                   read FgJudic      write FgJudic;
    property gGrContrat: TgGrContratCollection read FgGrContrat  write SetgGrContrat;
    property gMed: TgMedCollection             read FgMed        write SetgMed;
    property gSCEE: TgSCEE                     read FgSCEE       write FgSCEE;
    property NFDet: TNFDetCollection           read FNFDet       write SetNFDet;
    property Total: TTotal                     read FTotal       write FTotal;
    property gFat: TgFat                       read FgFat        write FgFat;
    property gANEEL: TgANEEL                   read FgANEEL      write FgANEEL;
    property autXML: TautXMLCollection         read FautXML      write SetautXML;
    property infAdic: TInfAdic                 read FinfAdic     write FinfAdic;
    property infRespTec: TinfRespTec           read FinfRespTec  write FinfRespTec;
    property infNF3eSupl: TinfNF3eSupl         read FinfNF3eSupl write FinfNF3eSupl;
    property Signature: TSignature             read FSignature   write FSignature;
    property procNF3e: TProcNF3e               read FprocNF3e    write FprocNF3e;
  end;

const
  CMUN_EXTERIOR = 9999999;
  XMUN_EXTERIOR = 'EXTERIOR';
  UF_EXTERIOR = 'EX';

implementation

uses
  ACBrUtil.Base;

{ TinfNF3eSupl }

procedure TinfNF3eSupl.Assign(Source: TinfNF3eSupl);
begin
  qrCodNF3e := Source.qrCodNF3e;
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

{ TInfAdic }

procedure TInfAdic.Assign(Source: TInfAdic);
begin
  infAdFisco := Source.infAdFisco;
  infCpl     := Source.infCpl;
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

{ TgGrandFatCollectionItem }

procedure TgGrandFatCollectionItem.Assign(Source: TgGrandFatCollectionItem);
begin
  CompetFat := Source.CompetFat;
  vFat      := Source.vFat;
  uMed      := Source.uMed;
  qtdDias   := Source.qtdDias;
end;

{ TgGrandFatCollection }

function TgGrandFatCollection.GetItem(Index: Integer): TgGrandFatCollectionItem;
begin
  Result := TgGrandFatCollectionItem(inherited Items[Index]);
end;

function TgGrandFatCollection.New: TgGrandFatCollectionItem;
begin
  Result := TgGrandFatCollectionItem.Create;
  Self.Add(Result);
end;

procedure TgGrandFatCollection.SetItem(Index: Integer;
  Value: TgGrandFatCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgHistFatCollectionItem }

procedure TgHistFatCollectionItem.Assign(Source: TgHistFatCollectionItem);
begin
  xGrandFat := Source.xGrandFat;

  gGrandFat.Assign(Source.gGrandFat);
end;

constructor TgHistFatCollectionItem.Create;
begin
  inherited Create;

  FgGrandFat := TgGrandFatCollection.Create;
end;

destructor TgHistFatCollectionItem.Destroy;
begin
  FgGrandFat.Free;

  inherited Destroy;
end;

procedure TgHistFatCollectionItem.SetgGrandFat(
  const Value: TgGrandFatCollection);
begin
  FgGrandFat := Value;
end;

{ TgHistFatCollection }

function TgHistFatCollection.GetItem(Index: Integer): TgHistFatCollectionItem;
begin
  Result := TgHistFatCollectionItem(inherited Items[Index]);
end;

function TgHistFatCollection.New: TgHistFatCollectionItem;
begin
  Result := TgHistFatCollectionItem.Create;
  Self.Add(Result);
end;

procedure TgHistFatCollection.SetItem(Index: Integer;
  Value: TgHistFatCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgANEEL }

procedure TgANEEL.Assign(Source: TgANEEL);
begin
  gHistFat.Assign(Source.gHistFat);
end;

constructor TgANEEL.Create;
begin
  inherited Create;

  FgHistFat := TgHistFatCollection.Create;
end;

destructor TgANEEL.Destroy;
begin
  FgHistFat.Free;

  inherited Destroy;
end;

procedure TgANEEL.SetgHistFat(const Value: TgHistFatCollection);
begin
  FgHistFat := Value;
end;

{ TgFat }

procedure TgFat.Assign(Source: TgFat);
begin
  CompetFat    := Source.CompetFat;
  dVencFat     := Source.dVencFat;
  dApresFat    := Source.dApresFat;
  dProxLeitura := Source.dProxLeitura;
  nFat         := Source.nFat;
  codBarras    := Source.codBarras;
  codDebAuto   := Source.codDebAuto;
  codBanco     := Source.codBanco;
  codAgencia   := Source.codAgencia;

  enderCorresp.Assign(Source.enderCorresp);
  gPIX.Assign(Source.gPIX);
end;

constructor TgFat.Create;
begin
  inherited Create;

  FenderCorresp := TEndereco.Create;
  FgPIX         := TgPIX.Create;
end;

destructor TgFat.Destroy;
begin
  FenderCorresp.Free;
  FgPIX.Free;

  inherited Destroy;
end;

{ TTotal }

procedure TTotal.Assign(Source: TTotal);
begin
  vProd       := Source.vProd;
  vBC         := Source.vBC;
  vICMS       := Source.vICMS;
  vICMSDeson  := Source.vICMSDeson;
  vFCP        := Source.vFCP;
  vBCST       := Source.vBCST;
  vST         := Source.vST;
  vFCPST      := Source.vFCPST;
  vCOFINS     := Source.vCOFINS;
  vPIS        := Source.vPIS;
  vNF         := Source.vNF;
  vRetCSLL    := Source.vRetCSLL;
  vRetPIS     := Source.vRetPIS;
  vRetCOFINS  := Source.vRetCOFINS;
  vPISEfet    := Source.vPISEfet;
  vCOFINSEfet := Source.vCOFINSEfet;
  vIRRF       := Source.vIRRF;
end;

{ TgContabCollectionItem }

procedure TgContabCollectionItem.Assign(Source: TgContabCollectionItem);
begin
  cContab := Source.cContab;
  xContab := Source.xContab;
  vContab := Source.vContab;
  tpLanc  := Source.tpLanc;
end;

{ TgContabCollection }

function TgContabCollection.GetItem(Index: Integer): TgContabCollectionItem;
begin
  Result := TgContabCollectionItem(inherited Items[Index]);
end;

function TgContabCollection.New: TgContabCollectionItem;
begin
  Result := TgContabCollectionItem.Create;
  Self.Add(Result);
end;

procedure TgContabCollection.SetItem(Index: Integer;
  Value: TgContabCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgProcCollectionItem }

procedure TgProcCollectionItem.Assign(Source: TgProcCollectionItem);
begin
  tpProc    := Source.tpProc;
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

{ TgProRef }

procedure TgProcRef.Assign(Source: TgProcRef);
begin
  vItem        := Source.vItem;
  qFaturada    := Source.qFaturada;
  vProd        := Source.vProd;
  indDevolucao := Source.indDevolucao;
  vBC          := Source.vBC;
  pICMS        := Source.pICMS;
  vICMS        := Source.vICMS;
  vPIS         := Source.vPIS;
  vCOFINS      := Source.vCOFINS;

  pFCP        := Source.pFCP;
  vFCP        := Source.vFCP;
  vBCST       := Source.vBCST;
  pICMSST     := Source.pICMSST;
  vICMSST     := Source.vICMSST;
  pFCPST      := Source.pFCPST;
  vFCPST      := Source.vFCPST;
  vPISEfet    := Source.vPISEfet;
  vCOFINSEfet := Source.vCOFINSEfet;

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

{ TCOFINS }

procedure TCOFINS.Assign(Source: TCOFINS);
begin
  CST     := Source.CST;
  vBC     := Source.vBC;
  pCOFINS := Source.pCOFINS;
  vCOFINS := Source.vCOFINS;
end;

{ TCOFINS }

procedure TCOFINSEfet.Assign(Source: TCOFINSEfet);
begin
  vBCCOFINSEfet := Source.vBCCOFINSEfet;
  pCOFINSEfet   := Source.pCOFINSEfet;
  vCOFINSEfet   := Source.vCOFINSEfet;
end;

{ TPIS }

procedure TPIS.Assign(Source: TPIS);
begin
  CST  := Source.CST;
  vBC  := Source.vBC;
  pPIS := Source.pPIS;
  vPIS := Source.vPIS;
end;

{ TPISEfet }

procedure TPISEfet.Assign(Source: TPISEfet);
begin
  vBCPISEfet := Source.vBCPISEfet;
  pPISEfet   := Source.pPISEfet;
  vPISEfet   := Source.vPISEfet;
end;

{ TICMS }

procedure TICMS.Assign(Source: TICMS);
begin
  CST        := Source.CST;
  vBC        := Source.vBC;
  pICMS      := Source.pICMS;
  vICMS      := Source.vICMS;
  pFCP       := Source.pFCP;
  vFCP       := Source.vFCP;
  vBCST      := Source.vBCST;
  pICMSST    := Source.pICMSST;
  vICMSST    := Source.vICMSST;
  pFCPST     := Source.pFCPST;
  vFCPST     := Source.vFCPST;
  pRedBC     := Source.pRedBC;
  vICMSDeson := Source.vICMSDeson;
  cBenef     := Source.cBenef;
end;

{ TImposto }

procedure TImposto.Assign(Source: TImposto);
begin
  ICMS.Assign(Source.ICMS);
  PIS.Assign(Source.PIS);
  COFINS.Assign(Source.COFINS);
end;

constructor TImposto.Create;
begin
  inherited Create;

  FICMS       := TICMS.Create;
  FPIS        := TPIS.Create;
  FPISEfet    := TPISEfet.Create;
  FCOFINS     := TCOFINS.Create;
  FCOFINSEfet := TCOFINSEfet.Create;
  FretTrib    := TretTrib.Create;
end;

destructor TImposto.Destroy;
begin
  FICMS.Free;
  FPIS.Free;
  FPISEfet.Free;
  FCOFINS.Free;
  FCOFINSEfet.Free;
  FretTrib.Free;

  inherited Destroy;
end;

{ TgMedicao }

procedure TgMedicao.Assign(Source: TgMedicao);
begin
  nMed := Source.nMed;
  nContrat := Source.nContrat;
  tpGrMed := Source.tpGrMed;
  cPosTarif := Source.cPosTarif;
  uMed := Source.uMed;
  vMedAnt := Source.vMedAnt;
  vMedAtu := Source.vMedAtu;
  vConst := Source.vConst;
  vMed := Source.vMed;
  pPerdaTran := Source.pPerdaTran;
  vMedPerdaTran := Source.vMedPerdaTran;
  tpMotNaoLeitura := Source.tpMotNaoLeitura;
end;

{ TProd }

procedure TProd.Assign(Source: TProd);
begin
  indOrigemQtd := Source.indOrigemQtd;
  cProd        := Source.cProd;
  xProd        := Source.xProd;
  cClass       := Source.cClass;
  CFOP         := Source.CFOP;
  uMed         := Source.uMed;
  qFaturada    := Source.qFaturada;
  vItem        := Source.vItem;
  vProd        := Source.vProd;
  indDevolucao := Source.indDevolucao;
  indPrecoACL  := Source.indPrecoACL;

  gMedicao.Assign(Source.gMedicao);
end;

constructor TProd.Create;
begin
  inherited Create;

  FgMedicao := TgMedicao.Create;
end;

destructor TProd.Destroy;
begin
  FgMedicao.Free;

  inherited Destroy;
end;

{ TgAdBandCollectionItem }

procedure TgAdBandCollectionItem.Assign(Source: TgAdBandCollectionItem);
begin
  dIniAdBand   := Source.dIniAdBand;
  dFimAdBand   := Source.dFimAdBand;
  tpBand       := Source.tpBand;
  vAdBand      := Source.vAdBand;
  vAdBandAplic := Source.vAdBandAplic;
  motDifBand   := Source.motDifBand;
end;

{ TgAdBandCollection }

function TgAdBandCollection.GetItem(Index: Integer): TgAdBandCollectionItem;
begin
  Result := TgAdBandCollectionItem(inherited Items[Index]);
end;

function TgAdBandCollection.New: TgAdBandCollectionItem;
begin
  Result := TgAdBandCollectionItem.Create;
  Self.Add(Result);
end;

procedure TgAdBandCollection.SetItem(Index: Integer;
  Value: TgAdBandCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgTarifCollectionItem }

procedure TgTarifCollectionItem.Assign(Source: TgTarifCollectionItem);
begin
  dIniTarif   := Source.dIniTarif;
  dFimTarif   := Source.dFimTarif;
  tpAto       := Source.tpAto;
  nAto        := Source.nAto;
  anoAto      := Source.anoAto;
  tpTarif     := Source.tpTarif;
  cPosTarif   := Source.cPosTarif;
  uMed        := Source.uMed;
  vTarifHom   := Source.vTarifHom;
  vTarifAplic := Source.vTarifAplic;
  motDifTarif := Source.motDifTarif;
end;

{ TgTarifCollection }

function TgTarifCollection.GetItem(Index: Integer): TgTarifCollectionItem;
begin
  Result := TgTarifCollectionItem(inherited Items[Index]);
end;

function TgTarifCollection.New: TgTarifCollectionItem;
begin
  Result := TgTarifCollectionItem.Create;
  Self.Add(Result);
end;

procedure TgTarifCollection.SetItem(Index: Integer;
  Value: TgTarifCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TdetItem }

procedure TdetItem.Assign(Source: TdetItem);
begin
  nItemAnt  := Source.nItemAnt;
  infAdProd := Source.infAdProd;

  gTarif.Assign(Source.gTarif);
  gAdBand.Assign(Source.gAdBand);
  Prod.Assign(Source.Prod);
  Imposto.Assign(Source.Imposto);
  gProcRef.Assign(Source.gProcRef);
  gContab.Assign(Source.gContab);
end;

constructor TdetItem.Create;
begin
  inherited Create;

  FgTarif   := TgTarifCollection.Create;
  FgAdBand  := TgAdBandCollection.Create;
  FProd     := TProd.Create;
  FImposto  := TImposto.Create;
  FgProcRef := TgProcRef.Create;
  FgContab  := TgContabCollection.Create;
end;

destructor TdetItem.Destroy;
begin
  FgTarif.Free;
  FgAdBand.Free;
  FProd.Free;
  FImposto.Free;
  FgProcRef.Free;
  FgContab.Free;

  inherited Destroy;
end;

procedure TdetItem.SetgAdBand(const Value: TgAdBandCollection);
begin
  FgAdBand := Value;
end;

procedure TdetItem.SetgContab(const Value: TgContabCollection);
begin
  FgContab := Value;
end;

procedure TdetItem.SetgTarif(const Value: TgTarifCollection);
begin
  FgTarif := Value;
end;

{ TdetItemAnt }

procedure TdetItemAnt.Assign(Source: TdetItemAnt);
begin
  nItemAnt  := Source.nItemAnt;
  vItem     := Source.vItem;
  qFaturada := Source.qFaturada;
  vProd     := Source.vProd;
  cClass    := Source.cClass;
  vBC       := Source.vBC;
  pICMS     := Source.pICMS;
  vICMS     := Source.vICMS;
  vPIS      := Source.vPIS;
  vCOFINS   := Source.vCOFINS;

  vFCP        := Source.vFCP;
  vBCST       := Source.vBCST;
  vICMSST     := Source.vICMSST;
  vFCPST      := Source.vFCPST;
  vPISEfet    := Source.vPISEfet;
  vCOFINSEfet := Source.vCOFINSEfet;

  retTrib.Assign(Source.retTrib);
end;

{ TgAjusteNF3eAnt }

procedure TgAjusteNF3eAnt.Assign(Source: TgAjusteNF3eAnt);
begin
  tpAjuste  := Source.tpAjuste;
  motAjuste := Source.motAjuste;
end;

{ TDetCollectionItem }

procedure TDetCollectionItem.Assign(Source: TDetCollectionItem);
begin
  nItem := Source.nItem;

  gAjusteNF3eAnt.Assign(Source.gAjusteNF3eAnt);
  detItemAnt.Assign(Source.detItemAnt);
  detItem.Assign(Source.detItem);
end;

constructor TDetCollectionItem.Create;
begin
  inherited Create;

  FgAjusteNF3eAnt := TgAjusteNF3eAnt.Create;
  FdetItemAnt := TdetItemAnt.Create;
  FdetItem := TdetItem.Create;
end;

destructor TDetCollectionItem.Destroy;
begin
  FgAjusteNF3eAnt.Free;
  FdetItemAnt.Free;
  FdetItem.Free;

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

{ TNFDetCollectionItem }

procedure TNFDetCollectionItem.Assign(Source: TNFDetCollectionItem);
begin
  chNF3eAnt   := Source.chNF3eAnt;
  mod6HashAnt := Source.mod6HashAnt;

  Det.Assign(Source.Det);
end;

constructor TNFDetCollectionItem.Create;
begin
  inherited Create;

  FDet := TDetCollection.Create;
end;

destructor TNFDetCollectionItem.Destroy;
begin
  FDet.Free;

  inherited Destroy;
end;

procedure TNFDetCollectionItem.SetDet(const Value: TDetCollection);
begin
  FDet := Value;
end;

{ TNFDetCollection }

function TNFDetCollection.GetItem(Index: Integer): TNFDetCollectionItem;
begin
  Result := TNFDetCollectionItem(inherited Items[Index]);
end;

function TNFDetCollection.New: TNFDetCollectionItem;
begin
  Result := TNFDetCollectionItem.Create;
  Self.Add(Result);
end;

procedure TNFDetCollection.SetItem(Index: Integer; Value: TNFDetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgSaldoCredCollectionItem }

procedure TgSaldoCredCollectionItem.Assign(Source: TgSaldoCredCollectionItem);
begin
  tpPosTar      := Source.tpPosTar;
  vSaldAnt      := Source.vSaldAnt;
  vCredExpirado := Source.vCredExpirado;
  vSaldAtual    := Source.vSaldAtual;
  vCredExpirar  := Source.vCredExpirar;
  CompetExpirar := Source.CompetExpirar;
end;

{ TgSaldoCredCollection }

function TgSaldoCredCollection.GetItem(
  Index: Integer): TgSaldoCredCollectionItem;
begin
  Result := TgSaldoCredCollectionItem(inherited Items[Index]);
end;

function TgSaldoCredCollection.New: TgSaldoCredCollectionItem;
begin
  Result := TgSaldoCredCollectionItem.Create;
  Self.Add(Result);
end;

procedure TgSaldoCredCollection.SetItem(Index: Integer;
  Value: TgSaldoCredCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgConsumidorCollectionItem }

procedure TgConsumidorCollectionItem.Assign(Source: TgConsumidorCollectionItem);
begin
  idAcessGer := Source.idAcessGer;
  vPotInst   := Source.vPotInst;
  tpFonteEnergia := Source.tpFonteEnergia;
  enerAloc   := Source.enerAloc;
  tpPosTar   := Source.tpPosTar;
end;

{ TgConsumidorCollection }

function TgConsumidorCollection.GetItem(
  Index: Integer): TgConsumidorCollectionItem;
begin
  Result := TgConsumidorCollectionItem(inherited Items[Index]);
end;

function TgConsumidorCollection.New: TgConsumidorCollectionItem;
begin
  Result := TgConsumidorCollectionItem.Create;
  Self.Add(Result);
end;

procedure TgConsumidorCollection.SetItem(Index: Integer;
  Value: TgConsumidorCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgSCEE }

procedure TgSCEE.Assign(Source: TgSCEE);
begin
  tpPartComp := Source.tpPartComp;

  gConsumidor.Assign(Source.gConsumidor);
  gSaldoCred.Assign(Source.gSaldoCred);
end;

constructor TgSCEE.Create;
begin
  inherited Create;

  FgConsumidor := TgConsumidorCollection.Create;
  FgSaldoCred := TgSaldoCredCollection.Create;
end;

destructor TgSCEE.Destroy;
begin
  FgConsumidor.Free;
  FgSaldoCred.Free;

  inherited Destroy;
end;

procedure TgSCEE.SetgConsumidor(const Value: TgConsumidorCollection);
begin
  FgConsumidor := Value;
end;

procedure TgSCEE.SetgSaldoCred(const Value: TgSaldoCredCollection);
begin
  FgSaldoCred := Value;
end;

{ TgMedCollectionItem }

procedure TgMedCollectionItem.Assign(Source: TgMedCollectionItem);
begin
  nMed      := Source.nMed;
  idMedidor := Source.idMedidor;
  dMedAnt   := Source.dMedAnt;
  dMedAtu   := Source.dMedAtu;
end;

{ TgMedCollection }

function TgMedCollection.GetItem(Index: Integer): TgMedCollectionItem;
begin
  Result := TgMedCollectionItem(inherited Items[Index]);
end;

function TgMedCollection.New: TgMedCollectionItem;
begin
  Result := TgMedCollectionItem.Create;
  Self.Add(Result);
end;

procedure TgMedCollection.SetItem(Index: Integer; Value: TgMedCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgGrContratCollectionItem }

procedure TgGrContratCollectionItem.Assign(Source: TgGrContratCollectionItem);
begin
  nContrat     := Source.nContrat;
  tpGrContrat  := Source.tpGrContrat;
  tpPosTar     := Source.tpPosTar;
  qUnidContrat := Source.qUnidContrat;
end;

{ TgGrContratCollection }

function TgGrContratCollection.GetItem(
  Index: Integer): TgGrContratCollectionItem;
begin
  Result := TgGrContratCollectionItem(inherited Items[Index]);
end;

function TgGrContratCollection.New: TgGrContratCollectionItem;
begin
  Result := TgGrContratCollectionItem.Create;
  Self.Add(Result);
end;

procedure TgGrContratCollection.SetItem(Index: Integer;
  Value: TgGrContratCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TgJudic }

procedure TgJudic.Assign(Source: TgJudic);
begin
  chNF3e := Source.chNF3e;
end;

{ TgSub }

procedure TgSub.Assign(Source: TgSub);
begin
  chNF3e     := Source.chNF3e;
  CNPJ       := Source.CNPJ;
  serie      := Source.serie;
  nNF        := Source.nNF;
  CompetEmis := Source.CompetEmis;
  CompetApur := Source.CompetApur;
  hash115    := Source.hash115;
  motSub     := Source.motSub;
end;

{ Tacessante }

procedure Tacessante.Assign(Source: Tacessante);
begin
  idAcesso     := Source.idAcesso;
  idCodCliente := Source.idCodCliente;
  tpAcesso     := Source.tpAcesso;
  xNomeUC      := Source.xNomeUC;
  tpClasse     := Source.tpClasse;
  tpSubClasse  := Source.tpSubClasse;
  tpFase       := Source.tpFase;
  tpGrpTensao  := Source.tpGrpTensao;
  tpModTar     := Source.tpModTar;
  latGPS       := Source.latGPS;
  longGPS      := Source.longGPS;
end;

{ TEndereco }

procedure TEndereco.Assign(Source: TEndereco);
begin
  xLgr    := Source.xLgr;
  nro     := Source.nro;
  xCpl    := Source.xCpl;
  xBairro := Source.xBairro;
  cMun    := Source.cMun;
  xMun    := Source.xMun;
  UF      := Source.UF;
  CEP     := Source.CEP;
  fone    := Source.fone;
  email   := Source.email;
end;

{ TDest }

procedure TDest.Assign(Source: TDest);
begin
  CNPJCPF        := Source.CNPJCPF;
  idOutros       := Source.idOutros;
  xNome          := Source.xNome;
  indIEDest      := Source.indIEDest;
  IE             := Source.IE;
  IM             := Source.IM;
  cNIS           := Source.cNIS;
  NB             := Source.NB;
  xNomeAdicional := Source.xNomeAdicional;

  EnderDest.Assign(Source.EnderDest);
end;

constructor TDest.Create();
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
  CNPJ  := Source.CNPJ;
  IE    := Source.IE;
  xNome := Source.xNome;
  xFant := Source.xFant;

  EnderEmit.Assign(Source.EnderEmit);
end;

constructor TEmit.Create();
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
  cUF     := Source.cUF;
  cNF     := Source.cNF;
  modelo  := Source.modelo;
  serie   := Source.serie;
  nNF     := Source.nNF;
  dhEmi   := Source.dhEmi;
  cMunFG  := Source.cMunFG;
  tpEmis  := Source.tpEmis;
  cDV     := Source.cDV;
  tpAmb   := Source.tpAmb;
  finNF3e := Source.finNF3e;
  verProc := Source.verProc;
  dhCont  := Source.dhCont;
  xJust   := Source.xJust;
  nSiteAutoriz := Source.nSiteAutoriz;
end;

{ TinfNF3e }

procedure TinfNF3e.Assign(Source: TinfNF3e);
begin
  ID     := Source.ID;
  Versao := Source.Versao;
end;

function TinfNF3e.GetID: String;
begin
  Result := Copy(FID, 5, 44);
end;

function TinfNF3e.GetVersao: Real;
begin
  if FVersao <= 0 then
     Result := 2
  else
     Result := FVersao;
end;

function TinfNF3e.GetVersaoStr: String;
begin
  if FVersao <= 0 then
    FVersao := 2;

  Result := 'versao="'+FloatToString(FVersao,'.','#0.00')+'"';
end;

{ TNF3e }

procedure TNF3e.Assign(Source: TNF3e);
begin
  infNF3e.Assign(Source.infNF3e);
  Ide.Assign(Source.Ide);
  Emit.Assign(Source.Emit);
  Dest.Assign(Source.Dest);
  acessante.Assign(Source.acessante);
  gSub.Assign(Source.gSub);
  gJudic.Assign(Source.gJudic);
  gGrContrat.Assign(Source.gGrContrat);
  gMed.Assign(Source.gMed);
  gSCEE.Assign(Source.gSCEE);
  NFDet.Assign(Source.NFDet);
  Total.Assign(Source.Total);
  gFat.Assign(Source.gFat);
  gANEEL.Assign(Source.gANEEL);
  autXML.Assign(Source.autXML);
  infAdic.Assign(Source.infAdic);
  infRespTec.Assign(Source.infRespTec);
  infNF3eSupl.Assign(Source.infNF3eSupl);
  Signature.Assign(Source.Signature);
  procNF3e.Assign(Source.procNF3e);
end;

constructor TNF3e.Create;
begin
  inherited Create;

  FinfNF3e     := TinfNF3e.Create;
  FIde         := TIde.Create;
  FEmit        := TEmit.Create;
  FDest        := TDest.Create;
  Facessante   := Tacessante.Create;
  FgSub        := TgSub.Create;
  FgJudic      := TgJudic.Create;
  FgGrContrat  := TgGrContratCollection.Create;
  FgMed        := TgMedCollection.Create;
  FgSCEE       := TgSCEE.Create;
  FNFDet       := TNFDetCollection.Create;
  FTotal       := TTotal.Create;
  FgFat        := TgFat.Create;
  FgANEEL      := TgANEEL.Create;
  FautXML      := TautXMLCollection.Create;
  FinfAdic     := TinfAdic.Create;
  FinfRespTec  := TinfRespTec.Create;
  FinfNF3eSupl := TinfNF3eSupl.Create;
  FSignature   := TSignature.Create;
  FprocNF3e    := TProcNF3e.Create;

  FinfNF3e.Versao := 0;
  FIde.nSiteAutoriz := sa0;
end;

destructor TNF3e.Destroy;
begin
  FinfNF3e.Free;
  FIde.Free;
  FEmit.Free;
  FDest.Free;
  Facessante.Free;
  FgSub.Free;
  FgJudic.Free;
  FgGrContrat.Free;
  FgMed.Free;
  FgSCEE.Free;
  FNFDet.Free;
  FTotal.Free;
  FgFat.Free;
  FgANEEL.Free;
  FautXML.Free;
  FinfAdic.Free;
  FinfRespTec.Free;
  FinfNF3eSupl.Free;
  FSignature.Free;
  FprocNF3e.Free;

  inherited Destroy;
end;

procedure TNF3e.SetgGrContrat(const Value: TgGrContratCollection);
begin
  FgGrContrat := Value;
end;

procedure TNF3e.SetgMed(const Value: TgMedCollection);
begin
  FgMed := Value;
end;

procedure TNF3e.SetNFDet(const Value: TNFDetCollection);
begin
  FNFDet := Value;
end;

procedure TNF3e.SetautXML(const Value: TautXMLCollection);
begin
  FautXML := Value;
end;

{ TretTrib }

procedure TretTrib.Assign(Source: TretTrib);
begin
  vRetPIS    := Source.vRetPIS;
  vRetCOFINS := Source.vRetCOFINS;
  vRetCSLL   := Source.vRetCSLL;
  vBCIRRF    := Source.vBCIRRF;
  vIRRF      := Source.vIRRF;
end;

{ TgPIX }

procedure TgPIX.Assign(Source: TgPIX);
begin
  urlQRCodePIX := Source.urlQRCodePIX;
end;

end.
