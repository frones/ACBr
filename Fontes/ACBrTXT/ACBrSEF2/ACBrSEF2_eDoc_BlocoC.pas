{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou                                 }
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

unit ACBrSEF2_eDoc_BlocoC;

interface

Uses SysUtils, Classes, ACBrSEF2Conversao;

type
  TRegistroSEFC020 = Class;
  TRegistroSEFC020List = Class;
  TRegistroSEFC300List = Class;
  TRegistroSEFC550List = Class;
  TRegistroSEFC560List = Class;
  TRegistroSEFC600List = Class;
  TRegistroSEFC610List = Class;

  TRegistroSEFC001 = Class
  private
    fIND_DAD: TSEFIIIndicadorConteudo;
    fRegistrosC020: TRegistroSEFC020List;
    fRegistrosC550: TRegistroSEFC550List;
    fRegistrosC600: TRegistroSEFC600List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_DAD : TSEFIIIndicadorConteudo read fIND_DAD write fIND_DAD;
    property RegistrosC020: TRegistroSEFC020List  read fRegistrosC020 write fRegistrosC020;
    property RegistrosC550: TRegistroSEFC550List  read fRegistrosC550 write fRegistrosC550;
    property RegistrosC600: TRegistroSEFC600List  read fRegistrosC600 write fRegistrosC600;
  end;

  //LINHA C040: COMPLEMENTO DO DOCUMENTO - ISS
  TRegistroSEFC040 = Class
  private
    fVL_BC_RT_ISS: Currency;
    fVL_RT_ISS   : Currency;
    fVL_BC_ISS   : Currency;
    fVL_ISS      : Currency;
    fCOD_MUN_SERV: String;
  public
    property COD_MUN_SERV : String   read fCOD_MUN_SERV write fCOD_MUN_SERV;
    property VL_BC_ISS    : Currency read fVL_BC_ISS    write fVL_BC_ISS;
    property VL_ISS       : Currency read fVL_ISS       write fVL_ISS;
    property VL_BC_RT_ISS : Currency read fVL_BC_RT_ISS write fVL_BC_RT_ISS;
    property VL_RT_ISS    : Currency read fVL_RT_ISS    write fVL_RT_ISS ;
  end;
  

  //LINHA C020: DOCUMENTO - NOTA FISCAL (CÓDIGO 01), NOTA FISCAL DE PRODUTOR (CÓDIGO 04) E NOTA FISCAL ELETRÔNICA (CÓDIGO 55)
  TRegistroSEFC020 = Class
  private
    fVL_IPI     : Currency;
    fVL_FRT     : Currency;
    fVL_ACMO    : Currency;
    fVL_DOC     : Currency;
    fVL_DESC    : Currency;
    fVL_OUT_DA  : Currency;
    fVL_BC_ICMS : Currency;
    fVL_ICMS    : Currency;
    fVL_OP_ISS  : Currency;
    fVL_AT      : Currency;
    fVL_BC_ST   : Currency;
    fVL_SEG     : Currency;
    fVL_MERC    : Currency;
    fVL_ICMS_ST : Currency;
    fNUM_DOC    : Integer;
    fCHV_NFE    : String;
    fCOD_NAT    : String;
    fSER        : String;
    fCOD_INF_OBS: String;
    fCOD_PART   : String;
    fDT_DOC     : TDateTime;
    fDT_EMIS    : TDateTime;
    fCOD_SIT    : TCodigoSituacao;
    fIND_EMIT   : TIndiceEmissao;
    fIND_OPER   : TIndiceOperacao;
    fIND_PGTO   : TIndicePagamento;
    fCOD_MOD    : TSEFIIDocFiscalReferenciado;
    fRegistroC300: TRegistroSEFC300List;
    fRegistroC040: TRegistroSEFC040;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override;

    property IND_OPER   : TIndiceOperacao read fIND_OPER write fIND_OPER;
    property IND_EMIT   : TIndiceEmissao  read fIND_EMIT write fIND_EMIT;
    property COD_MOD    : TSEFIIDocFiscalReferenciado read fCOD_MOD write fCOD_MOD;
    property COD_SIT    : TCodigoSituacao  read fCOD_SIT  write fCOD_SIT;
    property IND_PGTO   : TIndicePagamento read fIND_PGTO write fIND_PGTO;
    property NUM_DOC    : Integer   read fNUM_DOC     write fNUM_DOC;
    property COD_PART   : String   read fCOD_PART    write fCOD_PART;
    property SER        : String    read fSER         write fSER;
    property CHV_NFE    : String    read fCHV_NFE     write fCHV_NFE;
    property COD_NAT    : String    read fCOD_NAT     write fCOD_NAT;
    property COD_INF_OBS: String    read fCOD_INF_OBS write fCOD_INF_OBS;
    property DT_EMIS    : TDateTime read fDT_EMIS     write fDT_EMIS;
    property DT_DOC     : TDateTime read fDT_DOC      write fDT_DOC;
    property VL_DOC     : Currency  read fVL_DOC      write fVL_DOC;
    property VL_DESC    : Currency  read fVL_DESC     write fVL_DESC;
    property VL_ACMO    : Currency  read fVL_ACMO     write fVL_ACMO;
    property VL_MERC    : Currency  read fVL_MERC     write fVL_MERC;
    property VL_FRT     : Currency  read fVL_FRT      write fVL_FRT;
    property VL_SEG     : Currency  read fVL_SEG      write fVL_SEG;
    property VL_OUT_DA  : Currency  read fVL_OUT_DA   write fVL_OUT_DA;
    property VL_OP_ISS  : Currency  read fVL_OP_ISS   write fVL_OP_ISS;
    property VL_BC_ICMS : Currency  read fVL_BC_ICMS  write fVL_BC_ICMS;
    property VL_ICMS    : Currency  read fVL_ICMS     write fVL_ICMS;
    property VL_BC_ST   : Currency  read fVL_BC_ST    write fVL_BC_ST;
    property VL_ICMS_ST : Currency  read fVL_ICMS_ST  write fVL_ICMS_ST;
    property VL_AT      : Currency  read fVL_AT       write fVL_AT;
    property VL_IPI     : Currency  read fVL_IPI      write fVL_IPI;

    property RegistrosC300: TRegistroSEFC300List read fRegistroC300 write fRegistroC300;
    property RegistrosC040: TRegistroSEFC040 read fRegistroC040 write fRegistroC040;
  end;

  TRegistroSEFC020List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEFC020;
    procedure SetItem(Index: Integer; const Value: TRegistroSEFC020);
  public
    function New(): TRegistroSEFC020;
    property Itens[Index: Integer]: TRegistroSEFC020 read GetItem write SetItem;
  end;


  TRegistroSEFC310 = Class
  private
    fALIQ_ISS: Currency;
    fCTISS: String;
    fVL_ISS_I: Currency;
    fVL_BC_ISS_I: Currency;
  public
    property CTISS        : String   read fCTISS       write fCTISS;
    property VL_BC_ISS_I  : Currency read fVL_BC_ISS_I write fVL_BC_ISS_I;
    property ALIQ_ISS     : Currency read fALIQ_ISS    write fALIQ_ISS;
    property VL_ISS_I     : Currency read fVL_ISS_I    write fVL_ISS_I;
  End;

  //LINHA C300: ITENS DO DOCUMENTO
  TRegistroSEFC300 = Class
  private
    fVL_ICMS_ST_I: Currency;
    fVL_UNIT     : Currency;
    fVL_ITEM     : Currency;
    fVL_BC_IPI   : Currency;
    fVL_ICMS_I   : Currency;
    fVL_BC_ICMS_I: Currency;
    fVL_DESC_I   : Currency;
    fVL_BC_ST_I  : Currency;
    fVL_ACMO_I   : Currency;
    fVL_IPI_I    : Currency;
    fQTD         : Double;
    fALIQ_ICMS   : Double;
    fALIQ_IPI    : Double;
    fALIQ_ST     : Double;
    fCOD_ITEM    : String;
    fNUM_ITEM    : Integer;
    fCFOP        : Integer;
    fCST         : String;
    fCOD_NCM     : String;
    fUNID        : String;
    fRegistroC310: TRegistroSEFC310;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override;
    property NUM_ITEM    : Integer  read fNUM_ITEM     write fNUM_ITEM;
    property CFOP        : Integer  read fCFOP         write FCFOP;
    property UNID        : String   read fUNID         write fUNID;
    property COD_ITEM    : String   read fCOD_ITEM     write fCOD_ITEM;
    property COD_NCM     : String   read fCOD_NCM      write fCOD_NCM;
    property CST         : String   read fCST          write fCST;
    property QTD         : Double   read fQTD          write fQTD;
    property ALIQ_ICMS   : Double   read fALIQ_ICMS    write fALIQ_ICMS;
    property ALIQ_ST     : Double   read fALIQ_ST      write fALIQ_ST;
    property ALIQ_IPI    : Double   read fALIQ_IPI     write fALIQ_IPI;
    property VL_UNIT     : Currency read fVL_UNIT      write fVL_UNIT;
    property VL_DESC_I   : Currency read fVL_DESC_I    write fVL_DESC_I;
    property VL_ACMO_I   : Currency read fVL_ACMO_I    write fVL_ACMO_I;
    property VL_ITEM     : Currency read fVL_ITEM      write fVL_ITEM;
    property VL_BC_ICMS_I: Currency read fVL_BC_ICMS_I write fVL_BC_ICMS_I;
    property VL_ICMS_I   : Currency read fVL_ICMS_I    write fVL_ICMS_I;
    property VL_BC_ST_I  : Currency read fVL_BC_ST_I   write fVL_BC_ST_I;
    property VL_ICMS_ST_I: Currency read fVL_ICMS_ST_I write fVL_ICMS_ST_I;
    property VL_BC_IPI   : Currency read fVL_BC_IPI    write fVL_BC_IPI;
    property VL_IPI_I    : Currency read fVL_IPI_I     write fVL_IPI_I;

    property RegistroC310: TRegistroSEFC310 read fRegistroC310 write fRegistroC310;
  end;

  TRegistroSEFC300List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEFC300;
    procedure SetItem(Index: Integer; const Value: TRegistroSEFC300);
  public
    function New(): TRegistroSEFC300;
    property Itens[Index: Integer]: TRegistroSEFC300 read GetItem write SetItem;
  end;

  //LINHA C550: DOCUMENTO - NOTA FISCAL DE VENDA AO CONSUMIDOR (CÓDIGO 02)

  { TRegistroSEFC550 }

  TRegistroSEFC550 = Class
  private
    fCNPJ_CONS   : String;
    fCPF_CONS    : String;
    fCOD_MOD     : TSEFIIDocFiscalReferenciado;
    FSERIE       : String;
    fCOP         : String;
    fCOD_INF_OBS : String;
    FSUBSERIE    : String;
    fNUM_DOC     : Integer;
    fVL_DOC      : Currency;
    fVL_DESC     : Currency;
    fVL_ACMO     : Currency;
    fVL_MERC     : Currency;
    fVL_BC_ICMS  : Currency;
    fVL_ICMS     : Currency;
    fDT_DOC      : TDateTime;
    fCOD_SIT     : TCodigoSituacao;
    fRegistroC560: TRegistroSEFC560List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy;

    property COD_SIT     : TCodigoSituacao read FCOD_SIT write FCOD_SIT;
    property CPF_CONS    : String    read FCPF_CONS      write FCPF_CONS;
    property CNPJ_CONS   : String    read FCNPJ_CONS     write FCNPJ_CONS;
    property COD_MOD     : TSEFIIDocFiscalReferenciado read FCOD_MOD       write FCOD_MOD;
    property COP         : String    read FCOP           write FCOP;
    property SERIE       : String    read FSERIE         write FSERIE;
    property SUBSERIE    : String    read FSUBSERIE      write FSUBSERIE;
    property COD_INF_OBS : String    read FCOD_INF_OBS   write FCOD_INF_OBS;
    property NUM_DOC     : Integer   read FNUM_DOC       write FNUM_DOC;
    property DT_DOC      : TDateTime read FDT_DOC        write FDT_DOC;
    property VL_DOC      : Currency  read FVL_DOC        write FVL_DOC;
    property VL_MERC     : Currency  read FVL_MERC       write FVL_MERC;
    property VL_DESC     : Currency  read FVL_DESC       write FVL_DESC;
    property VL_ACMO     : Currency  read FVL_ACMO       write FVL_ACMO;
    property VL_BC_ICMS  : Currency  read FVL_BC_ICMS    write FVL_BC_ICMS;
    property VL_ICMS     : Currency  read FVL_ICMS       write FVL_ICMS;

    property RegistroC560: TRegistroSEFC560List read FRegistroC560 write FRegistroC560;
  end;

  { TRegistroSEFC550List }

  TRegistroSEFC550List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEFC550;
    procedure SetItem(Index: Integer; const Value: TRegistroSEFC550);
  public
    function New(): TRegistroSEFC550;
    property Itens[Index: Integer]: TRegistroSEFC550 read GetItem write SetItem;
  end;

  //LINHA C560: ITENS DO DOCUMENTO

  { TRegistroSEFC560 }

  TRegistroSEFC560 = Class
  private
    fNUM_ITEM    : Integer;
    fCFOP        : Integer;
    fQTD         : Double;
    fALIQ_ICMS   : Double;
    fVL_UNIT     : Currency;
    fVL_ACMO_I   : Currency;
    fVL_DESC_I   : Currency;
    fVL_ITEM     : Currency;
    fVL_BC_ICMS_I: Currency;
    fVL_ICMS_I   : Currency;
    fCOD_ITEM    : String;
    fUNID        : String;
    fCST         : String;

  public
    constructor Create(AOwner: TRegistroSEFC550); virtual; /// Create

    property NUM_ITEM    : Integer  read fNUM_ITEM     write fNUM_ITEM;
    property CFOP        : Integer  read fCFOP         write fCFOP;
    property COD_ITEM    : String   read fCOD_ITEM     write fCOD_ITEM;
    property UNID        : String   read fUNID         write fUNID;
    property CST         : String   read fCST          write fCST;
    property QTD         : Double   read fQTD          write fQTD;
    property ALIQ_ICMS   : Double   read fALIQ_ICMS    write fALIQ_ICMS;
    property VL_UNIT     : Currency read fVL_UNIT      write fVL_UNIT;
    property VL_DESC_I   : Currency read fVL_DESC_I    write fVL_DESC_I;
    property VL_ACMO_I   : Currency read fVL_ACMO_I    write fVL_ACMO_I;
    property VL_ITEM     : Currency read fVL_ITEM      write fVL_ITEM;
    property VL_BC_ICMS_I: Currency read fVL_BC_ICMS_I write fVL_BC_ICMS_I;
    property VL_ICMS_I   : Currency read fVL_ICMS_I    write fVL_ICMS_I;
  end;

  { TRegistroSEFC560List }

  TRegistroSEFC560List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEFC560;
    procedure SetItem(Index: Integer; const Value: TRegistroSEFC560);
  public
    function New(AOwner: TRegistroSEFC550): TRegistroSEFC560;
    property Itens[Index: Integer]: TRegistroSEFC560 read GetItem write SetItem;
  end;


  //LINHA C605: COMPLEMENTO DO DOCUMENTO - ISS
  TRegistroSEFC605 = Class
  private
    fVL_ISS: Currency;
    fVL_BC_ISS: Currency;
    fVL_DESC_ISS: Currency;
    fVL_ACMO_ISS: Currency;
    fVL_ISN_ISS: Currency;
    fVL_CANC_ISS: Currency;
    fVL_NT_ISS: Currency;
  public
    property VL_CANC_ISS : Currency read fVL_CANC_ISS  write fVL_CANC_ISS;
    property VL_DESC_ISS : Currency read fVL_DESC_ISS  write fVL_DESC_ISS;
    property VL_ACMO_ISS : Currency read fVL_ACMO_ISS  write fVL_ACMO_ISS;
    property VL_BC_ISS   : Currency read fVL_BC_ISS    write fVL_BC_ISS  ;
    property VL_ISS      : Currency read fVL_ISS       write fVL_ISS     ;
    property VL_ISN_ISS  : Currency read fVL_ISN_ISS   write fVL_ISN_ISS ;
    property VL_NT_ISS   : Currency read fVL_NT_ISS    write fVL_NT_ISS  ;
  end;



  //LINHA C600: DOCUMENTO - CUPOM FISCAL/ICMS (CÓDIGO 2D E CÓDIGO 02)
  TRegistroSEFC600 = Class
  private
    fVL_CANC_ICMS: Currency;
    fVL_DOC      : Currency;
    fVL_DESC_ICMS: Currency;
    fVL_ACMO_ICMS: Currency;
    fVL_ISN      : Currency;
    fVL_ICMS     : Currency;
    fVL_BC_ICMS  : Currency;
    fVL_NT       : Currency;
    fVL_OP_ISS   : Currency;
    fVL_ST       : Currency;
    fECF_CX      : Integer;
    fNUM_DOC     : Integer;
    fCOP         : String;
    fCRZ         : Integer;
    fCRO         : Integer;
    fCNPJ_CONS   : String;
    fCOD_MOD     : TSEFIIDocFiscalReferenciado;
    fCPF_CONS    : String;
    fECF_FAB     : String;
    fDT_DOC      : TDateTime;
    fCOD_SIT     : TCodigoSituacao;
    fRegistroC610: TRegistroSEFC610List;
    FRegistroC605: TRegistroSEFC605;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy;

    property COD_SIT     : TCodigoSituacao read FCOD_SIT write FCOD_SIT;
    property CPF_CONS    : String    read FCPF_CONS     write FCPF_CONS;
    property CNPJ_CONS   : String    read FCNPJ_CONS    write FCNPJ_CONS;
    property COD_MOD     : TSEFIIDocFiscalReferenciado  read FCOD_MOD write FCOD_MOD;
    property ECF_FAB     : String    read FECF_FAB      write FECF_FAB;
    property ECF_CX      : Integer   read FECF_CX       write FECF_CX;
    property CRO         : Integer   read FCRO          write FCRO;
    property CRZ         : Integer   read FCRZ          write FCRZ;
    property NUM_DOC     : Integer   read FNUM_DOC      write FNUM_DOC;
    property COP         : String    read FCOP          write FCOP;
    property DT_DOC      : TDateTime read FDT_DOC       write FDT_DOC;
    property VL_DOC      : Currency  read FVL_DOC       write FVL_DOC;
    property VL_CANC_ICMS: Currency  read FVL_CANC_ICMS write FVL_CANC_ICMS;
    property VL_DESC_ICMS: Currency  read FVL_DESC_ICMS write FVL_DESC_ICMS;
    property VL_ACMO_ICMS: Currency  read FVL_ACMO_ICMS write FVL_ACMO_ICMS;
    property VL_OP_ISS   : Currency  read FVL_OP_ISS    write FVL_OP_ISS;
    property VL_BC_ICMS  : Currency  read FVL_BC_ICMS   write FVL_BC_ICMS;
    property VL_ICMS     : Currency  read FVL_ICMS      write FVL_ICMS;
    property VL_ISN      : Currency  read FVL_ISN       write FVL_ISN;
    property VL_NT       : Currency  read FVL_NT        write FVL_NT;
    property VL_ST       : Currency  read FVL_ST        write FVL_ST;

    property RegistroC605: TRegistroSEFC605 read FRegistroC605 write FRegistroC605;
    property RegistroC610: TRegistroSEFC610List read FRegistroC610 write FRegistroC610;
  end;

  TRegistroSEFC600List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEFC600;
    procedure SetItem(Index: Integer; const Value: TRegistroSEFC600);
  public
    function New(): TRegistroSEFC600;
    property Itens[Index: Integer]: TRegistroSEFC600 read GetItem write SetItem;
  end;

    //LINHA C615: COMPLEMENTO DO ITEM - ISS
  TRegistroSEFC615 = Class
  private
    fVL_ISN_ISS_I: currency;
    fVL_NT_ISS_I: currency;
    fALIQ_ISS: currency;
    fVL_ISS_I: currency;
    fVL_BC_ISS_I: currency;
  public
    property VL_BC_ISS_I  : currency read fVL_BC_ISS_I  write fVL_BC_ISS_I ;
    property ALIQ_ISS     : currency read fALIQ_ISS     write fALIQ_ISS    ;
    property VL_ISS_I     : currency read fVL_ISS_I     write fVL_ISS_I    ;
    property VL_ISN_ISS_I : currency read fVL_ISN_ISS_I write fVL_ISN_ISS_I;
    property VL_NT_ISS_I  : currency read fVL_NT_ISS_I  write fVL_NT_ISS_I ;
  end;

  //LINHA C610: ITENS DO DOCUMENTO
  TRegistroSEFC610 = Class
  private
    fVL_ST_I     : Currency;
    fVL_NT_I     : Currency;
    fVL_ITEM     : Currency;
    fVL_UNIT     : Currency;
    fVL_ICMS_I   : Currency;
    fVL_BC_ICMS_I: Currency;
    fVL_ACMO_I   : Currency;
    fVL_DESC_I   : Currency;
    fVL_ISN_I    : Currency;
    fALIQ_ICMS   : Double;
    fQTD         : Double;
    fCFOP        : String;
    fNUM_ITEM    : Integer;
    fCOD_ITEM    : String;
    fCST         : String;
    fUNID        : String;
    fRegistroC615: TRegistroSEFC615;
  public
    constructor Create(AOwner: TRegistroSEFC600); virtual; /// Create
    destructor Destroy; override;

    property NUM_ITEM    : Integer  read fNUM_ITEM     write fNUM_ITEM;
    property CFOP        : String   read fCFOP         write fCFOP;
    property COD_ITEM    : String   read fCOD_ITEM     write fCOD_ITEM;
    property UNID        : String   read fUNID         write fUNID;
    property CST         : String   read fCST          write fCST;
    property QTD         : Double   read fQTD          write fQTD;
    property ALIQ_ICMS   : Double   read fALIQ_ICMS    write fALIQ_ICMS;
    property VL_UNIT     : Currency read fVL_UNIT      write fVL_UNIT;
    property VL_DESC_I   : Currency read fVL_DESC_I    write fVL_DESC_I;
    property VL_ACMO_I   : Currency read fVL_ACMO_I    write fVL_ACMO_I;
    property VL_ITEM     : Currency read fVL_ITEM      write fVL_ITEM;
    property VL_BC_ICMS_I: Currency read fVL_BC_ICMS_I write fVL_BC_ICMS_I;
    property VL_ICMS_I   : Currency read fVL_ICMS_I    write fVL_ICMS_I;
    property VL_ISN_I    : Currency read fVL_ISN_I     write fVL_ISN_I;
    property VL_NT_I     : Currency read fVL_NT_I      write fVL_NT_I;
    property VL_ST_I     : Currency read fVL_ST_I      write fVL_ST_I;

    property RegistroC615 : TRegistroSEFC615 read fRegistroC615 write fRegistroC615;
  end;

  TRegistroSEFC610List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEFC610;
    procedure SetItem(Index: Integer; const Value: TRegistroSEFC610);
  public
    function New(AOwner: TRegistroSEFC600): TRegistroSEFC610;
    property Itens[Index: Integer]: TRegistroSEFC610 read GetItem write SetItem;
  end;

  TRegistroSEFC990 = Class
  private
    fQTD_LIN_C: Integer;
  public
    property QTD_LIN_C : Integer read fQTD_LIN_C write fQTD_LIN_C;
  end;

implementation

{ TRegistroSEFC560 }

constructor TRegistroSEFC560.Create(AOwner: TRegistroSEFC550);
begin

end;

{ TRegistroSEFC560List }

function TRegistroSEFC560List.GetItem(Index: Integer): TRegistroSEFC560;
begin
   Result := TRegistroSEFC560(Get(Index));
end;

procedure TRegistroSEFC560List.SetItem(Index: Integer;
  const Value: TRegistroSEFC560);
begin
   Put(Index, Value);
end;

function TRegistroSEFC560List.New(AOwner: TRegistroSEFC550): TRegistroSEFC560;
begin
   Result := TRegistroSEFC560.Create(AOwner);
   Add(Result);
end;

{ TRegistroSEFC550List }

function TRegistroSEFC550List.GetItem(Index: Integer): TRegistroSEFC550;
begin
   Result := TRegistroSEFC550(Get(Index));
end;

procedure TRegistroSEFC550List.SetItem(Index: Integer;
  const Value: TRegistroSEFC550);
begin
   Put(Index, Value);
end;

function TRegistroSEFC550List.New(): TRegistroSEFC550;
begin
   Result := TRegistroSEFC550.Create();
   Add(Result);
end;

{ TRegistroSEFC550 }

constructor TRegistroSEFC550.Create();
begin
   inherited Create;
   fRegistroC560 := TRegistroSEFC560List.Create;
end;

destructor TRegistroSEFC550.Destroy;
begin
  fRegistroC560.Free;
  inherited;
end;

{ TRegistroSEFC001 }

constructor TRegistroSEFC001.Create;
begin
   FRegistrosC020 := TRegistroSEFC020List.Create;
   fRegistrosC550 := TRegistroSEFC550List.create;
   FRegistrosC600 := TRegistroSEFC600List.Create;
end;

destructor TRegistroSEFC001.Destroy;
begin
   FRegistrosC020.Free;
   fRegistrosC550.Free;
   FRegistrosC600.Free;

   inherited;
end;

{ TRegistroSEFC020 }

constructor TRegistroSEFC020.Create();
begin
  inherited Create;
  FRegistroC300 := TRegistroSEFC300List.Create;
end;

destructor TRegistroSEFC020.Destroy;
begin
   fRegistroC300.Free;
   if (assigned(fRegistroC040)) then fRegistroC040.Free;
   inherited;
end;

{ TRegistroSEFC300List }

function TRegistroSEFC300List.GetItem(Index: Integer): TRegistroSEFC300;
begin
  Result := TRegistroSEFC300(Get(Index));
end;

function TRegistroSEFC300List.New(): TRegistroSEFC300;
begin
  Result := TRegistroSEFC300.Create();
  Add(Result);
end;

procedure TRegistroSEFC300List.SetItem(Index: Integer;
  const Value: TRegistroSEFC300);
begin
  Put(Index, Value);
end;

{ TRegistroSEFC600 }

constructor TRegistroSEFC600.Create();
begin
  inherited Create;
  FRegistroC610 := TRegistroSEFC610List.Create;
end;

destructor TRegistroSEFC600.Destroy;
begin
  if assigned(FRegistroC605) then FreeAndNil(FRegistroC605);

  FRegistroC610.Free;
  inherited;
end;

{ TRegistroSEFC020List }

function TRegistroSEFC020List.GetItem(Index: Integer): TRegistroSEFC020;
begin
  Result := TRegistroSEFC020(Get(Index));
end;

function TRegistroSEFC020List.New(): TRegistroSEFC020;
begin
  Result := TRegistroSEFC020.Create();
  Add(Result);
end;

procedure TRegistroSEFC020List.SetItem(Index: Integer;
  const Value: TRegistroSEFC020);
begin
  Put(Index, Value);
end;

{ TRegistroSEFC600List }

function TRegistroSEFC600List.GetItem(Index: Integer): TRegistroSEFC600;
begin
  Result := TRegistroSEFC600(Get(Index));
end;

function TRegistroSEFC600List.New(): TRegistroSEFC600;
begin
  Result := TRegistroSEFC600.Create();
  Add(Result);
end;

procedure TRegistroSEFC600List.SetItem(Index: Integer;
  const Value: TRegistroSEFC600);
begin
  Put(Index, Value);
end;

{ TRegistroSEFC610List }

function TRegistroSEFC610List.GetItem(Index: Integer): TRegistroSEFC610;
begin
  Result := TRegistroSEFC610(Inherited Items[Index]);
end;

function TRegistroSEFC610List.New(AOwner: TRegistroSEFC600): TRegistroSEFC610;
begin
  Result := TRegistroSEFC610.Create(AOwner);
  Add(Result);
end;

procedure TRegistroSEFC610List.SetItem(Index: Integer;
  const Value: TRegistroSEFC610);
begin
  Put(Index, Value);
end;

{ TRegistroSEFC990 }

{ TRegistroSEFC610 }

constructor TRegistroSEFC610.Create(AOwner: TRegistroSEFC600);
begin
end;

destructor TRegistroSEFC610.Destroy;
begin
  if assigned(fRegistroC615) then FreeAndNil(fRegistroC615);  

  inherited;
end;

{ TRegistroSEFC300 }

constructor TRegistroSEFC300.Create();
begin
  inherited Create;
end;

destructor TRegistroSEFC300.Destroy;
begin
  if assigned(fRegistroC310) then fRegistroC310.free;

  inherited;
end;

end.
