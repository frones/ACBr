{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Ribamar M. Santos                               }
{                              Juliomar Marchetti                              }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrADRCST_Bloco1;

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  DateUtils,
  ACBrADRCST_Blocos,
  ACBrADRCSTConversao;

type
  TRegistro1000List = class;
  TRegistro1001List = class;
  TRegistro1010 = class;
  TRegistro1100 = class;
  TRegistro1101 = class;

  TRegistro1110 = class;
  TRegistro1111 = class;
  TRegistro1120 = class;
  TRegistro1110List = class;
  TRegistro1111List = class;
  TRegistro1120List = class;

  TRegistro1200 = class;
  TRegistro1210 = class;
  TRegistro1220 = class;
  TRegistro1210List = class;
  TRegistro1220List = class;

  TRegistro1300 = class;
  TRegistro1310 = class;
  TRegistro1320 = class;
  TRegistro1310List = class;
  TRegistro1320List = class;

  TRegistro1400 = class;
  TRegistro1410 = class;
  TRegistro1420 = class;
  TRegistro1410List = class;
  TRegistro1420List = class;

  TRegistro1500 = class;
  TRegistro1510 = class;
  TRegistro1520 = class;
  TRegistro1510List = class;
  TRegistro1520List = class;

  TRegistro1999 = class;

  { TRegistro1000 }

  TRegistro1000 = class(TBlocos)
  private
    FIND_FECOP: TADRCSTIndicadorProdutoFECOP;
    FCOD_ITEM: string;
    FCOD_BARRAS: string;
    FCOD_ANP: string;
    FNCM: string;
    FCEST: string;
    FDESCR_ITEM: string;
    FRegistro1999: TRegistro1999;
    FUNID_ITEM: string;
    FALIQ_ICMS_ITEM: double;
    FALIQ_FECOP: double;
    FQTD_TOT_ENTRADA: double;
    FQTD_TOT_SAIDA: double;

    FRegistro1010: TRegistro1010;
    FRegistro1100: TRegistro1100;
    FRegistro1200: TRegistro1200;
    FRegistro1300: TRegistro1300;
    FRegistro1400: TRegistro1400;
    FRegistro1500: TRegistro1500;

    function GetRegistro1010: TRegistro1010;
    function GetRegistro1200: TRegistro1200;
    function GetRegistro1300: TRegistro1300;
    function GetRegistro1400: TRegistro1400;

    function GetRegistro1500: TRegistro1500;
  public
    constructor Create( ARegistro1999: TRegistro1999);overload;
    destructor Destroy; override;

    property IND_FECOP: TADRCSTIndicadorProdutoFECOP read FIND_FECOP write FIND_FECOP;
    property COD_ITEM: string read FCOD_ITEM write FCOD_ITEM;
    property COD_BARRAS: string read FCOD_BARRAS write FCOD_BARRAS;
    property COD_ANP: string read FCOD_ANP write FCOD_ANP;
    property NCM: string read FNCM write FNCM;
    property CEST: string read FCEST write FCEST;
    property DESCR_ITEM: string read FDESCR_ITEM write FDESCR_ITEM;
    property UNID_ITEM: string read FUNID_ITEM write FUNID_ITEM;
    property ALIQ_ICMS_ITEM: double read FALIQ_ICMS_ITEM write FALIQ_ICMS_ITEM;
    property ALIQ_FECOP: double read FALIQ_FECOP write FALIQ_FECOP;
    property QTD_TOT_ENTRADA: double read FQTD_TOT_ENTRADA write FQTD_TOT_ENTRADA;
    property QTD_TOT_SAIDA: double read FQTD_TOT_SAIDA write FQTD_TOT_SAIDA;

    property Registro1010: TRegistro1010 read GetRegistro1010;
    function Registro1010New: TRegistro1010;

    property Registro1100: TRegistro1100 read FRegistro1100 write FRegistro1100;

    property Registro1200: TRegistro1200 read GetRegistro1200 write FRegistro1200;
    function Registro1200New: TRegistro1200;

    property Registro1300: TRegistro1300 read GetRegistro1300 write FRegistro1300;
    function Registro1300New: TRegistro1300;

    property Registro1400: TRegistro1400 read GetRegistro1400 write FRegistro1400;
    function Registro1400New: TRegistro1400;

    property Registro1500: TRegistro1500 read GetRegistro1500 write FRegistro1500;
    function Registro1500New: TRegistro1500;

    property Registro1999: TRegistro1999 read FRegistro1999 write FRegistro1999;
  end;

  { TRegistro1000List }

  TRegistro1000List = class(TObjectList)
  private
    FRegistro1999: TRegistro1999;
    function GetItem(Index: integer): TRegistro1000;
    procedure SetItem(Index: integer; const Value: TRegistro1000);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

    function New(): TRegistro1000;
    property Items[Index: integer]: TRegistro1000 read GetItem write SetItem;

    property Registro1999: TRegistro1999 read FRegistro1999;
  end;

  TRegistro1001 = class(TBlocos)
  private
    FIND_FECOP: TADRCSTIndicadorProdutoFECOP;
    FCOD_ITEM: string;
    FCOD_BARRAS: string;
    FCOD_ANP: string;
    FNCM: string;
    FCEST: string;
    FDESCR_ITEM: string;
    FRegistro1999: TRegistro1999;
    FUNID_ITEM: string;
    FALIQ_ICMS_ITEM: double;
    FALIQ_FECOP: double;

    FRegistro1101: TRegistro1101;
  public
    constructor Create(ARegistro1999: TRegistro1999);overload;
    destructor Destroy; override;

    property IND_FECOP: TADRCSTIndicadorProdutoFECOP read FIND_FECOP write FIND_FECOP;
    property COD_ITEM: string read FCOD_ITEM write FCOD_ITEM;
    property COD_BARRAS: string read FCOD_BARRAS write FCOD_BARRAS;
    property COD_ANP: string read FCOD_ANP write FCOD_ANP;
    property NCM: string read FNCM write FNCM;
    property CEST: string read FCEST write FCEST;
    property DESCR_ITEM: string read FDESCR_ITEM write FDESCR_ITEM;
    property UNID_ITEM: string read FUNID_ITEM write FUNID_ITEM;
    property ALIQ_ICMS_ITEM: double read FALIQ_ICMS_ITEM write FALIQ_ICMS_ITEM;
    property ALIQ_FECOP: double read FALIQ_FECOP write FALIQ_FECOP;

    property Registro1101: TRegistro1101 read FRegistro1101 write FRegistro1101;
    property Registro1999: TRegistro1999 read FRegistro1999 write FRegistro1999;
  end;

  { TRegistro1001List }

  TRegistro1001List = class(TObjectList)
  private
    FRegistro1999: TRegistro1999;
    function GetItem(Index: integer): TRegistro1001;
    procedure SetItem(Index: integer; const Value: TRegistro1001);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

    function New(): TRegistro1001;
    property Items[Index: integer]: TRegistro1001 read GetItem write SetItem;

    property Registro1999: TRegistro1999 read FRegistro1999;
  end;

  { TRegistro1010 }

  TRegistro1010 = class(TBlocos)
  private
    FCOD_ITEM: string;
    FUNID_ITEM: string;
    FQTD: double;
    FVL_TOT_ITEM: double;
    FTXT_COMPL: string;
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    //destructor Destroy; override;

    property COD_ITEM: string read FCOD_ITEM;
    property UNID_ITEM: string read FUNID_ITEM;
    property QTD: double read FQTD write FQTD;
    property VL_TOT_ITEM: double read FVL_TOT_ITEM write FVL_TOT_ITEM;
    property TXT_COMPL: string read FTXT_COMPL write FTXT_COMPL;
  end;

  { TRegistro1100 }

  TRegistro1100 = class(TBlocos)
  private
    FQTD_TOT_ENTRADA: double;
    FMENOR_VL_UNIT_ITEM: double;
    FVL_BC_ICMSST_UNIT_MED: double;
    FVL_TOT_ICMS_SUPORT_ENTR: double;
    FVL_UNIT_MED_ICMS_SUPORT_ENTR: double;

    FRegistro1110List: TRegistro1110List;
    FRegistro1120List: TRegistro1120List;
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    destructor Destroy; override;

    property QTD_TOT_ENTRADA: double read FQTD_TOT_ENTRADA write FQTD_TOT_ENTRADA;
    property MENOR_VL_UNIT_ITEM: double read FMENOR_VL_UNIT_ITEM write FMENOR_VL_UNIT_ITEM;
    property VL_BC_ICMSST_UNIT_MED: double read FVL_BC_ICMSST_UNIT_MED write FVL_BC_ICMSST_UNIT_MED;
    property VL_TOT_ICMS_SUPORT_ENTR: double read FVL_TOT_ICMS_SUPORT_ENTR write FVL_TOT_ICMS_SUPORT_ENTR;
    property VL_UNIT_MED_ICMS_SUPORT_ENTR: double read FVL_UNIT_MED_ICMS_SUPORT_ENTR write FVL_UNIT_MED_ICMS_SUPORT_ENTR;


    property Registro1110List: TRegistro1110List read FRegistro1110List write FRegistro1110List;
    property Registro1120List: TRegistro1120List read FRegistro1120List write FRegistro1120List;
  end;

  { TRegistro1101 }

  TRegistro1101 = class(TBlocos)
  private
    FQTD_TOT_ENTRADA: double;
    FVL_BC_ICMSST_UNIT_MED: double;
    FVL_TOT_ICMS_SUPORT_ENTR: double;
    FVL_UNIT_MED_ICMS_SUPORT_ENTR: double;
    FQTD_TRANF: double;

    FRegistro1111List: TRegistro1111List;
  public
    constructor Create(const ARegistro1001: TRegistro1001);
    destructor Destroy; override;

    property QTD_TOT_ENTRADA: double read FQTD_TOT_ENTRADA write FQTD_TOT_ENTRADA;
    property VL_BC_ICMSST_UNIT_MED: double read FVL_BC_ICMSST_UNIT_MED write FVL_BC_ICMSST_UNIT_MED;
    property VL_TOT_ICMS_SUPORT_ENTR: double read FVL_TOT_ICMS_SUPORT_ENTR write FVL_TOT_ICMS_SUPORT_ENTR;
    property VL_UNIT_MED_ICMS_SUPORT_ENTR: double read FVL_UNIT_MED_ICMS_SUPORT_ENTR write FVL_UNIT_MED_ICMS_SUPORT_ENTR;
    property QTD_TRANF: double read FQTD_TRANF write FQTD_TRANF;


    property Registro1111List: TRegistro1111List read FRegistro1111List write FRegistro1111List;
  end;

  { TRegistro1110List }

  TRegistro1110List = class(TObjectList)
  private
    FRegistro1000: TRegistro1000;
    function GetItem(Index: integer): TRegistro1110;
    procedure SetItem(Index: integer; const Value: TRegistro1110);
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    function New(): TRegistro1110;
    property Items[Index: integer]: TRegistro1110 read GetItem write SetItem;
  end;

  TRegistro1110 = class(TBlocos)
  private
    FDT_DOC: TDateTime;
    FCOD_RESP_RET: TADRCSTIndicadorResponsavelRetencao;
    FCST_CSOSN: string;
    FCHAVE: string;
    FN_NF: integer;
    FCNPJ_EMIT: string;
    FUF_EMIT: string;
    FCNPJ_DEST: string;
    FUF_DEST: string;
    FCFOP: integer;
    FN_ITEM: integer;
    FUNID_ITEM: string;
    FQTD_ENTRADA: double;
    FVL_UNIT_ITEM: double;
    FVL_BC_ICMS_ST: double;
    FVL_ICMS_SUPORT_ENTR: double;
  public
    constructor Create(const ARegistro: TRegistro1000);
    //destructor Destroy; override;

    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property COD_RESP_RET: TADRCSTIndicadorResponsavelRetencao read FCOD_RESP_RET write FCOD_RESP_RET;
    property CST_CSOSN: string read FCST_CSOSN write FCST_CSOSN;
    property CHAVE: string read FCHAVE write FCHAVE;
    property N_NF: integer read FN_NF write FN_NF;
    property CNPJ_EMIT: string read FCNPJ_EMIT write FCNPJ_EMIT;
    property UF_EMIT: string read FUF_EMIT write FUF_EMIT;
    property CNPJ_DEST: string read FCNPJ_DEST write FCNPJ_DEST;
    property UF_DEST: string read FUF_DEST write FUF_DEST;
    property CFOP: integer read FCFOP write FCFOP;
    property N_ITEM: integer read FN_ITEM write FN_ITEM;
    property UNID_ITEM: string read FUNID_ITEM;
    property QTD_ENTRADA: double read FQTD_ENTRADA write FQTD_ENTRADA;
    property VL_UNIT_ITEM: double read FVL_UNIT_ITEM write FVL_UNIT_ITEM;
    property VL_BC_ICMS_ST: double read FVL_BC_ICMS_ST write FVL_BC_ICMS_ST;
    property VL_ICMS_SUPORT_ENTR: double read FVL_ICMS_SUPORT_ENTR write FVL_ICMS_SUPORT_ENTR;
  end;

  { TRegistro1111List }

  TRegistro1111List = class(TObjectList)
  private
    FRegistro1001: TRegistro1001;
    function GetItem(Index: integer): TRegistro1111;
    procedure SetItem(Index: integer; const Value: TRegistro1111);
  public
    constructor Create(const ARegistro1001: TRegistro1001);
    function New(): TRegistro1111;
    property Items[Index: integer]: TRegistro1111 read GetItem write SetItem;
  end;

  TRegistro1111 = class(TBlocos)
  private
    FDT_DOC: TDateTime;
    FCOD_RESP_RET: TADRCSTIndicadorResponsavelRetencao;
    FCST_CSOSN: string;
    FCHAVE: string;
    FN_NF: integer;
    FCNPJ_EMIT: string;
    FUF_EMIT: string;
    FCNPJ_DEST: string;
    FUF_DEST: string;
    FCFOP: integer;
    FN_ITEM: integer;
    FUNID_ITEM: string;
    FQTD_ENTRADA: double;
    FVL_UNIT_ITEM: double;
    FVL_BC_ICMS_ST: double;
    FVL_ICMS_SUPORT_ENTR: double;
  public
    constructor Create(const ARegistro: TRegistro1001);
    //destructor Destroy; override;

    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property COD_RESP_RET: TADRCSTIndicadorResponsavelRetencao read FCOD_RESP_RET write FCOD_RESP_RET;
    property CST_CSOSN: string read FCST_CSOSN write FCST_CSOSN;
    property CHAVE: string read FCHAVE write FCHAVE;
    property N_NF: integer read FN_NF write FN_NF;
    property CNPJ_EMIT: string read FCNPJ_EMIT write FCNPJ_EMIT;
    property UF_EMIT: string read FUF_EMIT write FUF_EMIT;
    property CNPJ_DEST: string read FCNPJ_DEST write FCNPJ_DEST;
    property UF_DEST: string read FUF_DEST write FUF_DEST;
    property CFOP: integer read FCFOP write FCFOP;
    property N_ITEM: integer read FN_ITEM write FN_ITEM;
    property UNID_ITEM: string read FUNID_ITEM;
    property QTD_ENTRADA: double read FQTD_ENTRADA write FQTD_ENTRADA;
    property VL_UNIT_ITEM: double read FVL_UNIT_ITEM write FVL_UNIT_ITEM;
    property VL_BC_ICMS_ST: double read FVL_BC_ICMS_ST write FVL_BC_ICMS_ST;
    property VL_ICMS_SUPORT_ENTR: double read FVL_ICMS_SUPORT_ENTR write FVL_ICMS_SUPORT_ENTR;
  end;

  { TRegistro1120List }

  TRegistro1120List = class(TObjectList)
  private
    FRegistro1000: TRegistro1000;
    function GetItem(Index: integer): TRegistro1120;
    procedure SetItem(Index: integer; const Value: TRegistro1120);
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    function New(): TRegistro1120;
    property Items[Index: integer]: TRegistro1120 read GetItem write SetItem;
  end;

  TRegistro1120 = class(TBlocos)
  private
    FDT_DOC: TDateTime;
    FCST_CSOSN: string;
    FCHAVE: string;
    FN_NF: integer;
    FCNPJ_EMIT: string;
    FUF_EMIT: string;
    FCNPJ_DEST: string;
    FUF_DEST: string;
    FCFOP: integer;
    FN_ITEM: integer;
    FUNID_ITEM: string;
    FQTD_DEVOLVIDA: double;
    FVL_UNIT_ITEM: double;
    FVL_BC_ICMS_ST: double;
    FVL_ICMS_SUPORT_ENTR: double;
    FCHAVE_REF: string;
    FN_ITEM_REF: integer;
  public
    constructor Create(const ARegistro: TRegistro1000);
    //destructor Destroy; override;

    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CST_CSOSN: string read FCST_CSOSN write FCST_CSOSN;
    property CHAVE: string read FCHAVE write FCHAVE;
    property N_NF: integer read FN_NF write FN_NF;
    property CNPJ_EMIT: string read FCNPJ_EMIT write FCNPJ_EMIT;
    property UF_EMIT: string read FUF_EMIT write FUF_EMIT;
    property CNPJ_DEST: string read FCNPJ_DEST write FCNPJ_DEST;
    property UF_DEST: string read FUF_DEST write FUF_DEST;
    property CFOP: integer read FCFOP write FCFOP;
    property N_ITEM: integer read FN_ITEM write FN_ITEM;
    property UNID_ITEM: string read FUNID_ITEM write FUNID_ITEM;
    property QTD_DEVOLVIDA: double read FQTD_DEVOLVIDA write FQTD_DEVOLVIDA;
    property VL_UNIT_ITEM: double read FVL_UNIT_ITEM write FVL_UNIT_ITEM;
    property VL_BC_ICMS_ST: double read FVL_BC_ICMS_ST write FVL_BC_ICMS_ST;
    property VL_ICMS_SUPORT_ENTR: double read FVL_ICMS_SUPORT_ENTR write FVL_ICMS_SUPORT_ENTR;
    property CHAVE_REF: string read FCHAVE_REF write FCHAVE_REF;
    property N_ITEM_REF: integer read FN_ITEM_REF write FN_ITEM_REF;
  end;


  { TRegistro1200 }

  TRegistro1200 = class(TBlocos)
  private
    FQTD_TOT_SAIDA: double;
    FVL_TOT_ICMS_EFETIVO: double;
    FVL_CONFRONTO_ICMS_ENTRADA: double;
    FRESULT_RECUPERAR_RESSARCIR: double;
    FRESULT_COMPLEMENTAR: double;
    FAPUR_ICMSST_RECUPERAR_RESSARCIR: double;
    FAPUR_ICMSST_COMPLEMENTAR: double;
    FAPUR_FECOP_RESSARCIR: double;
    FAPUR_FECOP_COMPLEMENTAR: double;

    FRegistro1210List: TRegistro1210List;
    FRegistro1220List: TRegistro1220List;
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    destructor Destroy; override;


    property QTD_TOT_SAIDA: double read FQTD_TOT_SAIDA write FQTD_TOT_SAIDA;
    property VL_TOT_ICMS_EFETIVO: double read FVL_TOT_ICMS_EFETIVO write FVL_TOT_ICMS_EFETIVO;
    property VL_CONFRONTO_ICMS_ENTRADA: double read FVL_CONFRONTO_ICMS_ENTRADA write FVL_CONFRONTO_ICMS_ENTRADA;
    property RESULT_RECUPERAR_RESSARCIR: double read FRESULT_RECUPERAR_RESSARCIR write FRESULT_RECUPERAR_RESSARCIR;
    property RESULT_COMPLEMENTAR: double read FRESULT_COMPLEMENTAR write FRESULT_COMPLEMENTAR;
    property APUR_ICMSST_RECUPERAR_RESSARCIR: double read FAPUR_ICMSST_RECUPERAR_RESSARCIR write FAPUR_ICMSST_RECUPERAR_RESSARCIR;
    property APUR_ICMSST_COMPLEMENTAR: double read FAPUR_ICMSST_COMPLEMENTAR write FAPUR_ICMSST_COMPLEMENTAR;
    property APUR_FECOP_RESSARCIR: double read FAPUR_FECOP_RESSARCIR write FAPUR_FECOP_RESSARCIR;
    property APUR_FECOP_COMPLEMENTAR: double read FAPUR_FECOP_COMPLEMENTAR write FAPUR_FECOP_COMPLEMENTAR;

    property Registro1210List: TRegistro1210List read FRegistro1210List write FRegistro1210List;
    property Registro1220List: TRegistro1220List read FRegistro1220List write FRegistro1220List;
  end;

  { TRegistro1210List }

  TRegistro1210List = class(TObjectList)
  private
    FRegistro1000: TRegistro1000;
    function GetItem(Index: integer): TRegistro1210;
    procedure SetItem(Index: integer; const Value: TRegistro1210);
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    function New(): TRegistro1210;
    property Items[Index: integer]: TRegistro1210 read GetItem write SetItem;
  end;

  TRegistro1210 = class(TBlocos)
  private
    FDT_DOC: TDateTime;
    FCST_CSOSN: string;
    FCHAVE: string;
    FN_NF: integer;
    FCNPJ_EMIT: string;
    FUF_EMIT: string;
    FCNPJ_CPF_DEST: string;
    FUF_DEST: string;
    FCFOP: integer;
    FN_ITEM: integer;
    FUNID_ITEM: string;
    FQTD_SAIDA: double;
    FVL_UNIT_ITEM: double;
    FVL_ICMS_EFETIVO: double;
  public
    constructor Create(const ARegistro: TRegistro1000);
    //destructor Destroy; override;

    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CST_CSOSN: string read FCST_CSOSN write FCST_CSOSN;
    property CHAVE: string read FCHAVE write FCHAVE;
    property N_NF: integer read FN_NF write FN_NF;
    property CNPJ_EMIT: string read FCNPJ_EMIT write FCNPJ_EMIT;
    property UF_EMIT: string read FUF_EMIT write FUF_EMIT;
    property CNPJ_CPF_DEST: string read FCNPJ_CPF_DEST write FCNPJ_CPF_DEST;
    property UF_DEST: string read FUF_DEST write FUF_DEST;
    property CFOP: integer read FCFOP write FCFOP;
    property N_ITEM: integer read FN_ITEM write FN_ITEM;
    property UNID_ITEM: string read FUNID_ITEM;
    property QTD_SAIDA: double read FQTD_SAIDA write FQTD_SAIDA;
    property VL_UNIT_ITEM: double read FVL_UNIT_ITEM write FVL_UNIT_ITEM;
    property VL_ICMS_EFETIVO: double read FVL_ICMS_EFETIVO write FVL_ICMS_EFETIVO;
  end;

  { TRegistro1220List }

  TRegistro1220List = class(TObjectList)
  private
    FRegistro1000: TRegistro1000;
    function GetItem(Index: integer): TRegistro1220;
    procedure SetItem(Index: integer; const Value: TRegistro1220);
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    function New(): TRegistro1220;
    property Items[Index: integer]: TRegistro1220 read GetItem write SetItem;
  end;

  TRegistro1220 = class(TBlocos)
  private
    FDT_DOC: TDateTime;
    FCST_CSOSN: string;
    FCHAVE: string;
    FN_NF: integer;
    FCNPJ_EMIT: string;
    FUF_EMIT: string;
    FCNPJ_CPF_DEST: string;
    FUF_DEST: string;
    FCFOP: integer;
    FN_ITEM: integer;
    FUNID_ITEM: string;
    FQTD_DEVOLVIDA: double;
    FVL_UNIT_ITEM: double;
    FVL_ICMS_EFETIVO: double;
    FCHAVE_REF: string;
    FN_ITEM_REF: integer;
  public
    constructor Create(const ARegistro: TRegistro1000);
    //destructor Destroy; override;

    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CST_CSOSN: string read FCST_CSOSN write FCST_CSOSN;
    property CHAVE: string read FCHAVE write FCHAVE;
    property N_NF: integer read FN_NF write FN_NF;
    property CNPJ_EMIT: string read FCNPJ_EMIT write FCNPJ_EMIT;
    property UF_EMIT: string read FUF_EMIT write FUF_EMIT;
    property CNPJ_CPF_DEST: string read FCNPJ_CPF_DEST write FCNPJ_CPF_DEST;
    property UF_DEST: string read FUF_DEST write FUF_DEST;
    property CFOP: integer read FCFOP write FCFOP;
    property N_ITEM: integer read FN_ITEM write FN_ITEM;
    property UNID_ITEM: string read FUNID_ITEM write FUNID_ITEM;
    property QTD_DEVOLVIDA: double read FQTD_DEVOLVIDA write FQTD_DEVOLVIDA;
    property VL_UNIT_ITEM: double read FVL_UNIT_ITEM write FVL_UNIT_ITEM;
    property VL_ICMS_EFETIVO: double read FVL_ICMS_EFETIVO write FVL_ICMS_EFETIVO;
    property CHAVE_REF: string read FCHAVE_REF write FCHAVE_REF;
    property N_ITEM_REF: integer read FN_ITEM_REF write FN_ITEM_REF;
  end;

  { TRegistro1300 }

  TRegistro1300 = class(TBlocos)
  private
    FQTD_TOT_SAIDA: double;
    FRegistro1310List: TRegistro1310List;
    FRegistro1320List: TRegistro1320List;
    FVL_TOT_ICMS_EFETIVO: double;
    FVL_CONFRONTO_ICMS_ENTRADA: double;
    FRESULT_RECUPERAR_RESSARCIR: double;
    FAPUR_ICMSST_RECUPERAR_RESSARCIR: double;
    FAPUR_FECOP_RESSARCIR: double;

  public
    constructor Create(const ARegistro1000: TRegistro1000);
    destructor Destroy; override;

    property QTD_TOT_SAIDA: double read FQTD_TOT_SAIDA write FQTD_TOT_SAIDA;
    property VL_TOT_ICMS_EFETIVO: double read FVL_TOT_ICMS_EFETIVO write FVL_TOT_ICMS_EFETIVO;
    property VL_CONFRONTO_ICMS_ENTRADA: double read FVL_CONFRONTO_ICMS_ENTRADA write FVL_CONFRONTO_ICMS_ENTRADA;
    property RESULT_RECUPERAR_RESSARCIR: double read FRESULT_RECUPERAR_RESSARCIR write FRESULT_RECUPERAR_RESSARCIR;
    property APUR_ICMSST_RECUPERAR_RESSARCIR: double read FAPUR_ICMSST_RECUPERAR_RESSARCIR write FAPUR_ICMSST_RECUPERAR_RESSARCIR;
    property APUR_FECOP_RESSARCIR: double read FAPUR_FECOP_RESSARCIR write FAPUR_FECOP_RESSARCIR;

    property Registro1310List: TRegistro1310List read FRegistro1310List write FRegistro1310List;
    property Registro1320List: TRegistro1320List read FRegistro1320List write FRegistro1320List;
  end;

  { TRegistro1310List }

  TRegistro1310List = class(TObjectList)
  private
    FRegistro1000: TRegistro1000;
    function GetItem(Index: integer): TRegistro1310;
    procedure SetItem(Index: integer; const Value: TRegistro1310);
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    function New(): TRegistro1310;
    property Items[Index: integer]: TRegistro1310 read GetItem write SetItem;
  end;

  TRegistro1310 = class(TBlocos)
  private
    FDT_DOC: TDateTime;
    FCST_CSOSN: string;
    FCHAVE: string;
    FN_NF: integer;
    FCNPJ_EMIT: string;
    FUF_EMIT: string;
    FCNPJ_CPF_DEST: string;
    FUF_DEST: string;
    FCFOP: integer;
    FN_ITEM: integer;
    FUNID_ITEM: string;
    FQTD_SAIDA: double;
    FVL_UNIT_ITEM: double;
    FVL_ICMS_EFETIVO: double;
  public
    constructor Create(const ARegistro: TRegistro1000);
    //destructor Destroy; override;

    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CST_CSOSN: string read FCST_CSOSN write FCST_CSOSN;
    property CHAVE: string read FCHAVE write FCHAVE;
    property N_NF: integer read FN_NF write FN_NF;
    property CNPJ_EMIT: string read FCNPJ_EMIT write FCNPJ_EMIT;
    property UF_EMIT: string read FUF_EMIT write FUF_EMIT;
    property CNPJ_CPF_DEST: string read FCNPJ_CPF_DEST write FCNPJ_CPF_DEST;
    property UF_DEST: string read FUF_DEST write FUF_DEST;
    property CFOP: integer read FCFOP write FCFOP;
    property N_ITEM: integer read FN_ITEM write FN_ITEM;
    property UNID_ITEM: string read FUNID_ITEM;
    property QTD_SAIDA: double read FQTD_SAIDA write FQTD_SAIDA;
    property VL_UNIT_ITEM: double read FVL_UNIT_ITEM write FVL_UNIT_ITEM;
    property VL_ICMS_EFETIVO: double read FVL_ICMS_EFETIVO write FVL_ICMS_EFETIVO;
  end;

  { TRegistro1320List }

  TRegistro1320List = class(TObjectList)
  private
    FRegistro1000: TRegistro1000;
    function GetItem(Index: integer): TRegistro1320;
    procedure SetItem(Index: integer; const Value: TRegistro1320);
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    function New(): TRegistro1320;
    property Items[Index: integer]: TRegistro1320 read GetItem write SetItem;
  end;

  TRegistro1320 = class(TBlocos)
  private
    FDT_DOC: TDateTime;
    FCST_CSOSN: string;
    FCHAVE: string;
    FN_NF: integer;
    FCNPJ_EMIT: string;
    FUF_EMIT: string;
    FCNPJ_CPF_DEST: string;
    FUF_DEST: string;
    FCFOP: integer;
    FN_ITEM: integer;
    FUNID_ITEM: string;
    FQTD_DEVOLVIDA: double;
    FVL_UNIT_ITEM: double;
    FVL_ICMS_EFETIVO: double;
    FCHAVE_REF: string;
    FN_ITEM_REF: integer;
  public
    constructor Create(const ARegistro: TRegistro1000);
    //destructor Destroy; override;

    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CST_CSOSN: string read FCST_CSOSN write FCST_CSOSN;
    property CHAVE: string read FCHAVE write FCHAVE;
    property N_NF: integer read FN_NF write FN_NF;
    property CNPJ_EMIT: string read FCNPJ_EMIT write FCNPJ_EMIT;
    property UF_EMIT: string read FUF_EMIT write FUF_EMIT;
    property CNPJ_CPF_DEST: string read FCNPJ_CPF_DEST write FCNPJ_CPF_DEST;
    property UF_DEST: string read FUF_DEST write FUF_DEST;
    property CFOP: integer read FCFOP write FCFOP;
    property N_ITEM: integer read FN_ITEM write FN_ITEM;
    property UNID_ITEM: string read FUNID_ITEM write FUNID_ITEM;
    property QTD_DEVOLVIDA: double read FQTD_DEVOLVIDA write FQTD_DEVOLVIDA;
    property VL_UNIT_ITEM: double read FVL_UNIT_ITEM write FVL_UNIT_ITEM;
    property VL_ICMS_EFETIVO: double read FVL_ICMS_EFETIVO write FVL_ICMS_EFETIVO;
    property CHAVE_REF: string read FCHAVE_REF write FCHAVE_REF;
    property N_ITEM_REF: integer read FN_ITEM_REF write FN_ITEM_REF;
  end;

  { TRegistro1400 }

  TRegistro1400 = class(TBlocos)
  private
    FQTD_TOT_SAIDA: double;
    FRegistro1410List: TRegistro1410List;
    FRegistro1420List: TRegistro1420List;
    FVL_TOT_ICMS_EFETIVO: double;
    FVL_CONFRONTO_ICMS_ENTRADA: double;
    FAPUR_ICMSST_RECUPERAR_RESSARCIR: double;

  public
    constructor Create(const ARegistro1000: TRegistro1000);
    destructor Destroy; override;

    property QTD_TOT_SAIDA: double read FQTD_TOT_SAIDA write FQTD_TOT_SAIDA;
    property VL_TOT_ICMS_EFETIVO: double read FVL_TOT_ICMS_EFETIVO write FVL_TOT_ICMS_EFETIVO;
    property VL_CONFRONTO_ICMS_ENTRADA: double read FVL_CONFRONTO_ICMS_ENTRADA write FVL_CONFRONTO_ICMS_ENTRADA;
    property APUR_ICMSST_RECUPERAR_RESSARCIR: double read FAPUR_ICMSST_RECUPERAR_RESSARCIR write FAPUR_ICMSST_RECUPERAR_RESSARCIR;

    property Registro1410List: TRegistro1410List read FRegistro1410List write FRegistro1410List;
    property Registro1420List: TRegistro1420List read FRegistro1420List write FRegistro1420List;
  end;

  { TRegistro1410List }

  TRegistro1410List = class(TObjectList)
  private
    FRegistro1000: TRegistro1000;
    function GetItem(Index: integer): TRegistro1410;
    procedure SetItem(Index: integer; const Value: TRegistro1410);
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    function New(): TRegistro1410;
    property Items[Index: integer]: TRegistro1410 read GetItem write SetItem;
  end;

  TRegistro1410 = class(TBlocos)
  private
    FDT_DOC: TDateTime;
    FCST_CSOSN: string;
    FCHAVE: string;
    FN_NF: integer;
    FCNPJ_EMIT: string;
    FUF_EMIT: string;
    FCNPJ_CPF_DEST: string;
    FUF_DEST: string;
    FCFOP: integer;
    FN_ITEM: integer;
    FUNID_ITEM: string;
    FQTD_SAIDA: double;
    FVL_UNIT_ITEM: double;
    FVL_ICMS_EFETIVO: double;
  public
    constructor Create(const ARegistro: TRegistro1000);
    //destructor Destroy; override;

    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CST_CSOSN: string read FCST_CSOSN write FCST_CSOSN;
    property CHAVE: string read FCHAVE write FCHAVE;
    property N_NF: integer read FN_NF write FN_NF;
    property CNPJ_EMIT: string read FCNPJ_EMIT write FCNPJ_EMIT;
    property UF_EMIT: string read FUF_EMIT write FUF_EMIT;
    property CNPJ_CPF_DEST: string read FCNPJ_CPF_DEST write FCNPJ_CPF_DEST;
    property UF_DEST: string read FUF_DEST write FUF_DEST;
    property CFOP: integer read FCFOP write FCFOP;
    property N_ITEM: integer read FN_ITEM write FN_ITEM;
    property UNID_ITEM: string read FUNID_ITEM write FUNID_ITEM;
    property QTD_SAIDA: double read FQTD_SAIDA write FQTD_SAIDA;
    property VL_UNIT_ITEM: double read FVL_UNIT_ITEM write FVL_UNIT_ITEM;
    property VL_ICMS_EFETIVO: double read FVL_ICMS_EFETIVO write FVL_ICMS_EFETIVO;
  end;

  { TRegistro1420List }

  TRegistro1420List = class(TObjectList)
  private
    FRegistro1000: TRegistro1000;
    function GetItem(Index: integer): TRegistro1420;
    procedure SetItem(Index: integer; const Value: TRegistro1420);
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    function New(): TRegistro1420;
    property Items[Index: integer]: TRegistro1420 read GetItem write SetItem;
  end;

  TRegistro1420 = class(TBlocos)
  private
    FDT_DOC: TDateTime;
    FCST_CSOSN: string;
    FCHAVE: string;
    FN_NF: integer;
    FCNPJ_EMIT: string;
    FUF_EMIT: string;
    FCNPJ_CPF_DEST: string;
    FUF_DEST: string;
    FCFOP: integer;
    FN_ITEM: integer;
    FUNID_ITEM: string;
    FQTD_DEVOLVIDA: double;
    FVL_UNIT_ITEM: double;
    FVL_ICMS_EFETIVO: double;
    FCHAVE_REF: string;
    FN_ITEM_REF: integer;
  public
    constructor Create(const ARegistro: TRegistro1000);
    //destructor Destroy; override;

    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CST_CSOSN: string read FCST_CSOSN write FCST_CSOSN;
    property CHAVE: string read FCHAVE write FCHAVE;
    property N_NF: integer read FN_NF write FN_NF;
    property CNPJ_EMIT: string read FCNPJ_EMIT write FCNPJ_EMIT;
    property UF_EMIT: string read FUF_EMIT write FUF_EMIT;
    property CNPJ_CPF_DEST: string read FCNPJ_CPF_DEST write FCNPJ_CPF_DEST;
    property UF_DEST: string read FUF_DEST write FUF_DEST;
    property CFOP: integer read FCFOP write FCFOP;
    property N_ITEM: integer read FN_ITEM write FN_ITEM;
    property UNID_ITEM: string read FUNID_ITEM;
    property QTD_DEVOLVIDA: double read FQTD_DEVOLVIDA write FQTD_DEVOLVIDA;
    property VL_UNIT_ITEM: double read FVL_UNIT_ITEM write FVL_UNIT_ITEM;
    property VL_ICMS_EFETIVO: double read FVL_ICMS_EFETIVO write FVL_ICMS_EFETIVO;
    property CHAVE_REF: string read FCHAVE_REF write FCHAVE_REF;
    property N_ITEM_REF: integer read FN_ITEM_REF write FN_ITEM_REF;
  end;


  { TRegistro1500 }

  TRegistro1500 = class(TBlocos)
  private
    FMVA_ICMSST: double;
    FQTD_TOT_SAIDA: double;
    FVL_ICMSST_UNIT_ENTR: double;
    FAPUR_ICMSST_RECUPERAR_RESSARCIR: double;

    FRegistro1510List: TRegistro1510List;
    FRegistro1520List: TRegistro1520List;
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    destructor Destroy; override;

    property QTD_TOT_SAIDA: double read FQTD_TOT_SAIDA write FQTD_TOT_SAIDA;
    property VL_ICMSST_UNIT_ENTR: double read FVL_ICMSST_UNIT_ENTR write FVL_ICMSST_UNIT_ENTR;
    property APUR_ICMSST_RECUPERAR_RESSARCIR: double read FAPUR_ICMSST_RECUPERAR_RESSARCIR write FAPUR_ICMSST_RECUPERAR_RESSARCIR;
    property MVA_ICMSST: double read FMVA_ICMSST write FMVA_ICMSST;

    property Registro1510List: TRegistro1510List read FRegistro1510List write FRegistro1510List;
    property Registro1520List: TRegistro1520List read FRegistro1520List write FRegistro1520List;
  end;

  { TRegistro1510List }

  TRegistro1510List = class(TObjectList)
  private
    FRegistro1000: TRegistro1000;
    function GetItem(Index: integer): TRegistro1510;
    procedure SetItem(Index: integer; const Value: TRegistro1510);
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    function New(): TRegistro1510;
    property Items[Index: integer]: TRegistro1510 read GetItem write SetItem;
  end;

  TRegistro1510 = class(TBlocos)
  private
    FDT_DOC: TDateTime;
    FCST_CSOSN: string;
    FCHAVE: string;
    FN_NF: integer;
    FCNPJ_EMIT: string;
    FUF_EMIT: string;
    FCNPJ_CPF_DEST: string;
    FUF_DEST: string;
    FCFOP: integer;
    FN_ITEM: integer;
    FUNID_ITEM: string;
    FQTD_SAIDA: double;
    FVL_UNIT_ITEM: double;
  public
    constructor Create(const ARegistro: TRegistro1000);
    //destructor Destroy; override;

    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CST_CSOSN: string read FCST_CSOSN write FCST_CSOSN;
    property CHAVE: string read FCHAVE write FCHAVE;
    property N_NF: integer read FN_NF write FN_NF;
    property CNPJ_EMIT: string read FCNPJ_EMIT write FCNPJ_EMIT;
    property UF_EMIT: string read FUF_EMIT write FUF_EMIT;
    property CNPJ_CPF_DEST: string read FCNPJ_CPF_DEST write FCNPJ_CPF_DEST;
    property UF_DEST: string read FUF_DEST write FUF_DEST;
    property CFOP: integer read FCFOP write FCFOP;
    property N_ITEM: integer read FN_ITEM write FN_ITEM;
    property UNID_ITEM: string read FUNID_ITEM;
    property QTD_SAIDA: double read FQTD_SAIDA write FQTD_SAIDA;
    property VL_UNIT_ITEM: double read FVL_UNIT_ITEM write FVL_UNIT_ITEM;
  end;

  { TRegistro1520List }

  TRegistro1520List = class(TObjectList)
  private
    FRegistro1000: TRegistro1000;
    function GetItem(Index: integer): TRegistro1520;
    procedure SetItem(Index: integer; const Value: TRegistro1520);
  public
    constructor Create(const ARegistro1000: TRegistro1000);
    function New(): TRegistro1520;
    property Items[Index: integer]: TRegistro1520 read GetItem write SetItem;
  end;

  TRegistro1520 = class(TBlocos)
  private
    FDT_DOC: TDateTime;
    FCST_CSOSN: string;
    FCHAVE: string;
    FN_NF: integer;
    FCNPJ_EMIT: string;
    FUF_EMIT: string;
    FCNPJ_CPF_DEST: string;
    FUF_DEST: string;
    FCFOP: integer;
    FN_ITEM: integer;
    FUNID_ITEM: string;
    FQTD_DEVOLVIDA: double;
    FVL_UNIT_ITEM: double;
    FCHAVE_REF: string;
    FN_ITEM_REF: integer;
  public
    constructor Create(const ARegistro: TRegistro1000);
    //destructor Destroy; override;

    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CST_CSOSN: string read FCST_CSOSN write FCST_CSOSN;
    property CHAVE: string read FCHAVE write FCHAVE;
    property N_NF: integer read FN_NF write FN_NF;
    property CNPJ_EMIT: string read FCNPJ_EMIT write FCNPJ_EMIT;
    property UF_EMIT: string read FUF_EMIT write FUF_EMIT;
    property CNPJ_CPF_DEST: string read FCNPJ_CPF_DEST write FCNPJ_CPF_DEST;
    property UF_DEST: string read FUF_DEST write FUF_DEST;
    property CFOP: integer read FCFOP write FCFOP;
    property N_ITEM: integer read FN_ITEM write FN_ITEM;
    property UNID_ITEM: string read FUNID_ITEM;
    property QTD_DEVOLVIDA: double read FQTD_DEVOLVIDA write FQTD_DEVOLVIDA;
    property VL_UNIT_ITEM: double read FVL_UNIT_ITEM write FVL_UNIT_ITEM;
    property CHAVE_REF: string read FCHAVE_REF write FCHAVE_REF;
    property N_ITEM_REF: integer read FN_ITEM_REF write FN_ITEM_REF;
  end;

  TRegistro1999 = class(TCloseBlocos)
  public
  end;


implementation

{ TRegistro1500 }

constructor TRegistro1500.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1510List := TRegistro1510List.Create(ARegistro1000);
  FRegistro1520List := TRegistro1520List.Create(ARegistro1000);
  ARegistro1000.Registro1999.Incrementa;

end;

destructor TRegistro1500.Destroy;
begin
  FRegistro1510List.Destroy;
  FRegistro1520List.Destroy;
  inherited Destroy;
end;

{ TRegistro1520 }

constructor TRegistro1520.Create(const ARegistro: TRegistro1000);
begin
  inherited Create;
  FUNID_ITEM := ARegistro.UNID_ITEM;
  ARegistro.Registro1999.Incrementa;

end;

{ TRegistro1510 }

constructor TRegistro1510.Create(const ARegistro: TRegistro1000);
begin
  inherited Create;
  FUNID_ITEM := ARegistro.UNID_ITEM;
  ARegistro.Registro1999.Incrementa;

end;

{ TRegistro1420 }

constructor TRegistro1420.Create(const ARegistro: TRegistro1000);
begin
  inherited Create;
  FUNID_ITEM := ARegistro.UNID_ITEM;
  ARegistro.Registro1999.Incrementa;

end;

{ TRegistro1410 }

constructor TRegistro1410.Create(const ARegistro: TRegistro1000);
begin
  inherited Create;
  FUNID_ITEM := ARegistro.UNID_ITEM;
  ARegistro.Registro1999.Incrementa;

end;

{ TRegistro1320 }

constructor TRegistro1320.Create(const ARegistro: TRegistro1000);
begin
  inherited Create;
  FUNID_ITEM := ARegistro.UNID_ITEM;
  ARegistro.Registro1999.Incrementa;

end;

{ TRegistro1310 }

constructor TRegistro1310.Create(const ARegistro: TRegistro1000);
begin
  inherited Create;
  FUNID_ITEM := ARegistro.UNID_ITEM;
  ARegistro.Registro1999.Incrementa;

end;

{ TRegistro1400 }

constructor TRegistro1400.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1410List := TRegistro1410List.Create(ARegistro1000);
  FRegistro1420List := TRegistro1420List.Create(ARegistro1000);
  ARegistro1000.Registro1999.Incrementa;

end;

destructor TRegistro1400.Destroy;
begin
  FRegistro1410List.Destroy;
  FRegistro1420List.Destroy;
  inherited Destroy;
end;

{ TRegistro1300 }

constructor TRegistro1300.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1310List := TRegistro1310List.Create(ARegistro1000);
  FRegistro1320List := TRegistro1320List.Create(ARegistro1000);

  ARegistro1000.Registro1999.Incrementa;

end;

destructor TRegistro1300.Destroy;
begin
  FRegistro1310List.Destroy;
  FRegistro1320List.Destroy;
  inherited Destroy;
end;

{ TRegistro1420List }

function TRegistro1420List.GetItem(Index: integer): TRegistro1420;
begin
  Result := TRegistro1420(inherited Items[Index]);
end;

procedure TRegistro1420List.SetItem(Index: integer; const Value: TRegistro1420);
begin
  Put(Index, Value);
end;

constructor TRegistro1420List.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1000 := ARegistro1000;
end;

function TRegistro1420List.New(): TRegistro1420;
begin
  Result := TRegistro1420.Create(FRegistro1000);
  Add(Result);
end;

{ TRegistro1410List }

function TRegistro1410List.GetItem(Index: integer): TRegistro1410;
begin
  Result := TRegistro1410(inherited Items[Index]);
end;

procedure TRegistro1410List.SetItem(Index: integer; const Value: TRegistro1410);
begin
  Put(Index, Value);
end;

constructor TRegistro1410List.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1000 := ARegistro1000;
end;

function TRegistro1410List.New(): TRegistro1410;
begin
  Result := TRegistro1410.Create(FRegistro1000);
  Add(Result);
end;

{ TRegistro1320List }

function TRegistro1320List.GetItem(Index: integer): TRegistro1320;
begin
  Result := TRegistro1320(inherited Items[Index]);
end;

procedure TRegistro1320List.SetItem(Index: integer; const Value: TRegistro1320);
begin
  Put(Index, Value);
end;

constructor TRegistro1320List.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1000 := ARegistro1000;
end;

function TRegistro1320List.New(): TRegistro1320;
begin
  Result := TRegistro1320.Create(FRegistro1000);
  Add(Result);
end;

{ TRegistro1310List }

function TRegistro1310List.GetItem(Index: integer): TRegistro1310;
begin
  Result := TRegistro1310(inherited Items[Index]);
end;

procedure TRegistro1310List.SetItem(Index: integer; const Value: TRegistro1310);
begin
  Put(Index, Value);
end;

constructor TRegistro1310List.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1000 := ARegistro1000;
end;

function TRegistro1310List.New(): TRegistro1310;
begin
  Result := TRegistro1310.Create(FRegistro1000);
  Add(Result);
end;

{ TRegistro1220 }

constructor TRegistro1220.Create(const ARegistro: TRegistro1000);
begin
  inherited Create;
  FUNID_ITEM := ARegistro.UNID_ITEM;
  ARegistro.Registro1999.Incrementa;

end;

{ TRegistro1210 }

constructor TRegistro1210.Create(const ARegistro: TRegistro1000);
begin
  inherited Create;
  FUNID_ITEM := ARegistro.UNID_ITEM;
  ARegistro.Registro1999.Incrementa;

end;

{ TRegistro1200 }

constructor TRegistro1200.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1210List := TRegistro1210List.Create(ARegistro1000);
  FRegistro1220List := TRegistro1220List.Create(ARegistro1000);
  ARegistro1000.Registro1999.Incrementa;

end;

destructor TRegistro1200.Destroy;
begin
  FRegistro1210List.Destroy;
  FRegistro1220List.Destroy;
  inherited Destroy;
end;

{ TRegistro1210List }

function TRegistro1210List.GetItem(Index: integer): TRegistro1210;
begin
  Result := TRegistro1210(inherited Items[Index]);
end;

procedure TRegistro1210List.SetItem(Index: integer; const Value: TRegistro1210);
begin
  Put(Index, Value);
end;

constructor TRegistro1210List.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1000 := ARegistro1000;
end;

function TRegistro1210List.New(): TRegistro1210;
begin
  Result := TRegistro1210.Create(FRegistro1000);
  Add(Result);
end;

{ TRegistro1220List }

function TRegistro1220List.GetItem(Index: integer): TRegistro1220;
begin
  Result := TRegistro1220(inherited Items[Index]);
end;

procedure TRegistro1220List.SetItem(Index: integer; const Value: TRegistro1220);
begin
  Put(Index, Value);
end;

constructor TRegistro1220List.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1000 := ARegistro1000;
end;

function TRegistro1220List.New(): TRegistro1220;
begin
  Result := TRegistro1220.Create(FRegistro1000);
  Add(Result);
end;

{ TRegistro1120 }

constructor TRegistro1120.Create(const ARegistro: TRegistro1000);
begin
  inherited Create;
  FUNID_ITEM := ARegistro.UNID_ITEM;
  ARegistro.Registro1999.Incrementa;

end;

{ TRegistro1110 }

constructor TRegistro1110.Create(const ARegistro: TRegistro1000);
begin
  inherited Create;
  FUNID_ITEM := ARegistro.UNID_ITEM;
  ARegistro.Registro1999.Incrementa;

end;

{ TRegistro1100 }

constructor TRegistro1100.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1110List := TRegistro1110List.Create(ARegistro1000);
  FRegistro1120List := TRegistro1120List.Create(ARegistro1000);
  ARegistro1000.Registro1999.Incrementa;

end;

destructor TRegistro1100.Destroy;
begin
  FRegistro1110List.Destroy;
  FRegistro1120List.Destroy;
  inherited Destroy;
end;

{ TRegistro1110List }

function TRegistro1110List.GetItem(Index: integer): TRegistro1110;
begin
  Result := TRegistro1110(inherited Items[Index]);
end;

procedure TRegistro1110List.SetItem(Index: integer; const Value: TRegistro1110);
begin
  Put(Index, Value);
end;

constructor TRegistro1110List.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1000 := ARegistro1000;
end;

function TRegistro1110List.New(): TRegistro1110;
begin
  Result := TRegistro1110.Create(FRegistro1000);
  Add(Result);
end;

{ TRegistro1120List }

function TRegistro1120List.GetItem(Index: integer): TRegistro1120;
begin
  Result := TRegistro1120(inherited Items[Index]);
end;

procedure TRegistro1120List.SetItem(Index: integer; const Value: TRegistro1120);
begin
  Put(Index, Value);
end;

constructor TRegistro1120List.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1000 := ARegistro1000;
end;

function TRegistro1120List.New(): TRegistro1120;
begin
  Result := TRegistro1120.Create(FRegistro1000);
  Add(Result);
end;

{ TRegistro1520List }

function TRegistro1520List.GetItem(Index: integer): TRegistro1520;
begin
  Result := TRegistro1520(inherited Items[Index]);
end;

procedure TRegistro1520List.SetItem(Index: integer; const Value: TRegistro1520);
begin
  Put(Index, Value);
end;

constructor TRegistro1520List.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1000 := ARegistro1000;
end;

function TRegistro1520List.New(): TRegistro1520;
begin
  Result := TRegistro1520.Create(FRegistro1000);
  Add(Result);
end;

{ TRegistro1510List }

function TRegistro1510List.GetItem(Index: integer): TRegistro1510;
begin
  Result := TRegistro1510(inherited Items[Index]);
end;

procedure TRegistro1510List.SetItem(Index: integer; const Value: TRegistro1510);
begin
  Put(Index, Value);
end;

constructor TRegistro1510List.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FRegistro1000 := ARegistro1000;
end;


function TRegistro1510List.New(): TRegistro1510;
begin
  Result := TRegistro1510.Create(FRegistro1000);
  Add(Result);
end;

{ TRegistro1010 }

constructor TRegistro1010.Create(const ARegistro1000: TRegistro1000);
begin
  inherited Create;
  FCOD_ITEM := ARegistro1000.COD_ITEM;
  FUNID_ITEM := ARegistro1000.UNID_ITEM;
  ARegistro1000.Registro1999.Incrementa;
end;

{ TRegistro1000 }

function TRegistro1000.GetRegistro1010: TRegistro1010;
begin
  if Assigned(FRegistro1010) then
    Result := FRegistro1010
  else
    Result := nil;
end;

function TRegistro1000.GetRegistro1200: TRegistro1200;
begin
  if Assigned(FRegistro1200) then
    Result := FRegistro1200
  else
    Result := nil;
end;

function TRegistro1000.GetRegistro1300: TRegistro1300;
begin
  if Assigned(FRegistro1300) then
    Result := FRegistro1300
  else
    Result := nil;
end;

function TRegistro1000.GetRegistro1400: TRegistro1400;
begin
  if Assigned(FRegistro1400) then
    Result := FRegistro1400
  else
    Result := nil;
end;

function TRegistro1000.GetRegistro1500: TRegistro1500;
begin
  if Assigned(FRegistro1500) then
    Result := FRegistro1500
  else
    Result := nil;
end;

constructor TRegistro1000.Create( ARegistro1999: TRegistro1999);
begin
  inherited Create;
  FRegistro1999 := ARegistro1999;
  FRegistro1999.Incrementa;

  FRegistro1010 := nil;
  FRegistro1100 := TRegistro1100.Create(Self);


  FRegistro1200 := nil;
  FRegistro1300 := nil;
  FRegistro1400 := nil;
  FRegistro1500 := nil;
end;

destructor TRegistro1000.Destroy;
begin
  FRegistro1100.Destroy;
  if Assigned(FRegistro1010) then
    FRegistro1010.Free;
  inherited Destroy;
end;

function TRegistro1000.Registro1010New: TRegistro1010;
begin
  if not Assigned(FRegistro1010) then
    FRegistro1010 := TRegistro1010.Create(Self);
  Result := FRegistro1010;
end;

function TRegistro1000.Registro1200New: TRegistro1200;
begin
  if not Assigned(FRegistro1200) then
    FRegistro1200 := TRegistro1200.Create(Self);
  Result := FRegistro1200;
end;

function TRegistro1000.Registro1300New: TRegistro1300;
begin
  if not Assigned(FRegistro1300) then
    FRegistro1300 := TRegistro1300.Create(Self);
  Result := FRegistro1300;
end;

function TRegistro1000.Registro1400New: TRegistro1400;
begin
  if not Assigned(FRegistro1400) then
    FRegistro1400 := TRegistro1400.Create(Self);
  Result := FRegistro1400;
end;

function TRegistro1000.Registro1500New: TRegistro1500;
begin
  if not Assigned(FRegistro1500) then
    FRegistro1500 := TRegistro1500.Create(Self);
  Result := FRegistro1500;
end;

{TRegistro1000List}

function TRegistro1000List.GetItem(Index: integer): TRegistro1000;
begin
  Result := TRegistro1000(inherited Items[Index]);
end;

function TRegistro1000List.New(): TRegistro1000;
begin
  Result := TRegistro1000.Create(FRegistro1999);
  Add(Result);
end;

procedure TRegistro1000List.SetItem(Index: integer; const Value: TRegistro1000);
begin
  Put(Index, Value);
end;

procedure TRegistro1000List.Clear;
begin
  inherited;
  FRegistro1999.Destroy;
  FRegistro1999 := TRegistro1999.Create;
end;

constructor TRegistro1000List.Create;
begin
  inherited Create;
  FRegistro1999 := TRegistro1999.Create;
end;

destructor TRegistro1000List.Destroy;
begin
  inherited Destroy;
  FRegistro1999.Destroy;
end;

{ TRegistro1001 }

constructor TRegistro1001.Create( ARegistro1999: TRegistro1999);
begin
  inherited Create;
  FRegistro1999 := ARegistro1999;
  FRegistro1999.Incrementa;

  FRegistro1101 := TRegistro1101.Create(Self);
end;

destructor TRegistro1001.Destroy;
begin
  FRegistro1101.Destroy;
  inherited;
end;

procedure TRegistro1001List.Clear;
begin
  inherited;
  FRegistro1999.Destroy;
  FRegistro1999 := TRegistro1999.Create;
end;

{ TRegistro1001List }

constructor TRegistro1001List.Create;
begin
  inherited Create;
  FRegistro1999 := TRegistro1999.Create;
end;

destructor TRegistro1001List.Destroy;
begin
  inherited Destroy;
  FRegistro1999.Destroy;
end;

function TRegistro1001List.GetItem(Index: integer): TRegistro1001;
begin
  Result := TRegistro1001(inherited Items[Index]);
end;

function TRegistro1001List.New: TRegistro1001;
begin
  Add(Result);
end;

procedure TRegistro1001List.SetItem(Index: integer; const Value: TRegistro1001);
begin
  Put(Index, Value);
end;

{ TRegistro1101 }

constructor TRegistro1101.Create(const ARegistro1001: TRegistro1001);
begin
  inherited Create;
  FRegistro1111List := TRegistro1111List.Create(ARegistro1001);
  ARegistro1001.Registro1999.Incrementa;
end;

destructor TRegistro1101.Destroy;
begin
  FRegistro1111List.Destroy;
  inherited;
end;

{ TRegistro1111 }

constructor TRegistro1111.Create(const ARegistro: TRegistro1001);
begin
  inherited Create;
  FUNID_ITEM := ARegistro.UNID_ITEM;
  ARegistro.Registro1999.Incrementa;
end;

{ TRegistro1111List }

constructor TRegistro1111List.Create(const ARegistro1001: TRegistro1001);
begin
  inherited Create;
  FRegistro1001 := ARegistro1001;
end;

function TRegistro1111List.GetItem(Index: integer): TRegistro1111;
begin
  Result := TRegistro1111(inherited Items[Index]);
end;

function TRegistro1111List.New: TRegistro1111;
begin
  Result := TRegistro1111.Create(FRegistro1001);
  Add(Result);
end;

procedure TRegistro1111List.SetItem(Index: integer; const Value: TRegistro1111);
begin
  Put(Index, Value);
end;

end.
