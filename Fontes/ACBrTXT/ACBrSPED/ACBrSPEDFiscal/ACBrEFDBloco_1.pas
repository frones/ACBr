{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

{******************************************************************************
|* Historico
|*
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
|* 16/01/2012: Cilleni Caeano
|*  - Criado o registro 1010
*******************************************************************************}

unit ACBrEFDBloco_1;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEFDBlocos;

type
  TRegistro1010List = class;
  TRegistro1100List = class;
  TRegistro1105List = class;
  TRegistro1110List = class;
  TRegistro1200List = class;
  TRegistro1210List = class;
  TRegistro1250List = class;
  TRegistro1255List = class;
  TRegistro1300List = class;
  TRegistro1310List = class;
  TRegistro1320List = class;
  TRegistro1350List = class;
  TRegistro1360List = class;
  TRegistro1370List = class;
  TRegistro1390List = class;
  TRegistro1391List = class;  
  TRegistro1400List = class;
  TRegistro1500List = class;
  TRegistro1510List = class;
  TRegistro1600List = class;
  TRegistro1601List = class;
  TRegistro1700List = class;
  TRegistro1710List = class;
  TRegistro1800List = class;
  TRegistro1900List = class;
  TRegistro1910List = class;
  TRegistro1920List = class;
  TRegistro1921List = class;
  TRegistro1922List = class;
  TRegistro1923List = class;
  TRegistro1925List = class;
  TRegistro1926List = class;
  TRegistro1960List = class;
  TRegistro1970List = class;
  TRegistro1975List = class;
  TRegistro1980List = class;

  /// Registro 1001 - ABERTURA DO BLOCO 1

  TRegistro1001 = class(TOpenBlocos)
  private
    FRegistro1010: TRegistro1010List;
    FRegistro1100: TRegistro1100List;
    FRegistro1200: TRegistro1200List;
    FRegistro1250: TRegistro1250List;
    FRegistro1300: TRegistro1300List;
    FRegistro1350: TRegistro1350List;
    FRegistro1390: TRegistro1390List;
    FRegistro1391: TRegistro1391List;
    FRegistro1400: TRegistro1400List;
    FRegistro1500: TRegistro1500List;
    FRegistro1600: TRegistro1600List;
    FRegistro1601: TRegistro1601List;
    FRegistro1700: TRegistro1700List;
    FRegistro1800: TRegistro1800List;
    FRegistro1900: TRegistro1900List;
    FRegistro1960: TRegistro1960List;
    FRegistro1970: TRegistro1970List;
    FRegistro1980: TRegistro1980List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property Registro1010: TRegistro1010List read FRegistro1010 write FRegistro1010;
    property Registro1100: TRegistro1100List read FRegistro1100 write FRegistro1100;
    property Registro1200: TRegistro1200List read FRegistro1200 write FRegistro1200;
    property Registro1250: TRegistro1250List read FRegistro1250 write FRegistro1250;
    property Registro1300: TRegistro1300List read FRegistro1300 write FRegistro1300;
    property Registro1350: TRegistro1350List read FRegistro1350 write FRegistro1350;
    property Registro1390: TRegistro1390List read FRegistro1390 write FRegistro1390;
    property Registro1391: TRegistro1391List read FRegistro1391 write FRegistro1391;
    property Registro1400: TRegistro1400List read FRegistro1400 write FRegistro1400;
    property Registro1500: TRegistro1500List read FRegistro1500 write FRegistro1500;
    property Registro1600: TRegistro1600List read FRegistro1600 write FRegistro1600;
    property Registro1601: TRegistro1601List read FRegistro1601 write FRegistro1601;
    property Registro1700: TRegistro1700List read FRegistro1700 write FRegistro1700;
    property Registro1800: TRegistro1800List read FRegistro1800 write FRegistro1800;
    property Registro1900: TRegistro1900List read FRegistro1900 write FRegistro1900;
    property Registro1960: TRegistro1960List read FRegistro1960 write FRegistro1960;
    property Registro1970: TRegistro1970List read FRegistro1970 write FRegistro1970;
    property Registro1980: TRegistro1980List read FRegistro1980 write FRegistro1980;
  end;

  /// Registro 1010 - Obrigatoriedade de registros do Bloco 1

  TRegistro1010 = class
  private
    fIND_EXP   : String;// Reg. 1100 - Ocorreu averbação (conclusão) de exportação no período:
    fIND_CCRF  : String;// Reg. 1200 – Existem informações acerca de créditos de ICMS a serem controlados, definidos pela Sefaz:
    fIND_COMB  : String;// Reg. 1300 – É comercio varejista de combustíveis:
    fIND_USINA : String;// Reg. 1390 – Usinas de açúcar e/álcool – O estabelecimento é produtor de açúcar e/ou álcool carburante:
    fIND_VA    : String;// Reg. 1400 – Existem informações a serem prestadas neste registro e o registro é obrigatório em sua Unidade da Federação:
    fIND_EE    : String;// Reg. 1500 - A empresa é distribuidora de energia e ocorreu fornecimento de energia elétrica para consumidores de outra UF:
    fIND_CART  : String;// Reg. 1600 ou 1601 - Realizou vendas com Cartão de Crédito ou de débito / instrumentos eletrônicos de pagamento
    fIND_FORM  : String;// Reg. 1700 - É obrigatório em sua unidade da federação o controle de utilização de documentos  fiscais em papel:
    fIND_AER   : String;// Reg. 1800 – A empresa prestou serviços de transporte aéreo de cargas e de passageiros:
    fIND_GIAF1 : String;// Reg. 1960 – Possui informações GIAF1:
    fIND_GIAF3 : String;// Reg. 1970 – Possui informações GIAF3:
    fIND_GIAF4 : String;// Reg. 1980 – Possui informações GIAF4:
    FIND_REST_RESSARC_COMPL_ICMS: string; //Reg. 1250 – Possui informações consolidadas de saldos de restituição, ressarcimento e complementação do ICMS?
  public
    constructor Create(AOwner: TRegistro1001); virtual; /// Create

    property IND_EXP   : String read fIND_EXP   write fIND_EXP   ;
    property IND_CCRF  : String read fIND_CCRF  write fIND_CCRF  ;
    property IND_COMB  : String read fIND_COMB  write fIND_COMB  ;
    property IND_USINA : String read fIND_USINA write fIND_USINA ;
    property IND_VA    : String read fIND_VA    write fIND_VA    ;
    property IND_EE    : String read fIND_EE    write fIND_EE    ;
    property IND_CART  : String read fIND_CART  write fIND_CART  ;
    property IND_FORM  : String read fIND_FORM  write fIND_FORM  ;
    property IND_AER   : String read fIND_AER   write fIND_AER   ;
    property IND_GIAF1 : String read fIND_GIAF1 write fIND_GIAF1 ;
    property IND_GIAF3 : String read fIND_GIAF3 write fIND_GIAF3 ;
    property IND_GIAF4 : String read fIND_GIAF4 write fIND_GIAF4 ;
    property IND_REST_RESSARC_COMPL_ICMS: string read FIND_REST_RESSARC_COMPL_ICMS write FIND_REST_RESSARC_COMPL_ICMS;
  end;

  /// Registro 1010 - Lista

  TRegistro1010List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1010;
    procedure SetItem(Index: Integer; const Value: TRegistro1010);
  public
    function New(AOwner: TRegistro1001): TRegistro1010;
    property Items[Index: Integer]: TRegistro1010 read GetItem write SetItem;
  end;

  /// Registro 1100 - REGISTRO DE INFORMAÇÕES SOBRE EXPORTAÇÃO

  TRegistro1100 = class
  private
    fIND_DOC: TACBrTipoDocto;     /// Informe o tipo de documento: 0 - Declaração de Exportação, 1 - Declaração Simplificada de Exportação.
    fNRO_DE: String;              /// Número da declaração
    fDT_DE: TDateTime;            /// Data da declaração (DDMMAAAA)
    fNAT_EXP: TACBrExportacao;    /// Preencher com: 0 - Exportação Direta, 1 - Exportação Indireta
    fNRO_RE: String;              /// Nº do registro de Exportação
    fDT_RE: TDateTime;            /// Data do Registro de Exportação (DDMMAAAA)
    fCHC_EMB: String;             /// Nº do conhecimento de embarque
    fDT_CHC: TDateTime;           /// Data do conhecimento de embarque (DDMMAAAA)
    fDT_AVB: TDateTime;           /// Data da averbação da Declaração de exportação (ddmmaaaa)
    fTP_CHC: TACBrConhecEmbarque; /// Informação do tipo de conhecimento de transporte : 01 - AWB; 02 - MAWB; 03 - HAWB;04 - COMAT; 06 - R. EXPRESSAS; 07 - ETIQ. REXPRESSAS; 08 - HR. EXPRESSAS; 09 - AV7; 10 - BL; 11 - MBL; 12 - HBL; 13 - CRT; 14 - DSIC; 16 - COMAT BL; 17 - RWB; 18 - HRWB; 19 - TIF/DTA; 20 - CP2; 91 - NÂO IATA; 92 - MNAO IATA; 93 - HNAO IATA; 99 - OUTROS.
    fPAIS: String;                /// Código do país de destino da mercadoria (Preencher conforme tabela do SISCOMEX)

    fRegistro1105: TRegistro1105List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_DOC: TACBrTipoDocto read FIND_DOC write FIND_DOC;
    property NRO_DE: String read FNRO_DE write FNRO_DE;
    property DT_DE: TDateTime read FDT_DE write FDT_DE;
    property NAT_EXP: TACBrExportacao read FNAT_EXP write FNAT_EXP;
    property NRO_RE: String read FNRO_RE write FNRO_RE;
    property DT_RE: TDateTime read FDT_RE write FDT_RE;
    property CHC_EMB: String read FCHC_EMB write FCHC_EMB;
    property DT_CHC: TDateTime read FDT_CHC write FDT_CHC;
    property DT_AVB: TDateTime read FDT_AVB write FDT_AVB;
    property TP_CHC: TACBrConhecEmbarque read FTP_CHC write FTP_CHC;
    property PAIS: String read FPAIS write FPAIS;
    //
    property Registro1105: TRegistro1105List read FRegistro1105 write FRegistro1105;
  end;

  /// Registro 1100 - Lista

  TRegistro1100List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1100;
    procedure SetItem(Index: Integer; const Value: TRegistro1100);
  public
    function New(): TRegistro1100;
    property Items[Index: Integer]: TRegistro1100 read GetItem write SetItem;
  end;

  /// Registro 1105 - DOCUMENTOS FISCAIS DE EXPORTAÇÃO

  TRegistro1105 = class
  private
    fCOD_MOD: String;       /// Código do modelo da NF, conforme tabela 4.1.1
    fSERIE: String;         /// Série da Nota Fiscal
    fNUM_DOC: String;       /// Número de Nota Fiscal de Exportação emitida pelo Exportador
    fCHV_NFE: String;       /// Chave da Nota Fiscal Eletrônica
    fDT_DOC: TDateTime;        /// Data da emissão da NF de exportação
    fCOD_ITEM: String;      /// Código do item (campo 02 do Registro 0200)

    fRegistro1110: TRegistro1110List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property SERIE: String read FSERIE write FSERIE;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property CHV_NFE: String read FCHV_NFE write FCHV_NFE;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    //
    property Registro1110: TRegistro1110List read FRegistro1110 write FRegistro1110;
  end;

  /// Registro 1105 - Lista

  TRegistro1105List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1105;
    procedure SetItem(Index: Integer; const Value: TRegistro1105);
  public
    function New(): TRegistro1105;
    property Items[Index: Integer]: TRegistro1105 read GetItem write SetItem;
  end;

  /// Registro 1110 - OPERAÇÕES DE EXPORTAÇÃO INDIRETA DE PRODUTOS NÃO INDUSTRIALIZADOS PELO ESTABELECIMENTO EMITENTE.

  TRegistro1110 = class
  private
    fCOD_PART: String;      /// Código do participante-Fornecedor da Mercadoria destinada à exportação (campo 02 do Registro 0150)
    fCOD_MOD: String;       /// Código do documento fiscal, conforme a Tabela 4.1.1
    fSER: String;           /// Série do documento fiscal recebido com fins específicos de exportação.
    fNUM_DOC: String;       /// Número do documento fiscal recebido com fins específicos de exportação.
    fDT_DOC: TDateTime;        /// Data da emissão do documento fiscal recebido com fins específicos de exportação
    fCHV_NFE: String;       /// Chave da Nota Fiscal Eletrônica
    fNR_MEMO: String;           /// Número do Memorando de Exportação
    fQTD: Double;           /// Quantidade do item efetivamente exportado.
    fUNID: String;          /// Unidade do item (Campo 02 do registro 0190)
  public
    constructor Create(AOwner: TRegistro1105); virtual; /// Create

    property COD_PART: String read FCOD_PART write FCOD_PART;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property SER: String read FSER write FSER;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CHV_NFE: String read FCHV_NFE write FCHV_NFE;
    property NR_MEMO: String read FNR_MEMO write FNR_MEMO;
    property QTD: Double read FQTD write FQTD;
    property UNID: String read FUNID write FUNID;
  end;

  /// Registro 1110 - Lista

  TRegistro1110List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1110;
    procedure SetItem(Index: Integer; const Value: TRegistro1110);
  public
    function New(AOwner: TRegistro1105): TRegistro1110;
    property Items[Index: Integer]: TRegistro1110 read GetItem write SetItem;
  end;

  /// Registro 1200 - OPERAÇÕES DE EXPORTAÇÃO INDIRETA DE PRODUTOS NÃO INDUSTRIALIZADOS PELO ESTABELECIMENTO EMITENTE.

  TRegistro1200 = class
  private
    fCOD_AJ_APUR : String;      /// Código de ajuste, conforme informado na Tabela indicada no item 5.1.1.
    fSLD_CRED    : Currency;        /// Saldo de créditos fiscais de períodos anteriores
    fCRED_APR    : Currency;        /// Total de crédito apropriado no mês
    fCRED_RECEB  : Currency;        /// Total de créditos recebidos por transferência
    fCRED_UTIL   : Currency;        /// Total de créditos utilizados no período
    fSLD_CRED_FIM: Currency;        /// Saldo de crédito fiscal acumulado a transportar para o período seguinte

    FRegistro1210: TRegistro1210List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_AJ_APUR : String read FCOD_AJ_APUR  write FCOD_AJ_APUR;
    property SLD_CRED    : Currency   read FSLD_CRED     write FSLD_CRED;
    property CRED_APR    : Currency   read FCRED_APR     write FCRED_APR;
    property CRED_RECEB  : Currency   read FCRED_RECEB   write FCRED_RECEB;
    property CRED_UTIL   : Currency   read FCRED_UTIL    write FCRED_UTIL;
    property SLD_CRED_FIM: Currency   read FSLD_CRED_FIM write FSLD_CRED_FIM;
    //
    property Registro1210: TRegistro1210List read FRegistro1210 write FRegistro1210;
  end;

  /// Registro 1200 - Lista

  TRegistro1200List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1200;
    procedure SetItem(Index: Integer; const Value: TRegistro1200);
  public
    function New(): TRegistro1200;
    property Items[Index: Integer]: TRegistro1200 read GetItem write SetItem;
  end;

  /// Registro 1210 - UTILIZAÇÃO DE CRÉDITOS FISCAIS - ICMS.

  TRegistro1210 = class
  private
    fTIPO_UTIL: String;        /// Tipo de utilização do crédito:
    fNR_DOC: String;           /// Número do documento utilizado na baixa de créditos
    fVL_CRED_UTIL: Currency;     /// Total de crédito utilizado
    fCHV_DOCe: String;
  public
    constructor Create(AOwner: TRegistro1200); virtual; /// Create

    property TIPO_UTIL: String read FTIPO_UTIL write FTIPO_UTIL;
    property NR_DOC: String read FNR_DOC write FNR_DOC;
    property VL_CRED_UTIL: Currency read FVL_CRED_UTIL write FVL_CRED_UTIL;
    property CHV_DOCe: String read fCHV_DOCe write fCHV_DOCe;
  end;

  /// Registro 1210 - Lista

  TRegistro1210List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1210;
    procedure SetItem(Index: Integer; const Value: TRegistro1210);
  public
    function New(AOwner: TRegistro1200): TRegistro1210;
    property Items[Index: Integer]: TRegistro1210 read GetItem write SetItem;
  end;

   /// Registro 1250 - OPERAÇÕES DE EXPORTAÇÃO INDIRETA DE PRODUTOS NÃO INDUSTRIALIZADOS PELO ESTABELECIMENTO EMITENTE.

  TRegistro1250 = class
  private
   fVL_CREDITO_ICMS_OP : currency; /// Informar o valor total do ICMS operação própria que o informante tem direito ao crédito, na forma prevista na legislação, referente às hipóteses de restituição em que há previsão deste crédito.
   fVL_ICMS_ST_REST: currency; /// Informar o valor total do ICMS ST que o informante tem direito ao crédito, na forma prevista na legislação, referente às hipóteses de restituição em que há previsão deste crédito.
   fVL_FCP_ST_REST: currency; /// Informar o valor total do FCP_ST agregado ao valor do ICMS ST informado no campo “VL_ICMS_ST_REST”.
   fVL_ICMS_ST_COMPL: currency; /// Informar o valor total do débito referente ao complemento do imposto, nos casos previstos na legislação.
   fVL_FCP_ST_COMPL: currency; /// Informar o valor total do FCP_ST agregado ao valor informado no campo “VL_ICMS_ST_COMPL”.

   FRegistro1255: TRegistro1255List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property VL_CREDITO_ICMS_OP : Currency   read fVL_CREDITO_ICMS_OP     write fVL_CREDITO_ICMS_OP;
    property VL_ICMS_ST_REST : Currency   read fVL_ICMS_ST_REST     write fVL_ICMS_ST_REST;
    property VL_FCP_ST_REST  : Currency   read fVL_FCP_ST_REST   write fVL_FCP_ST_REST;
    property VL_ICMS_ST_COMPL : Currency   read fVL_ICMS_ST_COMPL    write fVL_ICMS_ST_COMPL;
    property VL_FCP_ST_COMPL: Currency   read fVL_FCP_ST_COMPL write fVL_FCP_ST_COMPL;
    property Registro1255: TRegistro1255List read FRegistro1255 write FRegistro1255;
  end;

  /// Registro 1250 - Lista

  TRegistro1250List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1250;
    procedure SetItem(Index: Integer; const Value: TRegistro1250);
  public
    function New(): TRegistro1250;
    property Items[Index: Integer]: TRegistro1250 read GetItem write SetItem;
  end;

  /// REGISTRO 1255: INFORMAÇÕES CONSOLIDADAS DE SALDOS DE RESTITUIÇÃO, RESSARCIMENTO E COMPLEMENTAÇÃO DO ICMS POR MOTIVO

  TRegistro1255 = class
  private
   fCOD_MOT_REST_COMPL: string; /// Código do motivo da restituição ou complementação conforme Tabela 5.7.
   fVL_CREDITO_ICMS_OP_MOT: currency; /// Informar o valor total do ICMS operação própria que o informante tem direito ao crédito, na forma prevista na legislação, referente às hipóteses de restituição em que há previsão deste crédito, para o mesmo “COD_MOT_REST_COMPL”.
   fVL_ICMS_ST_REST_MOT: currency; /// Informar o valor total do ICMS ST que o informante tem direito ao crédito, na forma prevista na legislação, referente às hipóteses de restituição em que há previsão deste crédito, para o mesmo “COD_MOT_REST_COMPL”.
   fVL_FCP_ST_REST_MOT: currency; /// Informar o valor total do FCP_ST agregado ao valor do ICMS ST informado no campo “VL_ICMS_ST_REST_MOT”.
   fVL_ICMS_ST_COMPL_MOT: currency; /// Informar o valor total do débito referente ao complemento do imposto, nos casos previstos na legislação, para o mesmo “COD_MOT_REST_COMPL”.
   fVL_FCP_ST_COMPL_MOT: currency; /// Informar o valor total do FCP_ST agregado ao valor informado no campo “VL_ICMS_ST_COMPL_MOT”.
  public

    constructor Create(AOwner: TRegistro1250); virtual; /// Create

    property COD_MOT_REST_COMPL : string   read fCOD_MOT_REST_COMPL     write fCOD_MOT_REST_COMPL;
    property VL_CREDITO_ICMS_OP_MOT : Currency   read fVL_CREDITO_ICMS_OP_MOT     write fVL_CREDITO_ICMS_OP_MOT;
    property VL_ICMS_ST_REST_MOT : Currency   read fVL_ICMS_ST_REST_MOT     write fVL_ICMS_ST_REST_MOT;
    property VL_FCP_ST_REST_MOT  : Currency   read fVL_FCP_ST_REST_MOT   write fVL_FCP_ST_REST_MOT;
    property VL_ICMS_ST_COMPL_MOT : Currency   read fVL_ICMS_ST_COMPL_MOT    write fVL_ICMS_ST_COMPL_MOT;
    property VL_FCP_ST_COMPL_MOT: Currency   read fVL_FCP_ST_COMPL_MOT write fVL_FCP_ST_COMPL_MOT;
  end;

  /// Registro 1255 - Lista

  TRegistro1255List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1255;
    procedure SetItem(Index: Integer; const Value: TRegistro1255);
  public
    function New(AOwner: TRegistro1250): TRegistro1255;
    property Items[Index: Integer]: TRegistro1255 read GetItem write SetItem;
  end;

  /// Registro 1300 - MOVIMENTAÇÃO DE COMBUSTÍVEIS.

  TRegistro1300 = class
  private
    fCOD_ITEM: String;             /// Código do Produto constante do registro 0200
    fDT_FECH: TDateTime;               /// Data do fechamento da movimentação
    fESTQ_ABERT: Double;               /// Estoque no inicio do dia
    fVOL_ENTR: Double;                 /// Volume Total das Entradas
    fVOL_DISP: Double;                 /// Volume Disponível (05 + 06)
    fVOL_SAIDAS: Double;               /// Volume Total das Saídas (Somatório dos registros de Volume de Vendas)
    fESTQ_ESCR: Double;                /// Estoque Escritural (07 - 08)
    fVAL_AJ_PERDA: Double;             /// Valor da Perda
    fVAL_AJ_GANHO: Double;             /// Valor do ganho
    fFECH_FISICO:  Double;             /// Volume aferido no tanque, em litros. Estoque de fechamento físico do tanque

    FRegistro1310: TRegistro1310List;  /// BLOCO 1 - Lista de Registro1310 (FILHO)
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_ITEM: String       read FCOD_ITEM     write FCOD_ITEM;
    property DT_FECH: TDateTime     read FDT_FECH      write FDT_FECH;
    property ESTQ_ABERT: Double     read FESTQ_ABERT   write FESTQ_ABERT;
    property VOL_ENTR: Double       read FVOL_ENTR     write FVOL_ENTR;
    property VOL_DISP: Double       read FVOL_DISP     write FVOL_DISP;
    property VOL_SAIDAS: Double     read FVOL_SAIDAS   write FVOL_SAIDAS;
    property ESTQ_ESCR: Double      read FESTQ_ESCR    write FESTQ_ESCR;
    property VAL_AJ_PERDA: Double   read FVAL_AJ_PERDA write FVAL_AJ_PERDA;
    property VAL_AJ_GANHO: Double   read FVAL_AJ_GANHO write FVAL_AJ_GANHO;
    property FECH_FISICO : Double   read fFECH_FISICO  write fFECH_FISICO;

    property Registro1310: TRegistro1310List read FRegistro1310 write FRegistro1310;
  end;

  /// Registro 1300 - Lista

  TRegistro1300List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1300;
    procedure SetItem(Index: Integer; const Value: TRegistro1300);
  public
    function New(): TRegistro1300;
    property Items[Index: Integer]: TRegistro1300 read GetItem write SetItem;
  end;

  /// Registro 1310 - VOLUME DE VENDAS.

  TRegistro1310 = class
  private
    fNUM_TANQUE:   String; /// Tanque onde foi armazenado o combustível
    fESTQ_ABERT:   Double;     /// Estoque no inicio do dia, em litros
    fVOL_ENTR:     Double;     /// Volume Recebido no dia (em litros)
    fVOL_DISP:     Double;     /// Volume Disponível (03 + 04), em litros
    fVOL_SAIDAS:   Double;     /// Volume Total das Saídas, em litros
    fESTQ_ESCR :   Double;     /// Estoque Escritural(05 – 06), litros
    fVAL_AJ_PERDA: Double;     /// Valor da Perda, em litros
    fVAL_AJ_GANHO: Double;     /// Valor do ganho, em litros
    fFECH_FISICO:  Double;     /// Volume aferido no tanque, em litros. Estoque de fechamento físico do tanque

    FRegistro1320: TRegistro1320List;  /// BLOCO 1 - Lista de Registro1320 (FILHO)
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property NUM_TANQUE  : String   read fNUM_TANQUE   write fNUM_TANQUE;
    property ESTQ_ABERT  : Double       read fESTQ_ABERT   write fESTQ_ABERT;
    property VOL_ENTR    : Double       read fVOL_ENTR     write fVOL_ENTR;
    property VOL_DISP    : Double       read fVOL_DISP     write fVOL_DISP;
    property VOL_SAIDAS  : Double       read fVOL_SAIDAS   write fVOL_SAIDAS;
    property ESTQ_ESCR   : Double       read fESTQ_ESCR    write fESTQ_ESCR;
    property VAL_AJ_PERDA: Double       read fVAL_AJ_PERDA write fVAL_AJ_PERDA;
    property VAL_AJ_GANHO: Double       read fVAL_AJ_GANHO write fVAL_AJ_GANHO;
    property FECH_FISICO : Double       read fFECH_FISICO  write fFECH_FISICO;

    property Registro1320: TRegistro1320List read FRegistro1320 write FRegistro1320;
  end;

  /// Registro 1310 - Lista

  TRegistro1310List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1310;
    procedure SetItem(Index: Integer; const Value: TRegistro1310);
  public
    function New(): TRegistro1310;
    property Items[Index: Integer]: TRegistro1310 read GetItem write SetItem;
  end;

  /// Registro 1320 - VOLUME DE VENDAS

  TRegistro1320 = class
  private
    fNUM_BICO   : String;       /// Bico Ligado à Bomba
    fNR_INTERV  : String;       /// Número da intervenção
    fMOT_INTERV : String;       /// Motivo da Intervenção
    fNOM_INTERV : String;       /// Nome do Interventor
    fCNPJ_INTERV: String;       /// CNPJ da empresa responsável pela intervenção
    fCPF_INTERV : String;       /// CPF do técnico responsável pela intervenção
    fVAL_FECHA  : Double;       /// Valor da leitura final do contador, no fechamento do bico
    fVAL_ABERT  : Double;       /// Valor da leitura inicial do contador, na abertura do bico
    fVOL_AFERI  : Double;       /// Aferições da Bomba, em litros
    fVOL_VENDAS : Double;       /// Vendas (08 – 09 - 10 ) do bico, em litros
  public
    constructor Create(AOwner: TRegistro1310); virtual; /// Create

    property NUM_BICO:String       read fNUM_BICO    write fNUM_BICO    ;
    property NR_INTERV:String      read fNR_INTERV   write fNR_INTERV   ;
    property MOT_INTERV:String     read fMOT_INTERV  write fMOT_INTERV  ;
    property NOM_INTERV:String     read fNOM_INTERV  write fNOM_INTERV  ;
    property CNPJ_INTERV:String    read fCNPJ_INTERV write fCNPJ_INTERV ;
    property CPF_INTERV:String     read fCPF_INTERV  write fCPF_INTERV  ;
    property VAL_FECHA:Double          read fVAL_FECHA   write fVAL_FECHA   ;
    property VAL_ABERT:Double          read fVAL_ABERT   write fVAL_ABERT   ;
    property VOL_AFERI:Double          read fVOL_AFERI   write fVOL_AFERI   ;
    property VOL_VENDAS:Double         read fVOL_VENDAS  write fVOL_VENDAS  ;
  end;

  /// Registro 1320 - Lista

  TRegistro1320List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1320;
    procedure SetItem(Index: Integer; const Value: TRegistro1320);
  public
    function New(AOwner: TRegistro1310): TRegistro1320;
    property Items[Index: Integer]: TRegistro1320 read GetItem write SetItem;
  end;

  /// Registro 1350 - BOMBAS

  TRegistro1350 = class
  private
    fSERIE: String;              /// Número de Série da Bomba
    fFABRICANTE: String;         /// Nome do Fabricante da Bomba
    fMODELO: String;             /// Modelo da Bomba
    fTIPO_MEDICAO: TACBrMedicao; /// Identificador de medição: [ 0 - analógico -  1 – digital ]

    FRegistro1360: TRegistro1360List;  /// BLOCO 1 - Lista de Registro1360 (FILHO)
    FRegistro1370: TRegistro1370List;  /// BLOCO 1 - Lista de Registro1360 (FILHO)
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property SERIE:        String read fSERIE        write fSERIE       ;
    property FABRICANTE:   String read fFABRICANTE   write fFABRICANTE  ;
    property MODELO:       String read fMODELO       write fMODELO      ;
    property TIPO_MEDICAO: TACBrMedicao read fTIPO_MEDICAO write fTIPO_MEDICAO;

    property Registro1360: TRegistro1360List read FRegistro1360 write FRegistro1360;
    property Registro1370: TRegistro1370List read FRegistro1370 write FRegistro1370;
  end;

  /// Registro 1350 - Lista

  TRegistro1350List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistro1350;
    procedure SetItem(Index: Integer; const Value: TRegistro1350);
  public
    function New(): TRegistro1350;
    property Items[Index: Integer]: TRegistro1350 read GetItem write SetItem;
  end;

  /// Registro 1360: LACRES DA BOMBA
  TRegistro1360 = class
  private
    fNUM_LACRE: String;         /// Número de Série da Bomba
    fDT_APLICACAO: TDateTime;   /// Nome do Fabricante da Bomba
  public
    constructor Create(AOwner: TRegistro1350); virtual; /// Create

    property NUM_LACRE:    String    read fNUM_LACRE     write fNUM_LACRE    ;
    property DT_APLICACAO: TDateTime read fDT_APLICACAO  write fDT_APLICACAO ;
  end;

  /// Registro 1360 - Lista

  TRegistro1360List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistro1360;
    procedure SetItem(Index: Integer; const Value: TRegistro1360);
  public
    function New(AOwner: TRegistro1350): TRegistro1360;
    property Items[Index: Integer]: TRegistro1360 read GetItem write SetItem;
  end;

  /// Registro 1370 - BICOS DA BOMBA

  TRegistro1370 = class
  private
      fNUM_BICO:String;    /// Número seqüencial do bico ligado a bomba N 003 - O
      fCOD_ITEM:String;    /// Código do Produto, constante do registro 0200 C 060 - O
      fNUM_TANQUE:String;  /// Tanque que armazena o combustível.
  public
    constructor Create(AOwner: TRegistro1350); virtual; /// Create

    property   NUM_BICO  :String read fNUM_BICO   write fNUM_BICO  ;    /// Número seqüencial do bico ligado a bomba
    property   COD_ITEM  :String read fCOD_ITEM   write fCOD_ITEM  ;    /// Código do Produto, constante do registro
    property   NUM_TANQUE:String read fNUM_TANQUE write fNUM_TANQUE;  /// Tanque que armazena o combustível.
  end;

  /// Registro 1370  - Lista

  TRegistro1370List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistro1370 ;
    procedure SetItem(Index: Integer; const Value: TRegistro1370 );
  public
    function New(AOwner: TRegistro1350): TRegistro1370 ;
    property Items[Index: Integer]: TRegistro1370  read GetItem write SetItem;
  end;

  /// Registro 1390 - CONTROLE DE PRODUÇÃO DE USINA

  TRegistro1390 = class
  private
    fCOD_PROD: String;      /// Código do item (campo 02 do Registro 0200)

    FRegistro1391: TRegistro1391List;  /// REGISTRO 1391: PRODUÇÃO DIÁRIA DA USINA
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_PROD: String read fCOD_PROD write fCOD_PROD;

    property Registro1391: TRegistro1391List read FRegistro1391 write FRegistro1391;
  end;

  /// Registro 1390 - Lista

  TRegistro1390List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1390;
    procedure SetItem(Index: Integer; const Value: TRegistro1390);
  public
    function New(): TRegistro1390;
    property Items[Index: Integer]: TRegistro1390 read GetItem write SetItem;
  end;

  /// REGISTRO 1391: PRODUÇÃO DIÁRIA DA USINA

  TRegistro1391 = class
  private
    fDT_REGISTRO  : TDateTime;  /// Data de produção (DDMMAAAA)
    fQTD_MOID     : Currency;   /// Quantidade de cana esmagada (toneladas)
    fESTQ_INI     : Currency;   /// Estoque inicial (litros / Kg)
    fQTD_PRODUZ   : Currency;   /// Quantidade produzida (litros / Kg)
    fENT_ANID_HID : Currency;   /// Entrada de álcool anidro decorrente da transformação do álcool hidratado ou Entrada de álcool hidratado decorrente da transformação do álcool anidro (litros)
    fOUTR_ENTR    : Currency;   /// Outras entradas (litros / Kg)
    fPERDA        : Currency;   /// Evaporação (litros) ou Quebra de peso (Kg)
    fCONS         : Currency;   /// Consumo (litros)
    fSAI_ANI_HID  : Currency;   /// Saída para transformação (litros).
    fSAIDAS       : Currency;   /// Saídas (litros / Kg)
    fESTQ_FIN     : Currency;   /// Estoque final (litros / Kg)
    fESTQ_INI_MEL : Currency;   /// Estoque inicial de mel residual (Kg)
    fPROD_DIA_MEL : Currency;   /// Produção de mel residual (Kg) e entradas de mel (Kg)
    fUTIL_MEL     : Currency;   /// Mel residual utilizado (Kg) e saídas de mel (Kg)
    fPROD_ALC_MEL : Currency;   /// Produção de álcool (litros) ou açúcar (Kg) proveniente do mel residual.
    fOBS          : String;     /// Observações
    FCOD_ITEM: string;          /// Informar o insumo conforme código do item (campo 02 do Registro 0200)
    FTP_RESIDUO: Integer;       /// Tipo de resíduo produzido: 01 - Bagaço de cana
                                                            // 02 - DDG
                                                            // 03 - WDG
    FQTD_RESIDUO: Extended;      /// Quantidade de resíduo produzido (toneladas)
    FQTD_RESIDUO_DDG:Extended;   /// Quantidade de resíduo produzido de DDG (toneladas)
    FQTD_RESIDUO_WDG:Extended;   /// Quantidade de resíduo produzido de WDG (toneladas)
    FQTD_RESIDUO_CANA: Extended; /// Quantidade de resíduo produzido de bagaço de cana (toneladas)
  public                        
    constructor Create(AOwner: TRegistro1390); virtual; /// Create

    property DT_REGISTRO: TDateTime read fDT_REGISTRO write fDT_REGISTRO;
    property QTD_MOID: Currency read fQTD_MOID write fQTD_MOID;
    property ESTQ_INI: Currency read fESTQ_INI write fESTQ_INI;
    property QTD_PRODUZ: Currency read fQTD_PRODUZ write fQTD_PRODUZ;
    property ENT_ANID_HID: Currency read fENT_ANID_HID write fENT_ANID_HID;
    property OUTR_ENTR: Currency read fOUTR_ENTR write fOUTR_ENTR;
    property PERDA: Currency read fPERDA write fPERDA;
    property CONS: Currency read fCONS write fCONS;
    property SAI_ANI_HID: Currency read fSAI_ANI_HID write fSAI_ANI_HID;
    property SAIDAS: Currency read fSAIDAS write fSAIDAS;
    property ESTQ_FIN: Currency read fESTQ_FIN write fESTQ_FIN;
    property ESTQ_INI_MEL: Currency read fESTQ_INI_MEL write fESTQ_INI_MEL;
    property PROD_DIA_MEL: Currency read fPROD_DIA_MEL write fPROD_DIA_MEL;
    property UTIL_MEL: Currency read fUTIL_MEL write fUTIL_MEL;
    property PROD_ALC_MEL: Currency read fPROD_ALC_MEL write fPROD_ALC_MEL;
    property OBS: String read fOBS write fOBS;
    property COD_ITEM: string read FCOD_ITEM write FCOD_ITEM;
    property TP_RESIDUO: Integer read FTP_RESIDUO write FTP_RESIDUO;
    property QTD_RESIDUO: Extended read FQTD_RESIDUO write FQTD_RESIDUO;
    property QTD_RESIDUO_DDG: Extended read FQTD_RESIDUO_DDG write FQTD_RESIDUO_DDG;
    property QTD_RESIDUO_WDG: Extended read FQTD_RESIDUO_WDG write FQTD_RESIDUO_WDG;
    property QTD_RESIDUO_CANA: Extended read FQTD_RESIDUO_CANA write FQTD_RESIDUO_CANA;
  end;

  /// Registro 1391 - Lista

  TRegistro1391List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1391;
    procedure SetItem(Index: Integer; const Value: TRegistro1391);
  public
    function New(AOwner: TRegistro1390): TRegistro1391;
    property Items[Index: Integer]: TRegistro1391 read GetItem write SetItem;
  end;


  /// Registro 1400 - CINFORMAÇÃO SOBRE VALORES AGREGADOS.

  TRegistro1400 = class
  private
    fCOD_ITEM: String;      /// Código do item (campo 02 do Registro 0200)
    fCOD_ITEM_IPM : String;
    fMUN: String;           /// Código do Município de origem
    fVALOR: Currency;      /// Valor mensal correspondente ao município
  public
    constructor Create(AOwner: TRegistro1001); virtual; /// Create

    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property COD_ITEM_IPM: String read fCOD_ITEM_IPM write fCOD_ITEM_IPM;
    property MUN: String read FMUN write FMUN;
    property VALOR: Currency read FVALOR write FVALOR;
  end;

  /// Registro 1400 - Lista

  TRegistro1400List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1400;
    procedure SetItem(Index: Integer; const Value: TRegistro1400);
  public
    function New(AOwner: TRegistro1001): TRegistro1400;
    property Items[Index: Integer]: TRegistro1400 read GetItem write SetItem;
  end;

  /// Registro 1500 - NOTA FISCAL/CONTA DE ENERGIA ELÉTRICA (CÓDIGO 06) - OPERAÇÕES INTERESTADUAIS.

  TRegistro1500 = class
  private
    fIND_OPER: String;                   /// Indicador do tipo de operação:
    fIND_EMIT: String;                   /// Indicador do emitente do documento fiscal:
    fCOD_PART: String;                   /// Código do participante (campo 02 do Registro 0150):
    fCOD_MOD: String;                    /// Código do modelo do documento fiscal conforme a Tabela 4.1.1
    fCOD_SIT: TACBrCodSit;               /// Código da situação do documento fiscal conforme a Tabela 4.1.2
    fSER: String;                        /// Série do documento fiscal
    fSUB: String;                        /// Subsérie do documento fiscal
    fCOD_CONS: TACBrClasseConsumo;       /// Código de classe de consumo de energia elétrica conforme a Tabela 4.4.5 ou Código da classe de consumo de gás canalizado conforme Tabela 4.4.3.
    fNUM_DOC: String;                    /// Número do documento fiscal
    fDT_DOC: TDateTime;                  /// Data da emissão do documento fiscal
    fDT_E_S: TDateTime;                  /// Data da entrada ou da saída
    fVL_DOC: currency;                   /// Valor total do documento fiscal
    fVL_DESC: currency;                  /// Valor total do desconto
    fVL_FORN: currency;                  /// Valor total fornecido/consumido
    fVL_SERV_NT: currency;               /// Valor total dos serviços não-tributados pelo ICMS
    fVL_TERC: currency;                  /// Valor total cobrado em nome de terceiros
    fVL_DA: currency;                    /// Valor total de despesas acessórias indicadas no documento fiscal
    fVL_BC_ICMS: currency;               /// Valor acumulado da base de cálculo do ICMS
    fVL_ICMS: currency;                  /// Valor acumulado do ICMS
    fVL_BC_ICMS_ST: currency;            /// Valor acumulado da base de cálculo do ICMS substituição tributária
    fVL_ICMS_ST: currency;               /// Valor acumulado do ICMS retido por substituição tributária
    fCOD_INF: String;                    /// Código da informação complementar do documento fiscal (campo 02 do Registro 0450)
    fVL_PIS: currency;                   /// Valor do PIS
    fVL_COFINS: currency;                /// Valor da COFINS
    fTP_LIGACAO: TACBrTipoLigacao;       /// Código de tipo de Ligação [ 1 - Monofásico 2 - Bifásico 3 - Trifásico ]
    fCOD_GRUPO_TENSAO: TACBrGrupoTensao; /// Código de grupo de tensão: Vide Manual Registro C500 Campo 27

    FRegistro1510: TRegistro1510List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_OPER: String read FIND_OPER write FIND_OPER;
    property IND_EMIT: String read FIND_EMIT write FIND_EMIT;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property COD_SIT: TACBrCodSit read FCOD_SIT write FCOD_SIT;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property COD_CONS: TACBrClasseConsumo read FCOD_CONS write FCOD_CONS;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property DT_E_S: TDateTime read FDT_E_S write FDT_E_S;
    property VL_DOC: currency read FVL_DOC write FVL_DOC;
    property VL_DESC: currency read FVL_DESC write FVL_DESC;
    property VL_FORN: currency read FVL_FORN write FVL_FORN;
    property VL_SERV_NT: currency read FVL_SERV_NT write FVL_SERV_NT;
    property VL_TERC: currency read FVL_TERC write FVL_TERC;
    property VL_DA: currency read FVL_DA write FVL_DA;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
    property VL_BC_ICMS_ST: currency read FVL_BC_ICMS_ST write FVL_BC_ICMS_ST;
    property VL_ICMS_ST: currency read FVL_ICMS_ST write FVL_ICMS_ST;
    property COD_INF: String read FCOD_INF write FCOD_INF;
    property VL_PIS: currency read FVL_PIS write FVL_PIS;
    property VL_COFINS: currency read FVL_COFINS write FVL_COFINS;
    property TP_LIGACAO:        TACBrTipoLigacao read fTP_LIGACAO       write fTP_LIGACAO;
    property COD_GRUPO_TENSAO:  TACBrGrupoTensao read fCOD_GRUPO_TENSAO write fCOD_GRUPO_TENSAO;
    //
    property Registro1510: TRegistro1510List read FRegistro1510 write FRegistro1510;
  end;

  /// Registro 1500 - Lista

  TRegistro1500List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1500;
    procedure SetItem(Index: Integer; const Value: TRegistro1500);
  public
    function New(): TRegistro1500;
    property Items[Index: Integer]: TRegistro1500 read GetItem write SetItem;
  end;

  /// Registro 1510 - ITENS DO DOCUMENTO NOTA FISCAL/CONTA ENERGIA ELÉTRICA (CÓDIGO 06).

  TRegistro1510 = class
  private
    fNUM_ITEM: String;             /// Número seqüencial do item no documento fiscal
    fCOD_ITEM: String;             /// Código do item (campo 02 do Registro 0200)
    fCOD_CLASS: String;            /// Código de classificação do item de energia elétrica, conforme a Tabela 4.4.1
    fQTD: Double;                  /// Quantidade do item
    fUNID: String;                 /// Unidade do item (Campo 02 do registro 0190)
    fVL_ITEM: currency;            /// Valor do item
    fVL_DESC: currency;            /// Valor total do desconto
    fCST_ICMS: String;             /// Código da Situação Tributária, conforme a Tabela indicada no item 4.3.1
    fCFOP: String;                 /// Código Fiscal de Operação e Prestação
    fVL_BC_ICMS: currency;         /// Valor da base de cálculo do ICMS
    fALIQ_ICMS: currency;          /// Alíquota do ICMS
    fVL_ICMS: currency;            /// Valor do ICMS creditado/debitado
    fVL_BC_ICMS_ST: currency;      /// Valor da base de cálculo referente à substituição tributária
    fALIQ_ST: currency;            /// Alíquota do ICMS da substituição tributária na unidade da federação de destino
    fVL_ICMS_ST: currency;         /// Valor do ICMS referente à substituição tributária
    fIND_REC: TACBrTipoReceita;    /// Indicador do tipo de receita:
    fCOD_PART: String;             /// Código do participante receptor da receita, terceiro da operação (campo 02 do Registro 0150)
    fVL_PIS: currency;             /// Valor do PIS
    fVL_COFINS: currency;          /// Valor da COFINS
    fCOD_CTA: String;              /// Código da conta analítica contábil debitada/creditada
  public
    constructor Create(AOwner: TRegistro1500); virtual; /// Create

    property NUM_ITEM: String read FNUM_ITEM write FNUM_ITEM;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property COD_CLASS: String read FCOD_CLASS write FCOD_CLASS;
    property QTD: Double read FQTD write FQTD;
    property UNID: String read FUNID write FUNID;
    property VL_ITEM: currency read FVL_ITEM write FVL_ITEM;
    property VL_DESC: currency read FVL_DESC write FVL_DESC;
    property CST_ICMS: String read FCST_ICMS write FCST_ICMS;
    property CFOP: String read FCFOP write FCFOP;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property ALIQ_ICMS: currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
    property VL_BC_ICMS_ST: currency read FVL_BC_ICMS_ST write FVL_BC_ICMS_ST;
    property ALIQ_ST: currency read FALIQ_ST write FALIQ_ST;
    property VL_ICMS_ST: currency read FVL_ICMS_ST write FVL_ICMS_ST;
    property IND_REC: TACBrTipoReceita read FIND_REC write FIND_REC;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property VL_PIS: currency read FVL_PIS write FVL_PIS;
    property VL_COFINS: currency read FVL_COFINS write FVL_COFINS;
    property COD_CTA: String read FCOD_CTA write FCOD_CTA;
  end;

  /// Registro 1510 - Lista

  TRegistro1510List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1510;
    procedure SetItem(Index: Integer; const Value: TRegistro1510);
  public
    function New(AOwner: TRegistro1500): TRegistro1510;
    property Items[Index: Integer]: TRegistro1510 read GetItem write SetItem;
  end;

  /// Registro 1600 - TOTAL DAS OPERAÇÕES COM CARTÃO DE CRÉDITO E/OU DÉBITO,LOJA (PRIVATE LABEL) E DEMAIS INSTRUMENTOS DE PAGAMENTOS ELETRÔNICOS

  TRegistro1600 = class
  private
    fCOD_PART: String;      /// Código do participante (campo 02 do Registro 0150): identificação da instituição financeira e/ou de pagamento
    fTOT_CREDITO: currency; /// Valor total das operações de crédito realizadas no período
    fTOT_DEBITO: currency;  /// Valor total das operações de débito realizadas no período
  public
    constructor Create(AOwner: TRegistro1001); virtual; /// Create

    property COD_PART: String read FCOD_PART write FCOD_PART;
    property TOT_CREDITO: currency read FTOT_CREDITO write FTOT_CREDITO;
    property TOT_DEBITO: currency read FTOT_DEBITO write FTOT_DEBITO;
  end;

  /// Registro 1600 - Lista

  TRegistro1600List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1600;
    procedure SetItem(Index: Integer; const Value: TRegistro1600);
  public
    function New(AOwner: TRegistro1001): TRegistro1600;
    property Items[Index: Integer]: TRegistro1600 read GetItem write SetItem;
  end;

    /// Registro 1601 - OPERAÇÕES COM INSTRUMENTOS DE PAGAMENTOS ELETRÔNICOS

  TRegistro1601 = class
  private
    FCOD_PART_IP: String;   /// Código do participante (campo 02 do Registro 0150): identificação da instituição que efetuou o pagamento
    FCOD_PART_IT: String;   /// Código do participante (campo 02 do Registro 0150): identificação do intermediador da transação

    FTOT_VS: currency;      /// Valor total bruto das vendas e/ou prestações de serviços no campo de incidência do ICMS, incluindo operações com imunidade do imposto.
    FTOT_ISS: currency;     /// Valor total bruto das prestações de serviços no campo de incidência do ISS
    FTOT_OUTROS: currency;  /// Valor total de operações deduzido dos valores dos campos TOT_VS e TOT_ISS.
  public
    constructor Create(AOwner: TRegistro1001); virtual; /// Create

    property COD_PART_IP: String read FCOD_PART_IP write FCOD_PART_IP;
    property COD_PART_IT: String read FCOD_PART_IT write FCOD_PART_IT;

    property TOT_VS: currency read FTOT_VS write FTOT_VS;
    property TOT_ISS: currency read FTOT_ISS write FTOT_ISS;
    property TOT_OUTROS: currency read FTOT_OUTROS write FTOT_OUTROS;
  end;

  /// Registro 1601 - Lista

  TRegistro1601List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1601;
    procedure SetItem(Index: Integer; const Value: TRegistro1601);
  public
    function New(AOwner: TRegistro1001): TRegistro1601;
    property Items[Index: Integer]: TRegistro1601 read GetItem write SetItem;
  end;

  /// Registro 1700 - DOCUMENTOS FISCAIS UTILIZADOS

  TRegistro1700 = class
  private
    fCOD_DISP: TACBrDispositivo;  /// Codigo Dispositivo autorizado
    fCOD_MOD: String;             /// Codigo Modelo Documento Fiscal
    fSER: String;                 /// Serie Documento Fiscal
    fSUB: String;                 /// SubSerie Documento Fiscal
    fNUM_DOC_INI: String;         /// Numero Documento Fiscal Inicial - deve ser String
    fNUM_DOC_FIN: String;         /// Numero Documento Fiscal Final - deve ser String
    fNUM_AUT: String;             /// Numero da Autorizacao - deve ser String

    FRegistro1710: TRegistro1710List;  /// BLOCO 1- Lista de Registro1710 (FILHO fo FILHO)
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_DISP: TACBrDispositivo read fCOD_DISP write fCOD_DISP;
    property COD_MOD: String read fCOD_MOD write fCOD_MOD;
    property SER: String read fSER write fSER;
    property SUB: String read fSUB write fSUB;
    property NUM_DOC_INI: String read fNUM_DOC_INI write fNUM_DOC_INI;
    property NUM_DOC_FIN: String read fNUM_DOC_FIN write fNUM_DOC_FIN;
    property NUM_AUT: String read fNUM_AUT write fNUM_AUT;

    property Registro1710: TRegistro1710List read FRegistro1710 write FRegistro1710;
  end;

  /// Registro 1700 - Lista

  TRegistro1700List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1700;
    procedure SetItem(Index: Integer; const Value: TRegistro1700);
  public
    function New(): TRegistro1700;
    property Items[Index: Integer]: TRegistro1700 read GetItem write SetItem;
  end;

  /// Registro 1710 - DOCUMENTOS FISCAIS CANCELADOS/INUTILIZADOS

  TRegistro1710 = class
  private
    fNUM_DOC_INI: String;     /// Numero Documento Fiscal Inicial
    fNUM_DOC_FIN: String;     /// Numero Documento Fiscal Final
  public
    constructor Create(AOwner: TRegistro1700); virtual; /// Create

    property NUM_DOC_INI: String read fNUM_DOC_INI write fNUM_DOC_INI;
    property NUM_DOC_FIN: String read fNUM_DOC_FIN write fNUM_DOC_FIN;
  end;

  /// Registro 1710 - Lista

  TRegistro1710List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1710;
    procedure SetItem(Index: Integer; const Value: TRegistro1710);
  public
    function New(AOwner: TRegistro1700): TRegistro1710;
    property Items[Index: Integer]: TRegistro1710 read GetItem write SetItem;
  end;

  /// Registro 1800 - DEMONSTRATIVO CREDITO ICMS SOBRE TRANSPORTE AEREO

  TRegistro1800 = class
  private
    fVL_CARGA: Currency;        /// Valor Prestacoes Cargas Tributado
    fVL_PASS: Currency;         /// Valor Prestacoes Cargas Nao Tributado
    fVL_FAT: Currency;          /// Valor total do faturamento
    fIND_RAT: Currency;         /// Indice para rateio
    fVL_ICMS_ANT: Currency;     /// Valor Total Creditos ICMS
    fVL_BC_ICMS: Currency;      /// Valor Base Calculo ICMS
    fVL_ICMS_APUR: Currency;    /// Valor ICMS apurado no calculo
    fVL_BC_ICMS_APUR: Currency; /// Valor base ICMS apurada
    fVL_DIF: Currency;          /// Valor diferenca a estorno de credito na apuracao
  public
    constructor Create(AOwner: TRegistro1001); virtual; /// Create

    property VL_CARGA:Currency read fVL_CARGA write fVL_CARGA ;
    property VL_PASS:Currency read fVL_PASS write fVL_PASS ;
    property VL_FAT:Currency read fVL_FAT write fVL_FAT ;
    property IND_RAT:Currency read fIND_RAT write fIND_RAT ;
    property VL_ICMS_ANT:Currency read fVL_ICMS_ANT write fVL_ICMS_ANT ;
    property VL_BC_ICMS:Currency read fVL_BC_ICMS write fVL_BC_ICMS ;
    property VL_ICMS_APUR:Currency read fVL_ICMS_APUR write fVL_ICMS_APUR ;
    property VL_BC_ICMS_APUR:Currency read fVL_BC_ICMS_APUR write fVL_BC_ICMS_APUR ;
    property VL_DIF:Currency read fVL_DIF write fVL_DIF ;
  end;

 /// Registro 1800 - Lista

 TRegistro1800List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1800;
    procedure SetItem(Index: Integer; const Value: TRegistro1800);
  public
    function New(AOwner: TRegistro1001): TRegistro1800;
    property Items[Index: Integer]: TRegistro1800 read GetItem write SetItem;
  end;

  /// Registro 1900 - INDICADOR DE SUB-APURAÇÃO DO ICMS
  TRegistro1900 = class
  private
    fIND_APUR_ICMS: string;
    fDESCR_COMPL_OUT_APUR: string;
    FRegistro1910: TRegistro1910List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy
    property IND_APUR_ICMS: string read fIND_APUR_ICMS write fIND_APUR_ICMS;
    property DESCR_COMPL_OUT_APUR: string read fDESCR_COMPL_OUT_APUR write fDESCR_COMPL_OUT_APUR;

    property Registro1910: TRegistro1910List read FRegistro1910 write FRegistro1910;
  end;

  TRegistro1900List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1900;
    procedure SetItem(Index: Integer; const Value: TRegistro1900);
  public
    function New(): TRegistro1900;
    property Items[Index: Integer]: TRegistro1900 read GetItem write SetItem;
  end;

  /// Registro 1910 - PERÍODO DA SUB-APURAÇÃO DO ICMS
  TRegistro1910 = class
  private
    fDT_FIN: TDateTime;
    fDT_INI: TDateTime;
    FRegistro1920: TRegistro1920List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy
    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;

    property Registro1920: TRegistro1920List read FRegistro1920 write FRegistro1920;
  end;

  TRegistro1910List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1910;
    procedure SetItem(Index: Integer; const Value: TRegistro1910);
  public
    function New(): TRegistro1910;
    property Items[Index: Integer]: TRegistro1910 read GetItem write SetItem;
  end;

  /// Registro 1920 - SUB-APURAÇÃO DO ICMS
  TRegistro1920 = class
  private
    fVL_TOT_TRANSF_DEBITOS_OA: Currency;
    fVL_TOT_AJ_DEBITOS_OA: Currency;
    fVL_ESTORNOS_CRED_OA: Currency;
    fVL_TOT_TRANSF_CREDITOS_OA: Currency;
    fVL_TOT_AJ_CREDITOS_OA: Currency;
    fVL_ESTORNOS_DEB_OA: Currency;
    fVL_SLD_CREDOR_ANT_OA: Currency;
    fVL_SLD_APURADO_OA: Currency;
    fVL_TOT_DED: Currency;
    fVL_ICMS_RECOLHER_OA: Currency;
    fVL_SLD_CREDOR_TRANSP_OA: Currency;
    fDEB_ESP_OA: Currency;
    FRegistro1921: TRegistro1921List;
    FRegistro1925: TRegistro1925List;
    FRegistro1926: TRegistro1926List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy
    property VL_TOT_TRANSF_DEBITOS_OA : Currency read fVL_TOT_TRANSF_DEBITOS_OA  write fVL_TOT_TRANSF_DEBITOS_OA;
    property VL_TOT_AJ_DEBITOS_OA     : Currency read fVL_TOT_AJ_DEBITOS_OA      write fVL_TOT_AJ_DEBITOS_OA;
    property VL_ESTORNOS_CRED_OA      : Currency read fVL_ESTORNOS_CRED_OA       write fVL_ESTORNOS_CRED_OA;
    property VL_TOT_TRANSF_CREDITOS_OA: Currency read fVL_TOT_TRANSF_CREDITOS_OA write fVL_TOT_TRANSF_CREDITOS_OA;
    property VL_TOT_AJ_CREDITOS_OA    : Currency read fVL_TOT_AJ_CREDITOS_OA     write fVL_TOT_AJ_CREDITOS_OA;
    property VL_ESTORNOS_DEB_OA       : Currency read fVL_ESTORNOS_DEB_OA        write fVL_ESTORNOS_DEB_OA;
    property VL_SLD_CREDOR_ANT_OA     : Currency read fVL_SLD_CREDOR_ANT_OA      write fVL_SLD_CREDOR_ANT_OA;
    property VL_SLD_APURADO_OA        : Currency read fVL_SLD_APURADO_OA         write fVL_SLD_APURADO_OA;
    property VL_TOT_DED               : Currency read fVL_TOT_DED                write fVL_TOT_DED;
    property VL_ICMS_RECOLHER_OA      : Currency read fVL_ICMS_RECOLHER_OA       write fVL_ICMS_RECOLHER_OA;
    property VL_SLD_CREDOR_TRANSP_OA  : Currency read fVL_SLD_CREDOR_TRANSP_OA   write fVL_SLD_CREDOR_TRANSP_OA;
    property DEB_ESP_OA               : Currency read fDEB_ESP_OA                write fDEB_ESP_OA;

    property Registro1921: TRegistro1921List read FRegistro1921 write FRegistro1921;
    property Registro1925: TRegistro1925List read FRegistro1925 write FRegistro1925;
    property Registro1926: TRegistro1926List read FRegistro1926 write FRegistro1926;
  end;

  TRegistro1920List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1920;
    procedure SetItem(Index: Integer; const Value: TRegistro1920);
  public
    function New(): TRegistro1920;
    property Items[Index: Integer]: TRegistro1920 read GetItem write SetItem;
  end;

  /// Registro 1921 - AJUSTE/BENEFÍCIO/INCENTIVO DA SUB-APURAÇÃO DO ICMS
  TRegistro1921 = class
  private
    fCOD_AJ_APUR: String;
    fDESCR_COMPL_AJ: String;
    fVL_AJ_APUR: Currency;
    FRegistro1922: TRegistro1922List;
    FRegistro1923: TRegistro1923List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy
    property COD_AJ_APUR    : String   read fCOD_AJ_APUR    write fCOD_AJ_APUR;
    property DESCR_COMPL_AJ : String   read fDESCR_COMPL_AJ write fDESCR_COMPL_AJ;
    property VL_AJ_APUR     : Currency read fVL_AJ_APUR     write fVL_AJ_APUR;

    property Registro1922: TRegistro1922List read FRegistro1922 write FRegistro1922;
    property Registro1923: TRegistro1923List read FRegistro1923 write FRegistro1923;
  end;

  TRegistro1921List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1921;
    procedure SetItem(Index: Integer; const Value: TRegistro1921);
  public
    function New(): TRegistro1921;
    property Items[Index: Integer]: TRegistro1921 read GetItem write SetItem;
  end;

  /// Registro 1922 - NFORMAÇÕES ADICIONAIS DOS AJUSTES DA SUB-APURAÇÃO DO ICMS
  TRegistro1922 = class
  private
    fNUM_DA: String;
    fNUM_PROC: String;
    fIND_PROC: String;
    fPROC: String;
    fTXT_COMPL: String;
  public
    property NUM_DA    : String read fNUM_DA    write fNUM_DA;
    property NUM_PROC  : String read fNUM_PROC  write fNUM_PROC;
    property IND_PROC  : String read fIND_PROC  write fIND_PROC;
    property PROC      : String read fPROC      write fPROC;
    property TXT_COMPL : String read fTXT_COMPL write fTXT_COMPL;
  end;

  TRegistro1922List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1922;
    procedure SetItem(Index: Integer; const Value: TRegistro1922);
  public
    function New: TRegistro1922;
    property Items[Index: Integer]: TRegistro1922 read GetItem write SetItem;
  end;

  /// Registro 1923 - INFORMAÇÕES ADICIONAIS DOS AJUSTES DA SUB-APURAÇÃO DO ICMS
  TRegistro1923 = class
  private
    fCOD_PART: String;
    fCOD_MOD: String;
    fSER: String;
    fSUB: String;
    fNUM_DOC: String;
    fDT_DOC: TDateTime;
    fCOD_ITEM: String;
    fVL_AJ_ITEM: Currency;
    fCHV_DOCe: String;
  public
    property COD_PART   : String    read fCOD_PART   write fCOD_PART;
    property COD_MOD    : String    read fCOD_MOD    write fCOD_MOD;
    property SER        : String    read fSER        write fSER;
    property SUB        : String    read fSUB        write fSUB;
    property NUM_DOC    : String    read fNUM_DOC    write fNUM_DOC;
    property DT_DOC     : TDateTime read fDT_DOC     write fDT_DOC;
    property COD_ITEM   : String    read fCOD_ITEM   write fCOD_ITEM;
    property VL_AJ_ITEM : Currency  read fVL_AJ_ITEM write fVL_AJ_ITEM;
    property CHV_DOCe   : String    read fCHV_DOCe   write fCHV_DOCe;
  end;

  TRegistro1923List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1923;
    procedure SetItem(Index: Integer; const Value: TRegistro1923);
  public
    function New: TRegistro1923;
    property Items[Index: Integer]: TRegistro1923 read GetItem write SetItem;
  end;

  /// Registro 1925 - INFORMAÇÕES ADICIONAIS DA SUB-APURAÇÃO – VALORES DECLARATÓRIOS
  TRegistro1925 = class
  private
    fCOD_INF_ADIC: String;
    fVL_INF_ADIC: Currency;
    fDESCR_COMPL_AJ: String;
  public
    property COD_INF_ADIC   : String    read fCOD_INF_ADIC   write fCOD_INF_ADIC;
    property VL_INF_ADIC    : Currency  read fVL_INF_ADIC    write fVL_INF_ADIC;
    property DESCR_COMPL_AJ : String    read fDESCR_COMPL_AJ write fDESCR_COMPL_AJ;
  end;

  TRegistro1925List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1925;
    procedure SetItem(Index: Integer; const Value: TRegistro1925);
  public
    function New: TRegistro1925;
    property Items[Index: Integer]: TRegistro1925 read GetItem write SetItem;
  end;

  /// Registro 1926 - OBRIGAÇÕES DO ICMS A RECOLHER – OPERAÇÕES REFERENTES À SUB-APURAÇÃO
  TRegistro1926 = class
  private
    fCOD_OR: String;
    fVL_OR: Currency;
    fDT_VCTO: TDateTime;
    fCOD_REC: String;
    fNUM_PROC: String;
    fIND_PROC: String;
    fPROC: String;
    fTXT_COMPL: String;
    fMES_REF: String;
  public
    property COD_OR    : String    read fCOD_OR    write fCOD_OR;
    property VL_OR     : Currency  read fVL_OR     write fVL_OR;
    property DT_VCTO   : TDateTime read fDT_VCTO   write fDT_VCTO;
    property COD_REC   : String    read fCOD_REC   write fCOD_REC;
    property NUM_PROC  : String    read fNUM_PROC  write fNUM_PROC;
    property IND_PROC  : String    read fIND_PROC  write fIND_PROC;
    property PROC      : String    read fPROC      write fPROC;
    property TXT_COMPL : String    read fTXT_COMPL write fTXT_COMPL;
    property MES_REF   : String    read fMES_REF   write fMES_REF;
  end;

  TRegistro1926List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1926;
    procedure SetItem(Index: Integer; const Value: TRegistro1926);
  public
    function New: TRegistro1926;
    property Items[Index: Integer]: TRegistro1926 read GetItem write SetItem;
  end;


 /// Registro 1960 - REGISTRO 1960: GIAF 1 - GUIA DE INFORMAÇÃO E APURAÇÃO DE INCENTIVOS FISCAIS E FINANCEIROS: INDÚSTRIA (CRÉDITO PRESUMIDO)
  TRegistro1960 = class
  private
    fIND_AP: String;   ///Indicador da sub-apuração por tipo de benefício(conforme tabela 4.7.1)
    fG1_01: Currency;  /// Percentual de crédito presumido
    fG1_02: Currency;  /// Saídas não incentivadas de PI
    fG1_03: Currency;  /// Saídas incentivadas de PI
    fG1_04: Currency;  /// Saídas incentivadas de PI para fora do Nordeste
    fG1_05: Currency;  /// Saldo devedor do ICMS antes das deduções do incentivo
    fG1_06: Currency;  /// Saldo devedor do ICMS relativo à faixa incentivada de PI
    fG1_07: Currency;  /// Crédito presumido nas saídas incentivadas de PI para fora do Nordeste
    fG1_08: Currency;  /// Saldo devedor relativo à faixa incentivada de PI após o crédito presumido nas saídas para fora do Nordeste
    fG1_09: Currency;  /// Crédito presumido
    fG1_10: Currency;  /// Dedução de incentivo da Indústria (crédito presumido)
    fG1_11: Currency;  /// Saldo devedor do ICMS após deduções
  public
    constructor Create(AOwner: TRegistro1001); virtual; /// Create
    property IND_AP : String    read fIND_AP write fIND_AP;
    property G1_01  : Currency  read fG1_01  write fG1_01;
    property G1_02  : Currency  read fG1_02  write fG1_02;
    property G1_03  : Currency  read fG1_03  write fG1_03;
    property G1_04  : Currency  read fG1_04  write fG1_04;
    property G1_05  : Currency  read fG1_05  write fG1_05;
    property G1_06  : Currency  read fG1_06  write fG1_06;
    property G1_07  : Currency  read fG1_07  write fG1_07;
    property G1_08  : Currency  read fG1_08  write fG1_08;
    property G1_09  : Currency  read fG1_09  write fG1_09;
    property G1_10  : Currency  read fG1_10  write fG1_10;
    property G1_11  : Currency  read fG1_11  write fG1_11;
  end;

  TRegistro1960List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1960;
    procedure SetItem(Index: Integer; const Value: TRegistro1960);
  public
    function New(AOwner: TRegistro1001): TRegistro1960;
    property Items[Index: Integer]: TRegistro1960 read GetItem write SetItem;
  end;

  ///REGISTRO 1970: GIAF 3 - GUIA DE INFORMAÇÃO E APURAÇÃO DE INCENTIVOS FISCAIS E FINANCEIROS: IMPORTAÇÃO (DIFERIMENTO NA ENTRADA E CRÉDITO PRESUMIDO NA SAÍDA SUBSEQUENTE))
  TRegistro1970 = class
  private
    fIND_AP: String;                   /// Indicador da sub-apuração por tipo de benefício (conforme tabela 4.7.1)
    fG3_01: Currency;                  /// Importações com ICMS diferido
    fG3_02: Currency;                  /// ICMS diferido nas importações
    fG3_03: Currency;                  /// Saídas não incentivadas de PI
    fG3_04: Currency;                  /// Percentual de incentivo nas saídas para fora do Estado
    fG3_05: Currency;                  /// Saídas incentivadas de PI para fora do Estado
    fG3_06: Currency;                  /// ICMS das saídas incentivadas de PI para fora do Estado
    fG3_07: Currency;                  /// Crédito presumido nas saídas para fora do Estado.
    fG3_T : Currency;                  /// Dedução de incentivo da Importação (crédito presumido)
    fG3_08: Currency;                  /// Saldo devedor do ICMS antes das deduções do incentivo
    fG3_09: Currency;                  /// Saldo devedor do ICMS após deduções do incentivo
    FRegistro1975: TRegistro1975List;  /// BLOCO 1- Lista de Registro1975 (FILHO fo FILHO)
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy
    property IND_AP : String    read fIND_AP write fIND_AP;
    property G3_01  : Currency  read fG3_01  write fG3_01;
    property G3_02  : Currency  read fG3_02  write fG3_02;
    property G3_03  : Currency  read fG3_03  write fG3_03;
    property G3_04  : Currency  read fG3_04  write fG3_04;
    property G3_05  : Currency  read fG3_05  write fG3_05;
    property G3_06  : Currency  read fG3_06  write fG3_06;
    property G3_07  : Currency  read fG3_07  write fG3_07;
    property G3_T   : Currency  read fG3_T   write fG3_T;
    property G3_08  : Currency  read fG3_08  write fG3_08;
    property G3_09  : Currency  read fG3_09  write fG3_09;
    property Registro1975: TRegistro1975List read FRegistro1975 write FRegistro1975;
  end;

  TRegistro1970List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1970;
    procedure SetItem(Index: Integer; const Value: TRegistro1970);
  public
    function New(): TRegistro1970;
    property Items[Index: Integer]: TRegistro1970 read GetItem write SetItem;
  end;

  ///REGISTRO 1975: GIAF 3 - GUIA DE INFORMAÇÃO E APURAÇÃO DE INCENTIVOS FISCAIS E FINANCEIROS: IMPORTAÇÃO (SAÍDAS INTERNAS POR FAIXA DE ALÍQUOTA)
  TRegistro1975 = class
  private
    fALIQ_IMP_BASE: Currency; /// Alíquota incidente sobre as importações-base( Valores Válidos: [3,50; 6,00; 8,00; 10,00] )
    fG3_10: Currency;         /// Saídas incentivadas de PI
    fG3_11: Currency;         /// Importações-base para o crédito presumido
    fG3_12: Currency;         /// Crédito presumido nas saídas internas
  public
    property ALIQ_IMP_BASE : Currency    read fALIQ_IMP_BASE write fALIQ_IMP_BASE;
    property G3_10  : Currency  read fG3_10  write fG3_10;
    property G3_11  : Currency  read fG3_11  write fG3_11;
    property G3_12  : Currency  read fG3_12  write fG3_12;
  end;

  TRegistro1975List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1975;
    procedure SetItem(Index: Integer; const Value: TRegistro1975);
  public
    function New(): TRegistro1975;
    property Items[Index: Integer]: TRegistro1975 read GetItem write SetItem;
  end;


 ///REGISTRO 1980: GIAF 4 GUIA DE INFORMAÇÃO E APURAÇÃO DE INCENTIVOS FISCAIS E FINANCEIROS: CENTRAL DE DISTRIBUIÇÃO (ENTRADAS/SAÍDAS)
  TRegistro1980 = class
  private
    fIND_AP: String;   ///Indicador da sub-apuração por tipo de benefício(conforme tabela 4.7.1)
    fG4_01: Currency;  /// Entradas (percentual de incentivo)
    fG4_02: Currency;  /// Entradas não incentivadas de PI
    fG4_03: Currency;  /// Entradas incentivadas de PI
    fG4_04: Currency;  /// Saídas (percentual de incentivo)
    fG4_05: Currency;  /// Saídas não incentivadas de PI
    fG4_06: Currency;  /// Saídas incentivadas de PI
    fG4_07: Currency;  /// Saldo devedor do ICMS antes das deduções do incentivo (PI e itens não incentivados)
    fG4_08: Currency;  /// Crédito presumido nas entradas incentivadas de PI
    fG4_09: Currency;  /// Crédito presumido nas saídas incentivadas de PI
    fG4_10: Currency;  /// Dedução de incentivo da Central de Distribuição (entradas/saídas)
    fG4_11: Currency;  /// Saldo devedor do ICMS após deduções do incentivo
    fG4_12: Currency;  /// ndice de recolhimento da central de distribuição
  public
    constructor Create(AOwner: TRegistro1001); virtual; /// Create
    property IND_AP : String    read fIND_AP write fIND_AP;
    property G4_01  : Currency  read fG4_01  write fG4_01;
    property G4_02  : Currency  read fG4_02  write fG4_02;
    property G4_03  : Currency  read fG4_03  write fG4_03;
    property G4_04  : Currency  read fG4_04  write fG4_04;
    property G4_05  : Currency  read fG4_05  write fG4_05;
    property G4_06  : Currency  read fG4_06  write fG4_06;
    property G4_07  : Currency  read fG4_07  write fG4_07;
    property G4_08  : Currency  read fG4_08  write fG4_08;
    property G4_09  : Currency  read fG4_09  write fG4_09;
    property G4_10  : Currency  read fG4_10  write fG4_10;
    property G4_11  : Currency  read fG4_11  write fG4_11;
    property G4_12  : Currency  read fG4_12  write fG4_12;
  end;

  TRegistro1980List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1980;
    procedure SetItem(Index: Integer; const Value: TRegistro1980);
  public
    function New(AOwner: TRegistro1001): TRegistro1980;
    property Items[Index: Integer]: TRegistro1980 read GetItem write SetItem;
  end;

  /// Registro 1990 - ENCERRAMENTO DO BLOCO 1

  TRegistro1990 = class
  private
    fQTD_LIN_1: Integer; /// Quantidade total de linhas do Bloco 1
  public
    property QTD_LIN_1: Integer read fQTD_LIN_1 write fQTD_LIN_1;
  end;

implementation

{ TRegistro1100List }

function TRegistro1100List.GetItem(Index: Integer): TRegistro1100;
begin
  Result := TRegistro1100(Inherited Items[Index]);
end;

function TRegistro1100List.New(): TRegistro1100;
begin
  Result := TRegistro1100.Create();
  Add(Result);
end;

procedure TRegistro1100List.SetItem(Index: Integer; const Value: TRegistro1100);
begin
  Put(Index, Value);
end;

{ TRegistro1010List }

function TRegistro1010List.GetItem(Index: Integer): TRegistro1010;
begin
  Result := TRegistro1010(Inherited Items[Index]);
end;

function TRegistro1010List.New(AOwner: TRegistro1001): TRegistro1010;
begin
  Result := TRegistro1010.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1010List.SetItem(Index: Integer; const Value: TRegistro1010);
begin
  Put(Index, Value);
end;


{ TRegistro1105List }

function TRegistro1105List.GetItem(Index: Integer): TRegistro1105;
begin
  Result := TRegistro1105(Inherited Items[Index]);
end;

function TRegistro1105List.New(): TRegistro1105;
begin
  Result := TRegistro1105.Create();
  Add(Result);
end;

procedure TRegistro1105List.SetItem(Index: Integer; const Value: TRegistro1105);
begin
  Put(Index, Value);
end;

{ TRegistro1110List }

function TRegistro1110List.GetItem(Index: Integer): TRegistro1110;
begin
  Result := TRegistro1110(Inherited Items[Index]);
end;

function TRegistro1110List.New(AOwner: TRegistro1105): TRegistro1110;
begin
  Result := TRegistro1110.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1110List.SetItem(Index: Integer; const Value: TRegistro1110);
begin
  Put(Index, Value);
end;

{ TRegistro1200List }

function TRegistro1200List.GetItem(Index: Integer): TRegistro1200;
begin
  Result := TRegistro1200(Inherited Items[Index]);
end;

function TRegistro1200List.New(): TRegistro1200;
begin
  Result := TRegistro1200.Create();
  Add(Result);
end;

procedure TRegistro1200List.SetItem(Index: Integer; const Value: TRegistro1200);
begin
  Put(Index, Value);
end;

{ TRegistro1210List }

function TRegistro1210List.GetItem(Index: Integer): TRegistro1210;
begin
  Result := TRegistro1210(Inherited Items[Index]);
end;

function TRegistro1210List.New(AOwner: TRegistro1200): TRegistro1210;
begin
  Result := TRegistro1210.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1210List.SetItem(Index: Integer; const Value: TRegistro1210);
begin
  Put(Index, Value);
end;

{ TRegistro1300 }

constructor TRegistro1300.Create();
begin
  inherited Create;
  FRegistro1310 := TRegistro1310List.Create;  /// BLOCO 1 - Lista de Registro1310 (FILHO)
end;

destructor TRegistro1300.Destroy;
begin
  FRegistro1310.Free;
  inherited;
end;

{ TRegistro1300List }

function TRegistro1300List.GetItem(Index: Integer): TRegistro1300;
begin
  Result := TRegistro1300(Inherited Items[Index]);
end;

function TRegistro1300List.New(): TRegistro1300;
begin
  Result := TRegistro1300.Create();
  Add(Result);
end;

procedure TRegistro1300List.SetItem(Index: Integer; const Value: TRegistro1300);
begin
  Put(Index, Value);
end;

{ TRegistro1310 }

constructor TRegistro1310.Create();
begin
  inherited Create;
  FRegistro1320 := TRegistro1320List.Create;  /// BLOCO 1 - Lista de Registro1320 (FILHO)
end;

destructor TRegistro1310.Destroy;
begin
  FRegistro1320.Free;
  inherited;
end;

{ TRegistro1310List }

function TRegistro1310List.GetItem(Index: Integer): TRegistro1310;
begin
  Result := TRegistro1310(Inherited Items[Index]);
end;

function TRegistro1310List.New(): TRegistro1310;
begin
  Result := TRegistro1310.Create();
  Add(Result);
end;

procedure TRegistro1310List.SetItem(Index: Integer; const Value: TRegistro1310);
begin
  Put(Index, Value);
end;

{ TRegistro1320List }

function TRegistro1320List.GetItem(Index: Integer): TRegistro1320;
begin
  Result := TRegistro1320(Inherited Items[Index]);
end;

function TRegistro1320List.New(AOwner: TRegistro1310): TRegistro1320;
begin
  Result := TRegistro1320.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1320List.SetItem(Index: Integer; const Value: TRegistro1320);
begin
  Put(Index, Value);
end;
{ TRegistro1390List }

function TRegistro1390List.GetItem(Index: Integer): TRegistro1390;
begin
  Result := TRegistro1390(Inherited Items[Index]);
end;

function TRegistro1390List.New(): TRegistro1390;
begin
  Result := TRegistro1390.Create();
  Add(Result);
end;

procedure TRegistro1390List.SetItem(Index: Integer; const Value: TRegistro1390);
begin
  Put(Index, Value);
end;

{ TRegistro1400List }

function TRegistro1400List.GetItem(Index: Integer): TRegistro1400;
begin
  Result := TRegistro1400(Inherited Items[Index]);
end;

function TRegistro1400List.New(AOwner: TRegistro1001): TRegistro1400;
begin
  Result := TRegistro1400.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1400List.SetItem(Index: Integer; const Value: TRegistro1400);
begin
  Put(Index, Value);
end;

{ TRegistro1500List }

function TRegistro1500List.GetItem(Index: Integer): TRegistro1500;
begin
  Result := TRegistro1500(Inherited Items[Index]);
end;

function TRegistro1500List.New(): TRegistro1500;
begin
  Result := TRegistro1500.Create();
  Add(Result);
end;

procedure TRegistro1500List.SetItem(Index: Integer; const Value: TRegistro1500);
begin
  Put(Index, Value);
end;

{ TRegistro1510List }

function TRegistro1510List.GetItem(Index: Integer): TRegistro1510;
begin
  Result := TRegistro1510(Inherited Items[Index]);
end;

function TRegistro1510List.New(AOwner: TRegistro1500): TRegistro1510;
begin
  Result := TRegistro1510.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1510List.SetItem(Index: Integer; const Value: TRegistro1510);
begin
  Put(Index, Value);
end;

{ TRegistro1350List }

function TRegistro1350List.GetItem(Index: Integer): TRegistro1350;
begin
    Result := TRegistro1350(Inherited Items[Index]);
end;

function TRegistro1350List.New(): TRegistro1350;
begin
  Result := TRegistro1350.Create();
  Add(Result);
end;

procedure TRegistro1350List.SetItem(Index: Integer; const Value: TRegistro1350);
begin
  Put(Index, Value);
end;

{ TRegistro1360List }

function TRegistro1360List.GetItem(Index: Integer): TRegistro1360;
begin
    Result := TRegistro1360(Inherited Items[Index]);
end;

function TRegistro1360List.New(AOwner: TRegistro1350): TRegistro1360;
begin
  Result := TRegistro1360.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1360List.SetItem(Index: Integer; const Value: TRegistro1360);
begin
  Put(Index, Value);
end;

{ TRegistro1370List }

function TRegistro1370List.GetItem(Index: Integer): TRegistro1370;
begin
    Result := TRegistro1370(Inherited Items[Index]);
end;

function TRegistro1370List.New(AOwner: TRegistro1350): TRegistro1370;
begin
  Result := TRegistro1370.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1370List.SetItem(Index: Integer; const Value: TRegistro1370);
begin
  Put(Index, Value);
end;

{ TRegistro1600List }

function TRegistro1600List.GetItem(Index: Integer): TRegistro1600;
begin
  Result := TRegistro1600(Inherited Items[Index]);
end;

function TRegistro1600List.New(AOwner: TRegistro1001): TRegistro1600;
begin
  Result := TRegistro1600.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1600List.SetItem(Index: Integer; const Value: TRegistro1600);
begin
  Put(Index, Value);
end;

{ TRegistro1601List }

function TRegistro1601List.GetItem(Index: Integer): TRegistro1601;
begin
  Result := TRegistro1601(Inherited Items[Index]);
end;

function TRegistro1601List.New(AOwner: TRegistro1001): TRegistro1601;
begin
  Result := TRegistro1601.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1601List.SetItem(Index: Integer; const Value: TRegistro1601);
begin
  Put(Index, Value);
end;

{ TRegistro1350 }

constructor TRegistro1350.Create();
begin
  inherited Create;
  FRegistro1360 := TRegistro1360List.Create;  /// BLOCO 1 - Lista de Registro1360 (FILHO)
  FRegistro1370 := TRegistro1370List.Create;  /// BLOCO 1 - Lista de Registro1370 (FILHO)
end;

destructor TRegistro1350.Destroy;
begin
  FRegistro1360.Free;
  FRegistro1370.Free;
  inherited;
end;

{ TRegistro1700 }

constructor TRegistro1700.Create();
begin
  inherited Create;
  FRegistro1710 := TRegistro1710List.Create;  /// BLOCO 1 - Lista de Registro1710 (FILHO)
end;

destructor TRegistro1700.Destroy;
begin
  FRegistro1710.Free;
  inherited;
end;

{ TRegistro1700List }

function TRegistro1700List.GetItem(Index: Integer): TRegistro1700;
begin
  Result := TRegistro1700(Inherited Items[Index]);
end;

function TRegistro1700List.New(): TRegistro1700;
begin
  Result := TRegistro1700.Create();
  Add(Result);
end;

procedure TRegistro1700List.SetItem(Index: Integer; const Value: TRegistro1700);
begin
  Put(Index, Value);
end;

{ TRegistro1710 }

function TRegistro1710List.GetItem(Index: Integer): TRegistro1710;
begin
  Result := TRegistro1710(Inherited Items[Index]);
end;

function TRegistro1710List.New(AOwner: TRegistro1700): TRegistro1710;
begin
  Result := TRegistro1710.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1710List.SetItem(Index: Integer; const Value: TRegistro1710);
begin
  Put(Index, Value);
end;

{ TRegistro1710 }

constructor TRegistro1710.Create(AOwner: TRegistro1700);
begin
end;

{ TRegistro1800List }

function TRegistro1800List.GetItem(Index: Integer): TRegistro1800;
begin
  Result := TRegistro1800(Inherited Items[Index]);
end;

function TRegistro1800List.New(AOwner: TRegistro1001): TRegistro1800;
begin
  Result := TRegistro1800.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1800List.SetItem(Index: Integer; const Value: TRegistro1800);
begin
  Put(Index, Value);
end;

{ TRegistro1001 }

constructor TRegistro1001.Create;
begin
   inherited Create;
   FRegistro1010 := TRegistro1010List.Create;
   FRegistro1100 := TRegistro1100List.Create;
   FRegistro1200 := TRegistro1200List.Create;
   FRegistro1250 := TRegistro1250List.Create;
   FRegistro1300 := TRegistro1300List.Create;
   FRegistro1350 := TRegistro1350List.Create;
   FRegistro1390 := TRegistro1390List.Create;
   FRegistro1400 := TRegistro1400List.Create;
   FRegistro1500 := TRegistro1500List.Create;
   FRegistro1600 := TRegistro1600List.Create;
   FRegistro1601 := TRegistro1601List.Create;
   FRegistro1700 := TRegistro1700List.Create;
   FRegistro1800 := TRegistro1800List.Create;
   FRegistro1900 := TRegistro1900List.Create;
   FRegistro1960 := TRegistro1960List.Create;
   FRegistro1970 := TRegistro1970List.Create;
   FRegistro1980 := TRegistro1980List.Create;
   //
   IND_MOV := imSemDados;
end;

destructor TRegistro1001.Destroy;
begin
  FRegistro1010.Free;
  FRegistro1100.Free;
  FRegistro1200.Free;
  FRegistro1250.Free;
  FRegistro1300.Free;
  FRegistro1350.Free;
  FRegistro1390.Free;
  FRegistro1400.Free;
  FRegistro1500.Free;
  FRegistro1600.Free;
  FRegistro1601.Free;
  FRegistro1700.Free;
  FRegistro1800.Free;
  FRegistro1900.Free;
  FRegistro1960.Free;
  FRegistro1970.Free;
  FRegistro1980.Free;
  inherited;
end;

{ TRegistro1100 }

constructor TRegistro1100.Create();
begin
   inherited Create;
   fRegistro1105 := TRegistro1105List.Create;
end;

destructor TRegistro1100.Destroy;
begin
  fRegistro1105.Free;
  inherited;
end;

{ TRegistro1010 }

constructor TRegistro1010.Create(AOwner: TRegistro1001);
begin
end;

{ TRegistro1105 }

constructor TRegistro1105.Create();
begin
   inherited Create;
   fRegistro1110 := TRegistro1110List.Create;
end;

destructor TRegistro1105.Destroy;
begin
  fRegistro1110.Free;
  inherited;
end;

{ TRegistro1200 }

constructor TRegistro1200.Create();
begin
   inherited Create;
   FRegistro1210 := TRegistro1210List.Create;
end;

destructor TRegistro1200.Destroy;
begin
  FRegistro1210.Free;
  inherited;
end;

{ TRegistro1500 }

constructor TRegistro1500.Create();
begin
   inherited Create;
   FRegistro1510 := TRegistro1510List.Create;
end;

destructor TRegistro1500.Destroy;
begin
  FRegistro1510.Free;
  inherited;
end;

{ TRegistro1110 }

constructor TRegistro1110.Create(AOwner: TRegistro1105);
begin
end;

{ TRegistro1210 }

constructor TRegistro1210.Create(AOwner: TRegistro1200);
begin
end;

{ TRegistro1320 }

constructor TRegistro1320.Create(AOwner: TRegistro1310);
begin
end;

{ TRegistro1360 }

constructor TRegistro1360.Create(AOwner: TRegistro1350);
begin
end;

{ TRegistro1370 }

constructor TRegistro1370.Create(AOwner: TRegistro1350);
begin
end;

{ TRegistro1390 }

constructor TRegistro1390.Create();
begin
  inherited Create;
  fRegistro1391 := TRegistro1391List.Create;
end;

destructor TRegistro1390.Destroy;
begin
  FRegistro1391.Free;
  inherited;
end;

{ TRegistro1391 }

constructor TRegistro1391.Create(AOwner: TRegistro1390);
begin
end;

{ TRegistro1400 }

constructor TRegistro1400.Create(AOwner: TRegistro1001);
begin
end;

{ TRegistro1510 }

constructor TRegistro1510.Create(AOwner: TRegistro1500);
begin
end;

{ TRegistro1600 }

constructor TRegistro1600.Create(AOwner: TRegistro1001);
begin
end;

{ TRegistro1601 }

constructor TRegistro1601.Create(AOwner: TRegistro1001);
begin
end;

{ TRegistro1800 }

constructor TRegistro1800.Create(AOwner: TRegistro1001);
begin
end;

{ TRegistro1391List }

function TRegistro1391List.GetItem(Index: Integer): TRegistro1391;
begin
  Result := TRegistro1391(Inherited Items[Index]);
end;

function TRegistro1391List.New(AOwner: TRegistro1390): TRegistro1391;
begin
  Result := TRegistro1391.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1391List.SetItem(Index: Integer;
  const Value: TRegistro1391);
begin
  Put(Index, Value);
end;

{ TRegistro1900 }

constructor TRegistro1900.Create();
begin
  inherited Create;
  FRegistro1910 := TRegistro1910List.Create;  /// BLOCO 1 - Lista de Registro1910 (FILHO)
end;

destructor TRegistro1900.Destroy;
begin
  FRegistro1910.Free;
  inherited;
end;


{ TRegistro1970 }

constructor TRegistro1970.Create();
begin
  inherited Create;
  FRegistro1975 := TRegistro1975List.Create;  /// BLOCO 1 - Lista de Registro1975 (FILHO)
end;

destructor TRegistro1970.Destroy;
begin
  FRegistro1975.Free;
  inherited;
end;

{ TRegistro1900List }

function TRegistro1900List.GetItem(Index: Integer): TRegistro1900;
begin
  Result := TRegistro1900(Inherited Items[Index]);
end;

function TRegistro1900List.New(): TRegistro1900;
begin
  Result := TRegistro1900.Create();
  Add(Result);
end;

procedure TRegistro1900List.SetItem(Index: Integer;
  const Value: TRegistro1900);
begin
  Put(Index, Value);
end;

{ TRegistro1910 }

constructor TRegistro1910.Create();
begin
  inherited Create;
  FRegistro1920 := TRegistro1920List.Create;
end;

destructor TRegistro1910.Destroy;
begin
  FRegistro1920.Free;
  inherited;
end;

{ TRegistro1910List }

function TRegistro1910List.GetItem(Index: Integer): TRegistro1910;
begin
  Result := TRegistro1910(Inherited Items[Index]);
end;

function TRegistro1910List.New(): TRegistro1910;
begin
  Result := TRegistro1910.Create();
  Add(Result);
end;

procedure TRegistro1910List.SetItem(Index: Integer;
  const Value: TRegistro1910);
begin
  Put(Index, Value);
end;

{ TRegistro1920 }

constructor TRegistro1920.Create();
begin
  inherited Create;
  FRegistro1921 := TRegistro1921List.Create;
  FRegistro1925 := TRegistro1925List.Create;
  FRegistro1926 := TRegistro1926List.Create;
end;

destructor TRegistro1920.Destroy;
begin
  FRegistro1921.Free;
  FRegistro1925.Free;
  FRegistro1926.Free;
  inherited;
end;

{ TRegistro1920List }

function TRegistro1920List.GetItem(Index: Integer): TRegistro1920;
begin
  Result := TRegistro1920(Inherited Items[Index]);
end;

function TRegistro1920List.New(): TRegistro1920;
begin
  Result := TRegistro1920.Create();
  Add(Result);
end;

procedure TRegistro1920List.SetItem(Index: Integer;
  const Value: TRegistro1920);
begin
  Put(Index, Value);
end;

{ TRegistro1921 }

constructor TRegistro1921.Create();
begin
  inherited Create;
  FRegistro1922 := TRegistro1922List.Create;
  FRegistro1923 := TRegistro1923List.Create;
end;

destructor TRegistro1921.Destroy;
begin
  FRegistro1922.Free;
  FRegistro1923.Free;
  inherited;
end;

{ TRegistro1921List }

function TRegistro1921List.GetItem(Index: Integer): TRegistro1921;
begin
  Result := TRegistro1921(Inherited Items[Index]);
end;

function TRegistro1921List.New(): TRegistro1921;
begin
  Result := TRegistro1921.Create();
  Add(Result);
end;

procedure TRegistro1921List.SetItem(Index: Integer;
  const Value: TRegistro1921);
begin
  Put(Index, Value);
end;

{ TRegistro1922List }

function TRegistro1922List.GetItem(Index: Integer): TRegistro1922;
begin
  Result := TRegistro1922(Inherited Items[Index]);
end;

function TRegistro1922List.New: TRegistro1922;
begin
  Result := TRegistro1922.Create;
  Add(Result);
end;

procedure TRegistro1922List.SetItem(Index: Integer;
  const Value: TRegistro1922);
begin
  Put(Index, Value);
end;

{ TRegistro1923List }

function TRegistro1923List.GetItem(Index: Integer): TRegistro1923;
begin
  Result := TRegistro1923(Inherited Items[Index]);
end;

function TRegistro1923List.New: TRegistro1923;
begin
  Result := TRegistro1923.Create;
  Add(Result);
end;

procedure TRegistro1923List.SetItem(Index: Integer;
  const Value: TRegistro1923);
begin
  Put(Index, Value);
end;

{ TRegistro1925List }

function TRegistro1925List.GetItem(Index: Integer): TRegistro1925;
begin
  Result := TRegistro1925(Inherited Items[Index]);
end;

function TRegistro1925List.New: TRegistro1925;
begin
  Result := TRegistro1925.Create;
  Add(Result);
end;

procedure TRegistro1925List.SetItem(Index: Integer;
  const Value: TRegistro1925);
begin
  Put(Index, Value);
end;

{ TRegistro1926List }

function TRegistro1926List.GetItem(Index: Integer): TRegistro1926;
begin
  Result := TRegistro1926(Inherited Items[Index]);
end;

function TRegistro1926List.New: TRegistro1926;
begin
  Result := TRegistro1926.Create;
  Add(Result);
end;

procedure TRegistro1926List.SetItem(Index: Integer;
  const Value: TRegistro1926);
begin
  Put(Index, Value);
end;

{ TRegistro1960List }

function TRegistro1960List.GetItem(Index: Integer): TRegistro1960;
begin
  Result := TRegistro1960(Inherited Items[Index]);
end;

function TRegistro1960List.New(AOwner: TRegistro1001): TRegistro1960;
begin
  Result := TRegistro1960.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1960List.SetItem(Index: Integer; const Value: TRegistro1960);
begin
  Put(Index, Value);
end;

{ TRegistro1970List }

function TRegistro1970List.GetItem(Index: Integer): TRegistro1970;
begin
  Result := TRegistro1970(Inherited Items[Index]);
end;

function TRegistro1970List.New(): TRegistro1970;
begin
  Result := TRegistro1970.Create();
  Add(Result);
end;

procedure TRegistro1970List.SetItem(Index: Integer; const Value: TRegistro1970);
begin
  Put(Index, Value);
end;

{ TRegistro1975List }

function TRegistro1975List.GetItem(Index: Integer): TRegistro1975;
begin
  Result := TRegistro1975(Inherited Items[Index]);
end;

function TRegistro1975List.New(): TRegistro1975;
begin
  Result := TRegistro1975.Create;
  Add(Result);
end;

procedure TRegistro1975List.SetItem(Index: Integer; const Value: TRegistro1975);
begin
  Put(Index, Value);
end;

{ TRegistro1980List }

function TRegistro1980List.GetItem(Index: Integer): TRegistro1980;
begin
  Result := TRegistro1980(Inherited Items[Index]);
end;

function TRegistro1980List.New(AOwner: TRegistro1001): TRegistro1980;
begin
  Result := TRegistro1980.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1980List.SetItem(Index: Integer; const Value: TRegistro1980);
begin
  Put(Index, Value);
end;

{ TRegistro1960 }

constructor TRegistro1960.Create(AOwner: TRegistro1001);
begin

end;

{ TRegistro1980 }

constructor TRegistro1980.Create(AOwner: TRegistro1001);
begin

end;

{ TRegistro1250List }

function TRegistro1250List.GetItem(Index: Integer): TRegistro1250;
begin
  Result := TRegistro1250(Inherited Items[Index]);
end;

function TRegistro1250List.New: TRegistro1250;
begin
  Result := TRegistro1250.Create();
  Add(Result)
end;

procedure TRegistro1250List.SetItem(Index: Integer; const Value: TRegistro1250);
begin
  Put(Index, Value);
end;

{ TRegistro1250 }

constructor TRegistro1250.Create;
begin
   inherited Create;
   FRegistro1255 := TRegistro1255List.Create;
end;

destructor TRegistro1250.Destroy;
begin
  FRegistro1255.Free;
  inherited;
end;

{ TRegistro1255List }

function TRegistro1255List.GetItem(Index: Integer): TRegistro1255;
begin
  Result := TRegistro1255(Inherited Items[Index]);
end;

function TRegistro1255List.New(AOwner: TRegistro1250): TRegistro1255;
begin
  Result := TRegistro1255.Create(AOwner);
  Add(Result);
end;

procedure TRegistro1255List.SetItem(Index: Integer; const Value: TRegistro1255);
begin
  Put(Index, Value);
end;

{ TRegistro1255 }

constructor TRegistro1255.Create(AOwner: TRegistro1250);
begin
end;

end.
