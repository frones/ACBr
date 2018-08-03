{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrEFDBloco_D;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEFDBlocos, ACBrUtil;

type
  TRegistroD100List = class;

  TRegistroD101List = class; 

  TRegistroD110List = class;
  TRegistroD120List = class;
  TRegistroD130List = class;
  TRegistroD140List = class;
  TRegistroD150List = class;
  TRegistroD160List = class;
  TRegistroD161List = class;
  TRegistroD162List = class;
  TRegistroD170List = class;
  TRegistroD180List = class;
  TRegistroD190List = class; 
  TRegistroD195List = class; 
  TRegistroD197List = class; 
  TRegistroD300List = class;
  TRegistroD301List = class;
  TRegistroD310List = class;
  TRegistroD350List = class;
  TRegistroD355List = class;
  TRegistroD360List = class;
  TRegistroD365List = class;
  TRegistroD370List = class;
  TRegistroD390List = class;
  TRegistroD400List = class;
  TRegistroD410List = class;
  TRegistroD411List = class;
  TRegistroD420List = class;
  TRegistroD500List = class;
  TRegistroD510List = class;
  TRegistroD530List = class;
  TRegistroD590List = class; 
  TRegistroD600List = class;
  TRegistroD610List = class;
  TRegistroD690List = class;
  TRegistroD695List = class;
  TRegistroD696List = class;
  TRegistroD697List = class;

  /// Registro D001 - ABERTURA DO BLOCO D

  TRegistroD001 = class(TOpenBlocos)
  private
    FRegistroD100: TRegistroD100List;
    FRegistroD300: TRegistroD300List;
    FRegistroD350: TRegistroD350List;
    FRegistroD400: TRegistroD400List;
    FRegistroD500: TRegistroD500List;
    FRegistroD600: TRegistroD600List;
    FRegistroD695: TRegistroD695List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroD100: TRegistroD100List read FRegistroD100 write FRegistroD100;
    property RegistroD300: TRegistroD300List read FRegistroD300 write FRegistroD300;
    property RegistroD350: TRegistroD350List read FRegistroD350 write FRegistroD350;
    property RegistroD400: TRegistroD400List read FRegistroD400 write FRegistroD400;
    property RegistroD500: TRegistroD500List read FRegistroD500 write FRegistroD500;
    property RegistroD600: TRegistroD600List read FRegistroD600 write FRegistroD600;
    property RegistroD695: TRegistroD695List read FRegistroD695 write FRegistroD695;
  end;

  /// Registro D100 - NOTA FISCAL DE SERVIÇO DE TRANSPORTE (CÓDIGO 07) E CONHECIMENTOS DE TRANSPORTE RODOVIÁRIO DE CARGAS (CÓDIGO 08), AQUAVIÁRIO DE CARGAS (CÓDIGO 09), AÉREO (CÓDIGO 10), FERROVIÁRIO DE CARGAS (CÓDIGO 11) E MULTIMODAL DE CARGAS (CÓDIGO 26) E NOTA FISCAL DE TRANSPORTE FERROVIÁRIO DE CARGA (CÓDIGO 27)

  TRegistroD100 = class
  private
    fIND_OPER: TACBrIndOper;        /// Indicador do tipo de operação: 0- Aquisição; 1- Prestação
    fIND_EMIT: TACBrIndEmit;        /// Indicador do emitente do documento fiscal: 0- Emissão própria; 1- Terceiros
    fCOD_PART: String;              /// Código do participante (campo 02 do Registro 0150):
    fCOD_MOD: String;               /// Código do modelo do documento fiscal, conforme a Tabela 4.1.1
    fCOD_SIT: TACBrCodSit;          /// Código da situação do documento fiscal, conforme a Tabela 4.1.2
    fSER: String;                   /// Série do documento fiscal
    fSUB: String;                   /// Subsérie do documento fiscal
    fNUM_DOC: String;               /// Número do documento fiscal
    fCHV_CTE: String;               /// Chave da Conhecimento Eletrônico
    fDT_DOC: TDateTime;             /// Data da emissão do documento fiscal
    fDT_A_P: TDateTime;             /// Data da aquisição ou da prestaçãodo serviço
    fTP_CT_e: String;               /// Tipo de conhecimento conforme definido no manual de integração do CT-e
    fCHV_CTE_REF: String;           /// Chave do CT-e de referencia cujos valores foram complementados: 1 ou 2
    fVL_DOC: currency;              /// Valor total do documento fiscal
    fVL_DESC: currency;             /// Valor total do desconto
    fIND_FRT: TACBrIndFrt;          /// Indicador do tipo do frete:
    fVL_SERV: currency;             /// Valor do frete indicado no documento fiscal
    fVL_BC_ICMS: currency;          /// Valor da base de cálculo do ICMS
    fVL_ICMS: currency;             /// Valor do ICMS
    fVL_NT: currency;               /// Valor não tributado
    fCOD_INF: String;               /// Valor do ICMS retido por substituição tributária
    fCOD_CTA: String;               /// Código da conta analitica contabil debitada/creditada
    fCOD_MUN_ORIG: String;          /// Código municipio origem conf. tab IBGE
    fCOD_MUN_DEST: String;          /// Código municipio destino conf. tab IBGE

    FRegistroD101: TRegistroD101List;
    FRegistroD110: TRegistroD110List;
    FRegistroD130: TRegistroD130List;
    FRegistroD140: TRegistroD140List;
    FRegistroD150: TRegistroD150List;
    FRegistroD160: TRegistroD160List;
    FRegistroD170: TRegistroD170List;
    FRegistroD180: TRegistroD180List;
    FRegistroD190: TRegistroD190List; /// BLOCO D - Lista de RegistroD190 (FILHO)
    FRegistroD195: TRegistroD195List;
  public
    constructor Create(AOwner: TRegistroD001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_OPER: TACBrIndOper read FIND_OPER write FIND_OPER;
    property IND_EMIT: TACBrIndEmit read FIND_EMIT write FIND_EMIT;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property COD_SIT: TACBrCodSit read FCOD_SIT write FCOD_SIT;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property CHV_CTE: String read FCHV_CTE write FCHV_CTE;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property DT_A_P: TDateTime read FDT_A_P write FDT_A_P;
    property TP_CT_e: String read FTP_CT_e write FTP_CT_e;
    property CHV_CTE_REF: String read FCHV_CTE_REF write FCHV_CTE_REF;
    property VL_DOC: currency read FVL_DOC write FVL_DOC;
    property VL_DESC: currency read FVL_DESC write FVL_DESC;
    property IND_FRT: TACBrIndFrt read FIND_FRT write FIND_FRT;
    property VL_SERV: currency read FVL_SERV write FVL_SERV;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
    property VL_NT: currency read FVL_NT write FVL_NT;
    property COD_INF: String read FCOD_INF write FCOD_INF;
    property COD_CTA: String read FCOD_CTA write FCOD_CTA;
    property COD_MUN_ORIG: String read FCOD_MUN_ORIG write FCOD_MUN_ORIG;
    property COD_MUN_DEST: String read FCOD_MUN_DEST write FCOD_MUN_DEST;

    property RegistroD101: TRegistroD101List read FRegistroD101 write FRegistroD101;
    property RegistroD110: TRegistroD110List read FRegistroD110 write FRegistroD110;
    property RegistroD130: TRegistroD130List read FRegistroD130 write FRegistroD130;
    property RegistroD140: TRegistroD140List read FRegistroD140 write FRegistroD140;
    property RegistroD150: TRegistroD150List read FRegistroD150 write FRegistroD150;
    property RegistroD160: TRegistroD160List read FRegistroD160 write FRegistroD160;
    property RegistroD170: TRegistroD170List read FRegistroD170 write FRegistroD170;
    property RegistroD180: TRegistroD180List read FRegistroD180 write FRegistroD180;
    property RegistroD190: TRegistroD190List read FRegistroD190 write FRegistroD190; 
    property RegistroD195: TRegistroD195List read FRegistroD195 write FRegistroD195;
  end;

  /// Registro D100 - Lista

  TRegistroD100List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD100; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD100); /// SetItem
  public
    function New(AOwner: TRegistroD001): TRegistroD100;
    property Items[Index: Integer]: TRegistroD100 read GetItem write SetItem;
  end;

  /// Registro D101 - EC 87/2015 - INFORMACAO COMPLEMENTAR DE OPERACOES INTERESTADUAIS

  TRegistroD101 = class
  private
    fVL_FCP_UF_DEST : currency;                  /// VALOR TOTAL FUNDO DE COMBATE A POBREZA
    fVL_ICMS_UF_DEST: currency;                  /// VALOR TOTAL DO ICMS DA UF DE DESTINO
    fVL_ICMS_UF_REM: currency;                   /// VALOR TOTAL DO ICMS DA UF DE ORIGEM
  public
    property VL_FCP_UF_DEST: currency read fVL_FCP_UF_DEST write fVL_FCP_UF_DEST;
    property VL_ICMS_UF_DEST: currency read fVL_ICMS_UF_DEST write fVL_ICMS_UF_DEST;
    property VL_ICMS_UF_REM: currency read fVL_ICMS_UF_REM write fVL_ICMS_UF_REM;
  end;

  /// Registro D101 - Lista

  TRegistroD101List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD101; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD101); /// SetItem
  public
    function New: TRegistroD101;
    property Items[Index: Integer]: TRegistroD101 read GetItem write SetItem;
  end;
  
  /// Registro D110 - COMPLEMENTO DOS BILHETES (CÓDIGO 13, CÓDIGO 14 E CÓDIGO 16)

  TRegistroD110 = class
  private
    fCOD_ITEM: string;
    fMUN_ITEM: integer;
    fVL_SERV: currency;
    fVL_OUT: currency;

    FRegistroD120: TRegistroD120List;
  public
    constructor Create(AOwner: TRegistroD100); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property NUN_ITEM: integer read fMUN_ITEM write fMUN_ITEM;
    property COD_ITEM: string read fCOD_ITEM write fCOD_ITEM;
    property VL_SERV: currency read fVL_SERV write fVL_SERV;
    property VL_OUT: currency read fVL_OUT write fVL_OUT;

    property RegistroD120: TRegistroD120List read FRegistroD120 write FRegistroD120;
  end;

  /// Registro D110 - Lista

  TRegistroD110List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD110; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD110); /// SetItem
  public
    function New(AOwner: TRegistroD100): TRegistroD110;
    property Items[Index: Integer]: TRegistroD110 read GetItem write SetItem;
  end;

  /// Registro D120 - COMPLEMENTO DA NOTA FISCAL DE SERVIÇOS DE TRANSPORTE (CÓDIGO 07)

  TRegistroD120 = class
  private
     fCOD_MUN_ORIG:String; // Código do município de origem do serviço, conforme a tabela IBGE
     fCOD_MUN_DEST:String; // Código do município de destino, conforme a tabela IBGE
     fVEIC_ID     :String; // Placa de identificação do veículo
     fUF_ID       :String;  // Sigla da UF da placa do veículo
  public
    property COD_MUN_ORIG :String read fCOD_MUN_ORIG write fCOD_MUN_ORIG;
    property COD_MUN_DEST :String read fCOD_MUN_DEST write fCOD_MUN_DEST;
    property VEIC_ID      :String read fVEIC_ID      write fVEIC_ID     ;
    property UF_ID        :String read fUF_ID        write fUF_ID       ;
  end;

  /// Registro D120 - Lista

  TRegistroD120List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD120; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD120); /// SetItem
  public
    function New: TRegistroD120;
    property Items[Index: Integer]: TRegistroD120 read GetItem write SetItem;
  end;

  /// Registro D130 - COMPLEMENTO DO CONHECIMENTO RODOVIÁRIO DE CARGAS (CÓDIGO 08)

  TRegistroD130 = class
  private
    fCOD_PART_CONSG: String;                /// Código do participante (campo 02 do Registro 0150):
    fCOD_PART_RED: String;                  /// Código do participante (campo 02 do Registro 0150):
    fIND_FRT_RED: TACBrTipoFreteRedespacho; /// Indicador do tipo do frete da operação de redespacho:
    fCOD_MUN_ORIG: String;                  /// Código do município de origem do serviço, conforme a tabela IBGE
    fCOD_MUN_DEST: String;                  /// Código do município de destino, conforme a tabela IBGE
    fVEIC_ID: String;                       /// Placa de identificação do veículo
    fVL_LIQ_FRT: currency;                  /// Valor líquido do frete
    fVL_SEC_CAT: currency;                  /// Soma de valores de Sec/Cat (serviços de coleta/custo adicional de transporte)
    fVL_DESP: currency;                     /// Soma de valores de despacho
    fVL_PEDG: currency;                     /// Soma dos valores de pedágio
    fVL_OUT: currency;                      /// Outros valores
    fVL_FRT: currency;                      /// Valor total do frete
    fUF_ID: String;                         /// Sigla da UF da placa do veículo
  public
    property COD_PART_CONSG: String read FCOD_PART_CONSG write FCOD_PART_CONSG;
    property COD_PART_RED: String read FCOD_PART_RED write FCOD_PART_RED;
    property IND_FRT_RED: TACBrTipoFreteRedespacho read FIND_FRT_RED write FIND_FRT_RED;
    property COD_MUN_ORIG: String read FCOD_MUN_ORIG write FCOD_MUN_ORIG;
    property COD_MUN_DEST: String read FCOD_MUN_DEST write FCOD_MUN_DEST;
    property VEIC_ID: String read FVEIC_ID write FVEIC_ID;
    property VL_LIQ_FRT: currency read FVL_LIQ_FRT write FVL_LIQ_FRT;
    property VL_SEC_CAT: currency read FVL_SEC_CAT write FVL_SEC_CAT;
    property VL_DESP: currency read FVL_DESP write FVL_DESP;
    property VL_PEDG: currency read FVL_PEDG write FVL_PEDG;
    property VL_OUT: currency read FVL_OUT write FVL_OUT;
    property VL_FRT: currency read FVL_FRT write FVL_FRT;
    property UF_ID: String read FUF_ID write FUF_ID;
  end;

  /// Registro D130 - Lista

  TRegistroD130List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD130; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD130); /// SetItem
  public
    function New: TRegistroD130;
    property Items[Index: Integer]: TRegistroD130 read GetItem write SetItem;
  end;

  /// Registro D140 - COMPLEMENTO DO CONHECIMENTO AQUAVIÁRIO DE CARGAS (CÓDIGO 09)

  TRegistroD140 = class
  private
    fCOD_PART_CONSG: String;        /// Código do participante (campo 02 do Registro 0150):
    fCOD_MUN_ORIG: String;          /// Código do município de origem do serviço, conforme a tabela IBGE
    fCOD_MUN_DEST: String;          /// Código do município de destino, conforme a tabela IBGE
    fIND_VEIC: TACBrTipoVeiculo;    /// Indicador do tipo do veículo transportador:
    fVEIC_ID: String;               /// Identificação da embarcação (IRIM ou Registro CPP)
    fIND_NAV: TACBrTipoNavegacao;   /// Indicador do tipo da navegação:
    fVIAGEM: String;                /// Número da viagem
    fVL_FRT_LIQ: currency;          /// Valor líquido do frete
    fVL_DESP_PORT: currency;        /// Valor das despesas portuárias
    fVL_DESP_CAR_DESC: currency;    /// Valor das despesas com carga e descarga
    fVL_OUT: currency;              /// Outros valores
    fVL_FRT_BRT: currency;          /// Valor bruto do frete
    fVL_FRT_MM: currency;           /// Valor adicional do frete para renovação da Marinha Mercante
  public
    property COD_PART_CONSG: String read FCOD_PART_CONSG write FCOD_PART_CONSG;
    property COD_MUN_ORIG: String read FCOD_MUN_ORIG write FCOD_MUN_ORIG;
    property COD_MUN_DEST: String read FCOD_MUN_DEST write FCOD_MUN_DEST;
    property IND_VEIC: TACBrTipoVeiculo read FIND_VEIC write FIND_VEIC;
    property VEIC_ID: String read FVEIC_ID write FVEIC_ID;
    property IND_NAV: TACBrTipoNavegacao read FIND_NAV write FIND_NAV;
    property VIAGEM: String read FVIAGEM write FVIAGEM;
    property VL_FRT_LIQ: currency read FVL_FRT_LIQ write FVL_FRT_LIQ;
    property VL_DESP_PORT: currency read FVL_DESP_PORT write FVL_DESP_PORT;
    property VL_DESP_CAR_DESC: currency read FVL_DESP_CAR_DESC write FVL_DESP_CAR_DESC;
    property VL_OUT: currency read FVL_OUT write FVL_OUT;
    property VL_FRT_BRT: currency read FVL_FRT_BRT write FVL_FRT_BRT;
    property VL_FRT_MM: currency read FVL_FRT_MM write FVL_FRT_MM;
  end;

  /// Registro D140 - Lista

  TRegistroD140List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD140; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD140); /// SetItem
  public
    function New: TRegistroD140;
    property Items[Index: Integer]: TRegistroD140 read GetItem write SetItem;
  end;

  /// Registro D150 - COMPLEMENTO DO CONHECIMENTO AÉREO (CÓDIGO 10)

  TRegistroD150 = class
  private
    fCOD_MUN_ORIG: String;     /// Código do município de origem do serviço, conforme a tabela IBGE
    fCOD_MUN_DEST: String;     /// Código do município de destino, conforme a tabela IBGE
    fVEIC_ID: String;          /// Identificação da aeronave (DAC)
    fVIAGEM: String;           /// Número do vôo.
    fIND_TFA: TACBrTipoTarifa; /// Indicador do tipo de tarifa aplicada: 0- Exp., 1- Enc., 2- C.I., 9- Outra
    fVL_PESO_TX: currency;     /// Peso taxado
    fVL_TX_TERR: currency;     /// Valor da taxa terrestre
    fVL_TX_RED: currency;      /// Valor da taxa de redespacho
    fVL_OUT: currency;         /// Outros valores
    fVL_TX_ADV: currency;      /// Valor da taxa "ad valorem"
  public
    property COD_MUN_ORIG: String read FCOD_MUN_ORIG write FCOD_MUN_ORIG;
    property COD_MUN_DEST: String read FCOD_MUN_DEST write FCOD_MUN_DEST;
    property VEIC_ID: String read FVEIC_ID write FVEIC_ID;
    property VIAGEM: String read FVIAGEM write FVIAGEM;
    property IND_TFA: TACBrTipoTarifa read FIND_TFA write FIND_TFA;
    property VL_PESO_TX: currency read FVL_PESO_TX write FVL_PESO_TX;
    property VL_TX_TERR: currency read FVL_TX_TERR write FVL_TX_TERR;
    property VL_TX_RED: currency read FVL_TX_RED write FVL_TX_RED;
    property VL_OUT: currency read FVL_OUT write FVL_OUT;
    property VL_TX_ADV: currency read FVL_TX_ADV write FVL_TX_ADV;
  end;

  /// Registro D150 - Lista

  TRegistroD150List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD150; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD150); /// SetItem
  public
    function New: TRegistroD150;
    property Items[Index: Integer]: TRegistroD150 read GetItem write SetItem;
  end;

  /// Registro D160 - CARGA TRANSPORTADA (CÓDIGO 07, 08, 09, 10, 11, 26 E 27)

  TRegistroD160 = class
  private
    fDESPACHO       : String; /// Identificação do número do despacho
    fCNPJ_CPF_REM   : String; /// CNPJ ou CPF do remetente das mercadorias que constam na nota fiscal.
    fIE_REM         : String; /// Inscrição Estadual do remetente das mercadorias que constam na nota fiscal.
    fCOD_MUN_ORI    : String; /// Código do Município de origem, conforme tabela IBGE
    fCNPJ_CPF_DEST  : String; /// CNPJ ou CPF do destinatário das mercadorias que constam na nota fiscal.
    fIE_DEST        : String; /// Inscrição Estadual do destinatário das mercadorias que constam na nota fiscal.
    fCOD_MUN_DEST   : String; /// Código do Município de destino, conforme tabela IBGE

    FRegistroD161: TRegistroD161List;
    FRegistroD162: TRegistroD162List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DESPACHO     : String read fDESPACHO       write fDESPACHO     ;
    property CNPJ_CPF_REM : String read fCNPJ_CPF_REM   write fCNPJ_CPF_REM ;
    property IE_REM       : String read fIE_REM         write fIE_REM       ;
    property COD_MUN_ORI  : String read fCOD_MUN_ORI    write fCOD_MUN_ORI  ;
    property CNPJ_CPF_DEST: String read fCNPJ_CPF_DEST  write fCNPJ_CPF_DEST;
    property IE_DEST      : String read fIE_DEST        write fIE_DEST      ;
    property COD_MUN_DEST : String read fCOD_MUN_DEST   write fCOD_MUN_DEST ;

    property RegistroD161: TRegistroD161List read FRegistroD161 write FRegistroD161;
    property RegistroD162: TRegistroD162List read FRegistroD162 write FRegistroD162;
  end;

  /// Registro D160 - Lista

  TRegistroD160List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD160; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD160); /// SetItem
  public
    function New: TRegistroD160;
    property Items[Index: Integer]: TRegistroD160 read GetItem write SetItem;
  end;

  /// Registro D161 - LOCAL DA COLETA E ENTREGA (CÓDIGO 07, 08, 09, 10, 11, 26 E 27)

  TRegistroD161 = class
  private
    fIND_CARGA: TACBrTipoTransporte;    /// Indicador do tipo de transporte da carga coletada:
    fCNPJ_COL: String;                  /// Número do CNPJ do contribuinte do local de coleta
    fIE_COL: String;                    /// Inscrição Estadual do contribuinte do local de coleta
    fCOD_MUN_COL: String;               /// Código do Município do local de coleta, conforme tabela IBGE
    fCNPJ_ENTG: String;                 /// Número do CNPJ do contribuinte do local de entrega
    fIE_ENTG: String;                   /// Inscrição Estadual do contribuinte do local de entrega
    fCOD_MUN_ENTG: String;              /// Código do Município do local de entrega, conforme tabela IBGE
  public
    property IND_CARGA: TACBrTipoTransporte read FIND_CARGA write FIND_CARGA;
    property CNPJ_COL: String read FCNPJ_COL write FCNPJ_COL;
    property IE_COL: String read FIE_COL write FIE_COL;
    property COD_MUN_COL: String read FCOD_MUN_COL write FCOD_MUN_COL;
    property CNPJ_ENTG: String read FCNPJ_ENTG write FCNPJ_ENTG;
    property IE_ENTG: String read FIE_ENTG write FIE_ENTG;
    property COD_MUN_ENTG: String read FCOD_MUN_ENTG write FCOD_MUN_ENTG;
  end;

  /// Registro D161 - Lista

  TRegistroD161List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD161; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD161); /// SetItem
  public
    function New: TRegistroD161;
    property Items[Index: Integer]: TRegistroD161 read GetItem write SetItem;
  end;

  /// Registro D162 - IDENTIFICAÇÃO DOS DOCUMENTOS FISCAIS (COD. 08, 8B, 09, 10, 11, 26, 27)

  TRegistroD162 = class
  private
    fCOD_MOD: String;       /// Código do documento fiscal
    FSER: String;           /// Série do documento
    FNUM_DOC: String;       /// Numero
    FDT_DOC: TDateTime;     /// Data de emissão
    FVL_DOC: currency;      /// Valor total do documento fiscal
    FVL_MERC: currency;     /// Valor das mercadorias constantes no documento fiscal
    FQTD_VOL: Integer;     /// Quantidade de volumes transportados
    FPESO_BRT: currency;    /// Peso bruto
    FPESO_LIQ: currency;    /// Peso liquido
  public
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property SER: String read FSER write FSER;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property VL_DOC: currency read FVL_DOC write FVL_DOC;
    property VL_MERC: currency read FVL_MERC write FVL_MERC;
    property QTD_VOL: Integer read FQTD_VOL write FQTD_VOL;
    property PESO_BRT: currency read FPESO_BRT write FPESO_BRT;
    property PESO_LIQ: currency read FPESO_LIQ write FPESO_LIQ;
  end;

  /// Registro D162 - Lista

  TRegistroD162List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD162; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD162); /// SetItem
  public
    function New: TRegistroD162;
    property Items[Index: Integer]: TRegistroD162 read GetItem write SetItem;
  end;

  /// Registro D170 - COMPLEMENTO DO CONHECIMENTO MULTIMODAL DE CARGAS (CÓDIGO 26)

  TRegistroD170 = class
  private
    fCOD_PART_CONSG: String;          /// Código do participante (campo 02 do Registro 0150):
    fCOD_PART_RED: String;            /// Código do participante (campo 02 do Registro 0150):
    fCOD_MUN_ORIG: String;            /// Código do município de origem do serviço, conforme a tabela IBGE
    fCOD_MUN_DEST: String;            /// Código do município de destino, conforme a tabela IBGE
    fOTM: String;                     /// Registro do operador de transporte multimodal
    fIND_NAT_FRT: TACBrNaturezaFrete; /// Indicador da natureza do frete:
    fVL_LIQ_FRT: currency;            /// Valor líquido do frete
    fVL_GRIS: currency;               /// Valor do gris (gerenciamento de risco)
    fVL_PDG: currency;                /// Somatório dos valores de pedágio
    fVL_OUT: currency;                /// Outros valores
    fVL_FRT: currency;                /// Valor total do frete
    fVEIC_ID: String;                 /// Placa de identificação do veículo
    fUF_ID: String;                   /// Sigla da UF da placa do veículo
  public
    property COD_PART_CONSG: String read FCOD_PART_CONSG write FCOD_PART_CONSG;
    property COD_PART_RED: String read FCOD_PART_RED write FCOD_PART_RED;
    property COD_MUN_ORIG: String read FCOD_MUN_ORIG write FCOD_MUN_ORIG;
    property COD_MUN_DEST: String read FCOD_MUN_DEST write FCOD_MUN_DEST;
    property OTM: String read FOTM write FOTM;
    property IND_NAT_FRT: TACBrNaturezaFrete read FIND_NAT_FRT write FIND_NAT_FRT;
    property VL_LIQ_FRT: currency read FVL_LIQ_FRT write FVL_LIQ_FRT;
    property VL_GRIS: currency read FVL_GRIS write FVL_GRIS;
    property VL_PDG: currency read FVL_PDG write FVL_PDG;
    property VL_OUT: currency read FVL_OUT write FVL_OUT;
    property VL_FRT: currency read FVL_FRT write FVL_FRT;
    property VEIC_ID: String read FVEIC_ID write FVEIC_ID;
    property UF_ID: String read FUF_ID write FUF_ID;
  end;

  /// Registro D170 - Lista

  TRegistroD170List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD170; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD170); /// SetItem
  public
    function New: TRegistroD170;
    property Items[Index: Integer]: TRegistroD170 read GetItem write SetItem;
  end;

  /// Registro D180 - MODAIS (CÓDIGO 26)

  TRegistroD180 = class
  private
    fNUM_SEQ: String;            /// Número de ordem seqüencial do modal
    fIND_EMIT: TACBrIndEmit;     /// Indicador do emitente do documento fiscal: 0- Emissão própria, 1- Terceiros
    fCNPJ_EMIT: String;          /// CNPJ do participante emitente do modal
    fUF_EMIT: String;            /// Sigla da unidade da federação do participante emitente do modal
    fIE_EMIT: String;            /// Inscrição Estadual do participante emitente do modal
    fCOD_MUN_ORIG: String;       /// Código do município de origem do serviço, conforme a tabela IBGE
    fCNPJ_CPF_TOM: String;       /// CNPJ/CPF do participante tomador do serviço
    fUF_TOM: String;             /// Sigla da unidade da federação do participante tomador do serviço
    fIE_TOM: String;             /// Inscrição Estadual do participante tomador do serviço
    fCOD_MUN_DEST: String;       /// Código do município de destino, conforme a tabela IBGE(Preencher com 9999999, se Exterior)
    fCOD_MOD: String;            /// Código do modelo do documento fiscal, conforme a Tabela 4.1.1
    fSER: String;                /// Série do documento fiscal
    fSUB: String;                /// Subsérie do documento fiscal
    fNUM_DOC: String;            /// Número do documento fiscal
    fDT_DOC: TDateTime;          /// Data da emissão do documento fiscal
    fVL_DOC: currency;           /// Valor total do documento fiscal
  public
    property NUM_SEQ: String read FNUM_SEQ write FNUM_SEQ;
    property IND_EMIT: TACBrIndEmit read FIND_EMIT write FIND_EMIT;
    property CNPJ_EMIT: String read FCNPJ_EMIT write FCNPJ_EMIT;
    property UF_EMIT: String read FUF_EMIT write FUF_EMIT;
    property IE_EMIT: String read FIE_EMIT write FIE_EMIT;
    property COD_MUN_ORIG: String read FCOD_MUN_ORIG write FCOD_MUN_ORIG;
    property CNPJ_CPF_TOM: String read FCNPJ_CPF_TOM write FCNPJ_CPF_TOM;
    property UF_TOM: String read FUF_TOM write FUF_TOM;
    property IE_TOM: String read FIE_TOM write FIE_TOM;
    property COD_MUN_DEST: String read FCOD_MUN_DEST write FCOD_MUN_DEST;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property VL_DOC: currency read FVL_DOC write FVL_DOC;
  end;

  /// Registro D180 - Lista

  TRegistroD180List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD180; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD180); /// SetItem
  public
    function New: TRegistroD180;
    property Items[Index: Integer]: TRegistroD180 read GetItem write SetItem;
  end;

  /// Registro D190 - REGISTRO ANALÍTICO DOS DOCUMENTOS (CÓDIGO 07, 08, 09, 10, 11, 26 E 27)

  TRegistroD190 = class
  private
    fCST_ICMS: String;        /// Código da Situação Tributária, conforme a tabela indicada no item 4.3.1
    fCFOP: String;            /// Código Fiscal de Operação e Prestação, conforme a tabela indicada no item 4.2.2
    fALIQ_ICMS: currency;     /// Alíquota do ICMS
    fVL_OPR: currency;        /// Valor da operação correspondente à combinação de CST_ICMS, CFOP, e alíquota do ICMS.
    fVL_BC_ICMS: currency;    /// Parcela correspondente ao "Valor da base de cálculo do ICMS" referente à combinação CST_ICMS, CFOP, e alíquota do ICMS
    fVL_ICMS: currency;       /// Parcela correspondente ao "Valor do ICMS" referente à combinação CST_ICMS,  CFOP e alíquota do ICMS
    fVL_RED_BC: currency;     /// Valor não tributado em função da redução da base de cálculo do ICMS, referente à combinação de CST_ICMS, CFOP e alíquota do ICMS.
    fCOD_OBS: String;         /// Código da observação do lançamento fiscal (campo 02 do Registro 0460)

    procedure SetCFOP(const Value: String);
  public
    property CST_ICMS: String read FCST_ICMS write FCST_ICMS;
    property CFOP: String read FCFOP write SetCFOP;
    property ALIQ_ICMS: currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_OPR: currency read FVL_OPR write FVL_OPR;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
    property VL_RED_BC: currency read FVL_RED_BC write FVL_RED_BC;
    property COD_OBS: String read FCOD_OBS write FCOD_OBS;
  end;

  /// Registro D190 - Lista

  TRegistroD190List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD190; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD190); /// SetItem
  public
    function New: TRegistroD190;
    property Items[Index: Integer]: TRegistroD190 read GetItem write SetItem;
  end;

  /// Registro D195 - OBSERVAÇOES DO LANÇAMENTO FISCAL (CÓDIGO 07, 08, 09, 10, 11, 26 E 27)

  TRegistroD195 = class
  private
    fCOD_OBS: String;    /// Código da observação do lançamento fiscal (campo 02 do Registro 0460)
    fTXT_COMPL: String;  /// Descrição complementar do código de observação.

    fRegistroD197: TRegistroD197List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_OBS: String read FCOD_OBS write FCOD_OBS;
    property TXT_COMPL: String read FTXT_COMPL write FTXT_COMPL;

    property RegistroD197: TRegistroD197List read FRegistroD197 write FRegistroD197;
  end;

  /// Registro D195 - Lista

  TRegistroD195List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD195; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD195); /// SetItem
  public
    function New: TRegistroD195;
    property Items[Index: Integer]: TRegistroD195 read GetItem write SetItem;
  end;

  /// Registro D197 - OUTRAS OBRIGAÇÕES TRIBUTÁRIAS, AJUSTES E INFORMAÇÕES DE VALORES PROVENIENTES DE DOCUMENTO FISCAL.

  TRegistroD197 = class
  private
    fCOD_AJ: String;           /// Código do ajustes/benefício/incentivo, conforme tabela indicada no item 5.3.
    fDESCR_COMPL_AJ: String;   /// Descrição complementar do ajuste da apuração, nos casos em que o código da tabela for “9999”
    fCOD_ITEM: String;         /// Código do item (campo 02 do Registro 0200)
    fVL_BC_ICMS: currency;     /// Base de cálculo do ICMS ou do ICMS ST
    fALIQ_ICMS: currency;      /// Alíquota do ICMS
    fVL_ICMS: currency;        /// Valor do ICMS ou do ICMS ST
    fVL_OUTROS: currency;      /// Outros valores
  public
    property COD_AJ: String read FCOD_AJ write FCOD_AJ;
    property DESCR_COMPL_AJ: String read FDESCR_COMPL_AJ write FDESCR_COMPL_AJ;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property ALIQ_ICMS: currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
    property VL_OUTROS: currency read FVL_OUTROS write FVL_OUTROS;
  end;

  /// Registro C197 - Lista

  TRegistroD197List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD197; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD197); /// SetItem
  public
    function New: TRegistroD197;
    property Items[Index: Integer]: TRegistroD197 read GetItem write SetItem;
  end;

  /// Registro D300 - REGISTRO ANALÍTICO DOS BILHETES CONSOLIDADOS DE PASSAGEM RODOVIÁRIO (CÓDIGO 13), DE PASSAGEM AQUAVIÁRIO (CÓDIGO 14), DE PASSAGEM E NOTA DE BAGAGEM (CÓDIGO 15) E DE PASSAGEM FERROVIÁRIO (CÓDIGO 16)

  TRegistroD300 = class
  private
    fCOD_MOD: String;         /// Código do modelo do documento fiscal, conforme a Tabela 4.1.1
    fSER: String;             /// Série do documento fiscal
    fSUB: String;             /// Subsérie do documento fiscal
    fNUM_DOC_INI: String;     /// Número do primeiro documento fiscal emitido (mesmo modelo, série e subsérie)
    fNUM_DOC_FIN: String;     /// Número do último documento fiscal emitido (mesmo modelo, série e subsérie)
    fCST_ICMS: String;        /// Código da Situação Tributária, conforme a Tabela indicada no item 4.3.1
    fCFOP: String;            /// Código Fiscal de Operação e Prestação conforme tabela indicada no item 4.2.2
    fALIQ_ICMS: currency;     /// Alíquota do ICMS
    fDT_DOC: TDateTime;       /// Data da emissão dos documentos fiscais
    fVL_OPR: currency;        /// Valor total acumulado das operações correspondentes à combinação de CST_ICMS, CFOP e alíquota do ICMS, incluídas as despesas acessórias e acréscimos.
    fVL_DESC: currency;       /// Valor total dos descontos
    fVL_SERV: currency;       /// Valor total da prestação de serviço
    fVL_SEG: currency;        /// Valor de seguro
    fVL_OUT_DESP: currency;   /// Valor de outras despesas
    fVL_BC_ICMS: currency;    /// Valor total da base de cálculo do ICMS
    fVL_ICMS: currency;       /// Valor total do ICMS
    fVL_RED_BC: currency;     /// Valor não tributado em função da redução da base de cálculo do ICMS, referente à combinação de CST_ICMS, CFOP e alíquota do ICMS.
    fCOD_OBS: String;         /// Código da observação do lançamento fiscal (campo 02 do Registro 0460)
    fCOD_CTA: String;         /// Código da conta analítica contábil debitada/creditada

    FRegistroD301: TRegistroD301List;
    FRegistroD310: TRegistroD310List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property NUM_DOC_INI: String read FNUM_DOC_INI write FNUM_DOC_INI;
    property NUM_DOC_FIN: String read FNUM_DOC_FIN write FNUM_DOC_FIN;
    property CST_ICMS: String read FCST_ICMS write FCST_ICMS;
    property CFOP: String read FCFOP write FCFOP;
    property ALIQ_ICMS: currency read FALIQ_ICMS write FALIQ_ICMS;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property VL_OPR: currency read FVL_OPR write FVL_OPR;
    property VL_DESC: currency read FVL_DESC write FVL_DESC;
    property VL_SERV: currency read FVL_SERV write FVL_SERV;
    property VL_SEG: currency read FVL_SEG write FVL_SEG;
    property VL_OUT_DESP: currency read FVL_OUT_DESP write FVL_OUT_DESP;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
    property VL_RED_BC: currency read FVL_RED_BC write FVL_RED_BC;
    property COD_OBS: String read FCOD_OBS write FCOD_OBS;
    property COD_CTA: String read FCOD_CTA write FCOD_CTA;

    property RegistroD301: TRegistroD301List read FRegistroD301 write FRegistroD301;
    property RegistroD310: TRegistroD310List read FRegistroD310 write FRegistroD310;
  end;

  /// Registro D300 - Lista

  TRegistroD300List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD300; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD300); /// SetItem
  public
    function New: TRegistroD300;
    property Items[Index: Integer]: TRegistroD300 read GetItem write SetItem;
  end;

  /// Registro D301 - DOCUMENTOS CANCELADOS DOS BILHETES DE PASSAGEM RODOVIÁRIO (CÓDIGO 13), DE PASSAGEM AQUAVIÁRIO (CÓDIGO 14), DE PASSAGEM E NOTA DE BAGAGEM (CÓDIGO 15) E DE PASSAGEM FERROVIÁRIO (CÓDIGO 16)

  TRegistroD301 = class
  private
    fNUM_DOC_CANC: String;    /// Número do documento fiscal cancelado
  public
    property NUM_DOC_CANC: String read FNUM_DOC_CANC write FNUM_DOC_CANC;
  end;

  /// Registro D301 - Lista

  TRegistroD301List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD301; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD301); /// SetItem
  public
    function New: TRegistroD301;
    property Items[Index: Integer]: TRegistroD301 read GetItem write SetItem;
  end;

  /// Registro D310 - COMPLEMENTO DOS BILHETES (CÓDIGO 13, 14, 15 E 16)

  TRegistroD310 = class
  private
    fCOD_MUN_ORIG: String;    /// Código do município de origem do serviço, conforme a tabela IBGE
    fVL_SERV: currency;       /// Valor total da prestação de serviço
    fVL_BC_ICMS: currency;    /// Valor total da base de cálculo do ICMS
    fVL_ICMS: currency;       /// Valor total do ICMS
  public
    property COD_MUN_ORIG: String read FCOD_MUN_ORIG write FCOD_MUN_ORIG;
    property VL_SERV: currency read FVL_SERV write FVL_SERV;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
  end;

  /// Registro D310 - Lista

  TRegistroD310List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD310; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD310); /// SetItem
  public
    function New: TRegistroD310;
    property Items[Index: Integer]: TRegistroD310 read GetItem write SetItem;
  end;

  /// Registro D350 - EQUIPAMENTO ECF (CÓDIGOS 2E, 13, 14, 15 e 16)

  TRegistroD350 = class
  private
    fCOD_MOD: String;      /// Código do modelo do documento fiscal, conforme a Tabela 4.1.1
    fECF_MOD: String;      /// Modelo do equipamento
    fECF_FAB: String;      /// Número de série de fabricação do ECF
    fECF_CX: String;       /// Número do caixa atribuído ao ECF

    FRegistroD355: TRegistroD355List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property ECF_MOD: String read FECF_MOD write FECF_MOD;
    property ECF_FAB: String read FECF_FAB write FECF_FAB;
    property ECF_CX: String read FECF_CX write FECF_CX;

    property RegistroD355: TRegistroD355List read FRegistroD355 write FRegistroD355;
  end;

  /// Registro D350 - Lista

  TRegistroD350List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD350; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD350); /// SetItem
  public
    function New: TRegistroD350;
    property Items[Index: Integer]: TRegistroD350 read GetItem write SetItem;
  end;

  /// Registro D355 - REDUÇÃO Z (CÓDIGOS 2E, 13, 14, 15 e 16)

  TRegistroD355 = class
  private
    fDT_DOC: TDateTime;       /// Data do movimento a que se refere a Redução Z
    fCRO: integer;            /// Posição do Contador de Reinício de Operação
    fCRZ: integer;            /// Posição do Contador de Redução Z
    fNUM_COO_FIN: integer;    /// Número do Contador de Ordem de Operação do último documento emitido no dia. (Número do COO na Redução Z)
    fGT_FIN: currency;        /// Valor do Grande Total final
    fVL_BRT: currency;        /// Valor da venda bruta

    FRegistroD360: TRegistroD360List;
    FRegistroD365: TRegistroD365List;
    FRegistroD390: TRegistroD390List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CRO: integer read FCRO write FCRO;
    property CRZ: integer read FCRZ write FCRZ;
    property NUM_COO_FIN: integer read FNUM_COO_FIN write FNUM_COO_FIN;
    property GT_FIN: currency read FGT_FIN write FGT_FIN;
    property VL_BRT: currency read FVL_BRT write FVL_BRT;

    property RegistroD360: TRegistroD360List read FRegistroD360 write FRegistroD360;
    property RegistroD365: TRegistroD365List read FRegistroD365 write FRegistroD365;
    property RegistroD390: TRegistroD390List read FRegistroD390 write FRegistroD390;
  end;

  /// Registro D355 - Lista

  TRegistroD355List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD355; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD355); /// SetItem
  public
    function New: TRegistroD355;
    property Items[Index: Integer]: TRegistroD355 read GetItem write SetItem;
  end;

  /// Registro D360 - PIS E COFINS TOTALIZADOS NO DIA (CÓDIGOS 2E, 13, 14, 15 e 16)

  TRegistroD360 = class
  private
    fVL_PIS: currency;        /// Valor total do PIS
    fVL_COFINS: currency;     /// Valor total do COFINS
  public
    property VL_PIS: currency read FVL_PIS write FVL_PIS;
    property VL_COFINS: currency read FVL_COFINS write FVL_COFINS;
  end;

  /// Registro D360 - Lista

  TRegistroD360List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD360; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD360); /// SetItem
  public
    function New: TRegistroD360;
    property Items[Index: Integer]: TRegistroD360 read GetItem write SetItem;
  end;

  /// Registro D365 - REGISTRO DOS TOTALIZADORES PARCIAIS DE REDUÇÃO Z (CODIGOS 2E 13 14 15 16)

  TRegistroD365 = class
  private
    fCOD_TOT_PAR: String;        /// Código do totalizador, conforme Tabela 4.4.6
    fVLR_ACUM_TOT: currency;     /// Valor acumulado no totalizador, relativo à respectiva Redução Z.
    fNR_TOT: String;             /// Número do totalizador quando ocorrer mais de uma situação com a mesma carga tributária efetiva.
    fDESCR_NR_TOT: String;       /// Descrição da situação tributária relativa ao totalizador parcial, quando houver mais de um com a mesma carga tributária efetiva.

    FRegistroD370: TRegistroD370List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_TOT_PAR: String read FCOD_TOT_PAR write FCOD_TOT_PAR;
    property VLR_ACUM_TOT: currency read FVLR_ACUM_TOT write FVLR_ACUM_TOT;
    property NR_TOT: String read FNR_TOT write FNR_TOT;
    property DESCR_NR_TOT: String read FDESCR_NR_TOT write FDESCR_NR_TOT;

    property RegistroD370: TRegistroD370List read FRegistroD370 write FRegistroD370;
  end;

  /// Registro D365 - Lista

  TRegistroD365List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD365; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD365); /// SetItem
  public
    function New: TRegistroD365;
    property Items[Index: Integer]: TRegistroD365 read GetItem write SetItem;
  end;

  /// Registro D370 - COMPLEMENTO DOS DOCUMENTOS INFORMADOS (CÓDIGO 13, 14, 15, 16 E 2E)

  TRegistroD370 = class
  private
    fCOD_MUN_ORIG: String;    /// Código do município de origem do serviço, conforme a tabela IBGE
    fVL_SERV: currency;       /// Valor total da prestação de serviço
    fQTD_BILH: currency;      /// Quantidade de bilhetes emitidos
    fVL_BC_ICMS: currency;    /// Valor total da base de cálculo do ICMS
    fVL_ICMS: currency;       /// Valor total do ICMS
  public
    property COD_MUN_ORIG: String read FCOD_MUN_ORIG write FCOD_MUN_ORIG;
    property VL_SERV: currency read FVL_SERV write FVL_SERV;
    property QTD_BILH: currency read FQTD_BILH write FQTD_BILH;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
  end;

  /// Registro D370 - Lista

  TRegistroD370List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD370; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD370); /// SetItem
  public
    function New: TRegistroD370;
    property Items[Index: Integer]: TRegistroD370 read GetItem write SetItem;
  end;

  /// Registro D390 - REGISTRO ANALÍTICO DO MOVIMENTO DIÁRIO (CÓDIGOS 13, 14, 15, 16 E 2E)

  TRegistroD390 = class
  private
    fCST_ICMS: String;           /// Código da Situação Tributária, conforme a Tabela indicada no item 4.3.1.
    fCFOP: String;               /// Código Fiscal de Operação e Prestação
    fALIQ_ICMS: currency;        /// Alíquota do ICMS
    fVL_OPR: currency;           /// Valor da operação correspondente à combinação de CST_ICMS, CFOP, e alíquota do ICMS, incluídas as despesas acessórias e acréscimos
    fVL_BC_ISSQN: currency;      /// Valor da base de cálculo do ISSQN
    fALIQ_ISSQN: currency;       /// Alíquota do ISSQN
    fVL_ISSQN: currency;         /// Valor do ISSQN
    fVL_BC_ICMS: currency;       /// Base de cálculo do ICMS acumulada relativa à alíquota informada
    fVL_ICMS: currency;          /// Valor do ICMS acumulado relativo à alíquota informada
    fCOD_OBS: String;            /// Código da observação do lançamento fiscal (campo 02 do Registro 0460)
  public
    property CST_ICMS: String read FCST_ICMS write FCST_ICMS;
    property CFOP: String read FCFOP write FCFOP;
    property ALIQ_ICMS: currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_OPR: currency read FVL_OPR write FVL_OPR;
    property VL_BC_ISSQN: currency read FVL_BC_ISSQN write FVL_BC_ISSQN;
    property ALIQ_ISSQN: currency read FALIQ_ISSQN write FALIQ_ISSQN;
    property VL_ISSQN: currency read FVL_ISSQN write FVL_ISSQN;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
    property COD_OBS: String read FCOD_OBS write FCOD_OBS;
  end;

  /// Registro D390 - Lista

  TRegistroD390List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD390; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD390); /// SetItem
  public
    function New: TRegistroD390;
    property Items[Index: Integer]: TRegistroD390 read GetItem write SetItem;
  end;

  /// Registro D400 - RESUMO DE MOVIMENTO DIÁRIO (CÓDIGO 18)

  TRegistroD400 = class
  private
    fCOD_PART: String;            /// Código do participante (campo 02 do Registro 0150): - agência, filial ou posto
    fCOD_MOD: String;             /// Código do modelo do documento fiscal, conforme a Tabela 4.1.1
    fCOD_SIT: TACBrCodSit;        /// Código da situação do documento fiscal, conforme a Tabela 4.1.2
    fSER: String;                 /// Série do documento fiscal
    fSUB: String;                 /// Subsérie do documento fiscal
    fNUM_DOC: String;             /// Número do documento fiscal resumo.
    fDT_DOC: TDateTime;           /// Data da emissão do documento fiscal
    fVL_DOC: currency;            /// Valor total do documento fiscal
    fVL_DESC: currency;           /// Valor acumulado dos descontos
    fVL_SERV: currency;           /// Valor acumulado da prestação de serviço
    fVL_BC_ICMS: currency;        /// Valor total da base de cálculo do ICMS
    fVL_ICMS: currency;           /// Valor total do ICMS
    fVL_PIS: currency;            /// Valor do PIS
    fVL_COFINS: currency;         /// Valor da COFINS
    fCOD_CTA: String;             /// Código da conta analítica contábil debitada/creditada

    FRegistroD410: TRegistroD410List;
    FRegistroD420: TRegistroD420List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_PART: String read FCOD_PART write FCOD_PART;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property COD_SIT: TACBrCodSit read FCOD_SIT write FCOD_SIT;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property VL_DOC: currency read FVL_DOC write FVL_DOC;
    property VL_DESC: currency read FVL_DESC write FVL_DESC;
    property VL_SERV: currency read FVL_SERV write FVL_SERV;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
    property VL_PIS: currency read FVL_PIS write FVL_PIS;
    property VL_COFINS: currency read FVL_COFINS write FVL_COFINS;
    property COD_CTA: String read FCOD_CTA write FCOD_CTA;

    property RegistroD410: TRegistroD410List read FRegistroD410 write FRegistroD410;
    property RegistroD420: TRegistroD420List read FRegistroD420 write FRegistroD420;
  end;

  /// Registro D400 - Lista

  TRegistroD400List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD400; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD400); /// SetItem
  public
    function New: TRegistroD400;
    property Items[Index: Integer]: TRegistroD400 read GetItem write SetItem;
  end;

  /// Registro D410 - DOCUMENTOS INFORMADOS (CÓDIGOS 13, 14, 15 E 16)

  TRegistroD410 = class
  private
    fCOD_MOD: String;      /// Código do modelo do documento fiscal , conforme a Tabela 4.1.1
    fSER: String;          /// Série do documento fiscal
    fSUB: String;          /// Subsérie do documento fiscal
    fNUM_DOC_INI: String;  /// Número do documento fiscal inicial (mesmo modelo, série e subsérie)
    fNUM_DOC_FIN: String;  /// Número do documento fiscal final(mesmo modelo, série e subsérie)
    fDT_DOC: TDateTime;    /// Data da emissão dos documentos fiscais
    fCST_ICMS: String;     /// Código da Situação Tributária, conforme a Tabela indicada no item 4.3.1
    fCFOP: String;         /// Código Fiscal de Operação e Prestação
    fALIQ_ICMS: currency;  /// Alíquota do ICMS
    fVL_OPR: currency;     /// Valor total acumulado das operações correspondentes à combinação de CST_ICMS, CFOP e alíquota do ICMS, incluídas as despesas acessórias e acréscimos.
    fVL_DESC: currency;    /// Valor acumulado dos descontos
    fVL_SERV: currency;    /// Valor acumulado da prestação de serviço
    fVL_BC_ICMS: currency; /// Valor acumulado da base de cálculo do ICMS
    fVL_ICMS: currency;    /// Valor acumulado do ICMS

    FRegistroD411: TRegistroD411List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property NUM_DOC_INI: String read FNUM_DOC_INI write FNUM_DOC_INI;
    property NUM_DOC_FIN: String read FNUM_DOC_FIN write FNUM_DOC_FIN;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CST_ICMS: String read FCST_ICMS write FCST_ICMS;
    property CFOP: String read FCFOP write FCFOP;
    property ALIQ_ICMS: currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_OPR: currency read FVL_OPR write FVL_OPR;
    property VL_DESC: currency read FVL_DESC write FVL_DESC;
    property VL_SERV: currency read FVL_SERV write FVL_SERV;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;

    property RegistroD411: TRegistroD411List read FRegistroD411 write FRegistroD411;
  end;

  /// Registro D410 - Lista

  TRegistroD410List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD410; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD410); /// SetItem
  public
    function New: TRegistroD410;
    property Items[Index: Integer]: TRegistroD410 read GetItem write SetItem;
  end;

  /// Registro D411 - DOCUMENTOS CANCELADOS DOS DOCUMENTOS INFORMADOS (CÓDIGOS 13, 14, 15 E 16)

  TRegistroD411 = class
  private
    fNUM_DOC_CANC: String;    /// Número do documento fiscal cancelado
  public
    property NUM_DOC_CANC: String read FNUM_DOC_CANC write FNUM_DOC_CANC;
  end;

  /// Registro D411 - Lista

  TRegistroD411List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD411; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD411); /// SetItem
  public
    function New: TRegistroD411;
    property Items[Index: Integer]: TRegistroD411 read GetItem write SetItem;
  end;

  /// Registro D420 - COMPLEMENTO DOS DOCUMENTOS INFORMADOS(CÓDIGOS 13, 14, 15 E 16)

  TRegistroD420 = class
  private
    fCOD_MUN_ORIG: String;    /// Código do município de origem do serviço, conforme a tabela IBGE
    fVL_SERV: currency;       /// Valor total da prestação de serviço
    fVL_BC_ICMS: currency;    /// Valor total da base de cálculo do ICMS
    fVL_ICMS: currency;       /// Valor total do ICMS
  public
    property COD_MUN_ORIG: String read FCOD_MUN_ORIG write FCOD_MUN_ORIG;
    property VL_SERV: currency read FVL_SERV write FVL_SERV;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
  end;

  /// Registro D420 - Lista

  TRegistroD420List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD420; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD420); /// SetItem
  public
    function New: TRegistroD420;
    property Items[Index: Integer]: TRegistroD420 read GetItem write SetItem;
  end;

  /// Registro D500 - NOTA FISCAL DE SERVIÇO DE COMUNICAÇÃO (CÓDIGO 21) E NOTA FISCAL DE SERVIÇO DE TELECOMUNICAÇÃO (CÓDIGO 22)

  TRegistroD500 = class
  private
    fIND_OPER: TACBrIndOper;           /// Indicador do tipo de operação: 0- Aquisição, 1- Prestação
    fIND_EMIT: TACBrIndEmit;           /// Indicador do emitente do documento fiscal: 0- Emissão própria, 1- Terceiros
    fCOD_PART: String;                 /// Código do participante (campo 02 do Registro 0150): - do prestador do serviço, no caso de aquisição, - do tomador do serviço, no caso de prestação.
    fCOD_MOD: String;                  /// Código do modelo do documento fiscal, conforme a Tabela 4.1.1
    fCOD_SIT: TACBrCodSit;             /// Código da situação do documento fiscal, conforme a Tabela 4.1.2
    fSER: String;                      /// Série do documento fiscal
    fSUB: String;                      /// Subsérie do documento fiscal
    fNUM_DOC: String;                  /// Número do documento fiscal
    fDT_DOC: TDateTime;                /// Data da emissão do documento fiscal
    fDT_A_P: TDateTime;                /// Data da entrada (aquisição) ou da saída (prestação do serviço)
    fVL_DOC: currency;                 /// Valor total do documento fiscal
    fVL_DESC: currency;                /// Valor total do desconto
    fVL_SERV: currency;                /// Valor da prestação de serviços
    fVL_SERV_NT: currency;             /// Valor total dos serviços não-tributados pelo ICMS
    fVL_TERC: currency;                /// Valores cobrados em nome de terceiros
    fVL_DA: currency;                  /// Valor de outras despesas indicadas no documento fiscal
    fVL_BC_ICMS: currency;             /// Valor da base de cálculo do ICMS
    fVL_ICMS: currency;                /// Valor do ICMS
    fCOD_INF: String;                  /// Código da informação complementar (campo 02 do Registro 0450)
    fVL_PIS: currency;                 /// Valor do PIS
    fVL_COFINS: currency;              /// Valor da COFINS
    fCOD_CTA: String;                  /// Código da conta analítica contábil debitada/creditada
    fTP_ASSINANTE: TACBrTipoAssinante; /// Código do Tipo de Assinante: 1 - Comercial/Industrial, 2 - Poder Público, 3 - Residencial/Pessoa física, 4 - Público, 5 - Semi-Público, 6 - Outros

    FRegistroD510: TRegistroD510List;
    FRegistroD530: TRegistroD530List;
    FRegistroD590: TRegistroD590List; /// BLOCO D - Lista de RegistroD590 (FILHO) {Jean Barreiros 04Dez2009}
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_OPER: TACBrIndOper read FIND_OPER write FIND_OPER;
    property IND_EMIT: TACBrIndEmit read FIND_EMIT write FIND_EMIT;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property COD_SIT: TACBrCodSit read FCOD_SIT write FCOD_SIT;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property DT_A_P: TDateTime read FDT_A_P write FDT_A_P;
    property VL_DOC: currency read FVL_DOC write FVL_DOC;
    property VL_DESC: currency read FVL_DESC write FVL_DESC;
    property VL_SERV: currency read FVL_SERV write FVL_SERV;
    property VL_SERV_NT: currency read FVL_SERV_NT write FVL_SERV_NT;
    property VL_TERC: currency read FVL_TERC write FVL_TERC;
    property VL_DA: currency read FVL_DA write FVL_DA;
    property VL_BC_ICMS: currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: currency read FVL_ICMS write FVL_ICMS;
    property COD_INF: String read FCOD_INF write FCOD_INF;
    property VL_PIS: currency read FVL_PIS write FVL_PIS;
    property VL_COFINS: currency read FVL_COFINS write FVL_COFINS;
    property COD_CTA: String read FCOD_CTA write FCOD_CTA;
    property TP_ASSINANTE: TACBrTipoAssinante read FTP_ASSINANTE write FTP_ASSINANTE;

    property RegistroD510: TRegistroD510List read FRegistroD510 write FRegistroD510;
    property RegistroD530: TRegistroD530List read FRegistroD530 write FRegistroD530;
    property RegistroD590: TRegistroD590List read FRegistroD590 write FRegistroD590;  {Jean Barreiros 04Dez2009}
  end;

  /// Registro D500 - Lista

  TRegistroD500List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD500; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD500); /// SetItem
  public
    function New: TRegistroD500;
    property Items[Index: Integer]: TRegistroD500 read GetItem write SetItem;
  end;

  /// Registro D510

  TRegistroD510 = class
  private
    FNUM_ITEM: String;                  //Número sequencial do item no documento fiscal
    FCOD_ITEM: String;                  //Código do item (campo 02 do Registro 0200)
    FCOD_CLASS: String;                 //Código de classificação do item do serviço de comunicação ou de telecomunicação, conforme a Tabela 4.4.1
    FQTD: Currency;                     //Quantidade do item
    FUNID: String;                      //Unidade do item (Campo 02 do registro 0190)
    FVL_ITEM: Currency;                 //Valor do item
    FVL_DESC: Currency;                 //Valor total do desconto
    FCST_ICMS: String;                  //Código da Situação Tributária, conforme a Tabela indicada no item 4.3.1
    FCFOP: String;                      //Código Fiscal de Operação e Prestação
    FVL_BC_ICMS: Currency;              //Valor da base de cálculo do ICMS
    FALIQ_ICMS: Currency;               //Alíquota do ICMS
    FVL_ICMS: Currency;                 //Valor do ICMS creditado/debitado
    FVL_BC_ICMS_UF : Currency;          //Valor da base de cálculo do ICMS de outras UFs
    FVL_ICMS_UF: Currency;              //Valor do ICMS de outras UFs
    FIND_REC: TACBrIndTipoReceita;      //Indicador do tipo de receita
    FCOD_PART: String;                  //Código do participante
    FVL_PIS: Currency;                  //Valor do PIS
    FVL_COFINS: Currency;               //Valor da COFINS
    FCOD_CTA: String;                   //Código da conta analítica contábil debitada/creditada
  public
    property NUM_ITEM: String read FNUM_ITEM write FNUM_ITEM;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property COD_CLASS: String read FCOD_CLASS write FCOD_CLASS;
    property QTD: Currency read FQTD write FQTD;
    property UNID: String read FUNID write FUNID;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property VL_DESC: Currency read FVL_DESC write FVL_DESC;
    property CST_ICMS: String read FCST_ICMS write FCST_ICMS;
    property CFOP: String read FCFOP write FCFOP;
    property VL_BC_ICMS: Currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property ALIQ_ICMS: Currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_ICMS: Currency read FVL_ICMS write FVL_ICMS;
    property VL_BC_ICMS_UF: Currency read FVL_BC_ICMS_UF write FVL_BC_ICMS_UF;
    property VL_ICMS_UF: Currency read FVL_ICMS_UF write FVL_ICMS_UF;
    property IND_REC: TACBrIndTipoReceita read FIND_REC write FIND_REC;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property VL_PIS: Currency read FVL_PIS write FVL_PIS;
    property VL_COFINS: Currency read FVL_COFINS write FVL_COFINS;
    property COD_CTA: String read FCOD_CTA write FCOD_CTA;
  end;

  TRegistroD510List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD510; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD510); /// SetItem
  public
    function New: TRegistroD510;
    property Items[Index: Integer]: TRegistroD510 read GetItem write SetItem;
  end;

  /// Registro D530

  TRegistroD530 = class
  private
    FIND_SERV: TACBrServicoPrestado;      //Indicador do tipo de serviço prestado
    FDT_INI_SERV: TDateTime;              //Data em que se iniciou a prestação do serviço
    FDT_FIN_SERV: TDateTime;              //Data em que se encerrou a prestação do serviço
    FPER_FISCAL: String;                  //Período fiscal da prestação do serviço (MMAAAA)
    FCOD_AREA: String;                    //Código de área do terminal faturado
    FTERMINAL: String;                    //Identificação do terminal faturado
  public
    property IND_SERV: TACBrServicoPrestado read FIND_SERV write FIND_SERV;
    property DT_INI_SERV: TDateTime read FDT_INI_SERV write FDT_INI_SERV;
    property DT_FIN_SERV: TDateTime read FDT_FIN_SERV write FDT_FIN_SERV;
    property PER_FISCAL: String read FPER_FISCAL write FPER_FISCAL;
    property COD_AREA: String read FCOD_AREA write FCOD_AREA;
    property TERMINAL: String read FTERMINAL write FTERMINAL;

  end;

  TRegistroD530List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD530; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD530); /// SetItem
  public
    function New: TRegistroD530;
    property Items[Index: Integer]: TRegistroD530 read GetItem write SetItem;
  end;

  /// Registro D590

  TRegistroD590 = class
  private
    fCST_ICMS: String;        /// Código da Situação Tributária, conforme a Tabela indicada no item 4.3.1.
    fCFOP: String;            /// Código Fiscal de Operação e Prestação do agrupamento de itens
    fALIQ_ICMS: Currency;     /// Alíquota do ICMS
    fVL_OPR: currency;        /// Valor da operação correspondente à combinação de CST_ICMS, CFOP, e alíquota do ICMS.
    fVL_BC_ICMS: currency;    /// Parcela correspondente ao "Valor da base de cálculo do ICMS" referente à combinação de CST_ICMS, CFOP e alíquota do ICMS.
    fVL_ICMS: currency;       /// Parcela correspondente ao "Valor do ICMS" referente à combinação de CST_ICMS, CFOP e alíquota do ICMS.
    fVL_BC_ICMS_ST: currency; /// Parcela correspondente ao "Valor da base de cálculo do ICMS" da substituição tributária referente à combinação de CST_ICMS, CFOP e alíquota do ICMS.
    fVL_ICMS_ST: currency;    /// Parcela correspondente ao valor creditado/debitado do ICMS da substituição tributária, referente à combinação de CST_ICMS,  CFOP, e alíquota do ICMS.
    fVL_RED_BC: currency;     /// Valor não tributado em função da redução da base de cálculo do ICMS, referente à combinação de CST_ICMS, CFOP e alíquota do ICMS.
    fCOD_OBS: String;         /// Código da observação do lançamento fiscal (campo 02 do Registro 0460)
  public
    property CST_ICMS: String read fCST_ICMS write fCST_ICMS;
    property CFOP: String read fCFOP write fCFOP;
    property ALIQ_ICMS: Currency read fALIQ_ICMS write fALIQ_ICMS;
    property VL_OPR: currency read fVL_OPR write fVL_OPR;
    property VL_BC_ICMS: currency read fVL_BC_ICMS write fVL_BC_ICMS;
    property VL_ICMS: currency read fVL_ICMS write fVL_ICMS;
    property VL_BC_ICMS_UF: currency read fVL_BC_ICMS_ST write fVL_BC_ICMS_ST;
    property VL_ICMS_UF: currency read fVL_ICMS_ST write fVL_ICMS_ST;
    property VL_RED_BC: currency read fVL_RED_BC write fVL_RED_BC;
    property COD_OBS: String read fCOD_OBS write fCOD_OBS;
  end;

  TRegistroD590List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD590; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD590); /// SetItem
  public
    function New: TRegistroD590;
    property Items[Index: Integer]: TRegistroD590 read GetItem write SetItem;
  end;

  /// Registro D600

  TRegistroD600 = class
  private
    FRegistroD610: TRegistroD610List;
    FRegistroD690: TRegistroD690List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroD610: TRegistroD610List read FRegistroD610 write FRegistroD610;
    property RegistroD690: TRegistroD690List read FRegistroD690 write FRegistroD690;
  end;

  TRegistroD600List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD600; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD600); /// SetItem
  public
    function New: TRegistroD600;
    property Items[Index: Integer]: TRegistroD600 read GetItem write SetItem;
  end;

  /// Registro D610

  TRegistroD610 = class
  private
  public
  end;

  TRegistroD610List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD610; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD610); /// SetItem
  public
    function New: TRegistroD610;
    property Items[Index: Integer]: TRegistroD610 read GetItem write SetItem;
  end;

  /// Registro D690

  TRegistroD690 = class
  private
  public
  end;

  TRegistroD690List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD690; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD690); /// SetItem
  public
    function New: TRegistroD690;
    property Items[Index: Integer]: TRegistroD690 read GetItem write SetItem;
  end;

  /// Registro D695

  TRegistroD695 = class
  private
    fCOD_MOD: String; // Código do modelo do documento fiscal, conforme a Tabela 4.1.1
    fSER: String;    // Série do documento fiscal
    fNRO_ORD_INI: integer;  // Número de ordem inicial
    fNRO_ORD_FIN: integer;  // Número de ordem final
    fDT_DOC_INI: Tdatetime; // Data de emissão inicial dos documentos
    fDT_DOC_FIN: Tdatetime; // Data de emissão final dos documentos
    fNOM_MEST:string;      // Nome do arquivo Mestre de Documento Fiscal
    fCHV_COD_DIG:string;   // Chave de codificação digital do arquivo Mestre de Documento Fiscal
    FRegistroD696: TRegistroD696List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_MOD: String read fCOD_MOD write fCOD_MOD;
    property SER: String read fSER write fSER;
    property NRO_ORD_INI: integer read fNRO_ORD_INI write fNRO_ORD_INI;
    property NRO_ORD_FIN: integer read fNRO_ORD_FIN write fNRO_ORD_FIN;
    property DT_DOC_INI: Tdatetime read fDT_DOC_INI write fDT_DOC_INI;
    property DT_DOC_FIN: Tdatetime read fDT_DOC_FIN write fDT_DOC_FIN;
    property NOM_MEST: String read fNOM_MEST write fNOM_MEST;
    property CHV_COD_DIG: String read fCHV_COD_DIG write fCHV_COD_DIG;

    property RegistroD696: TRegistroD696List read FRegistroD696 write FRegistroD696;
  end;

  TRegistroD695List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD695; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD695); /// SetItem
  public
    function New: TRegistroD695;
    property Items[Index: Integer]: TRegistroD695 read GetItem write SetItem;
  end;

  /// Registro D696

  TRegistroD696 = class
  private
    fCST_ICMS: String;        /// Código da Situação Tributária, conforme a Tabela indicada no item 4.3.1.
    fCFOP: String;            /// Código Fiscal de Operação e Prestação do agrupamento de itens
    fALIQ_ICMS: Currency;     /// Alíquota do ICMS
    fVL_OPR: currency;        /// Valor da operação correspondente à combinação de CST_ICMS, CFOP, e alíquota do ICMS.
    fVL_BC_ICMS: currency;    /// Parcela correspondente ao "Valor da base de cálculo do ICMS" referente à combinação de CST_ICMS, CFOP e alíquota do ICMS.
    fVL_ICMS: currency;       /// Parcela correspondente ao "Valor do ICMS" referente à combinação de CST_ICMS, CFOP e alíquota do ICMS.
    fVL_BC_ICMS_UF: currency; /// Parcela correspondente ao "Valor da base de cálculo do ICMS" da substituição tributária referente à combinação de CST_ICMS, CFOP e alíquota do ICMS.
    fVL_ICMS_UF: currency;    /// Parcela correspondente ao valor creditado/debitado do ICMS da substituição tributária, referente à combinação de CST_ICMS,  CFOP, e alíquota do ICMS.
    fVL_RED_BC: currency;     /// Valor não tributado em função da redução da base de cálculo do ICMS, referente à combinação de CST_ICMS, CFOP e alíquota do ICMS.
    fCOD_OBS: String;         /// Código da observação do lançamento fiscal (campo 02 do Registro 0460)
    FRegistroD697: TRegistroD697List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property CST_ICMS: String read fCST_ICMS write fCST_ICMS;
    property CFOP: String read fCFOP write fCFOP;
    property ALIQ_ICMS: Currency read fALIQ_ICMS write fALIQ_ICMS;
    property VL_OPR: currency read fVL_OPR write fVL_OPR;
    property VL_BC_ICMS: currency read fVL_BC_ICMS write fVL_BC_ICMS;
    property VL_ICMS: currency read fVL_ICMS write fVL_ICMS;
    property VL_BC_ICMS_UF: currency read fVL_BC_ICMS_UF write fVL_BC_ICMS_UF;
    property VL_ICMS_UF: currency read fVL_ICMS_UF write fVL_ICMS_UF;
    property VL_RED_BC: currency read fVL_RED_BC write fVL_RED_BC;
    property COD_OBS: String read fCOD_OBS write fCOD_OBS;

    property RegistroD697: TRegistroD697List read FRegistroD697 write FRegistroD697;
  end;

  TRegistroD696List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD696; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD696); /// SetItem
  public
    function New: TRegistroD696;
    property Items[Index: Integer]: TRegistroD696 read GetItem write SetItem;
  end;

  /// Registro D697

  TRegistroD697 = class
  private
    fUF: String;              /// Sigla da unidade da federação
    fVL_BC_ICMS: currency;    /// Valor da base de cálculo do ICMS
    fVL_ICMS: currency;       /// Valor do ICMS
  public
    property UF: String read fUF write fUF;
    property VL_BC_ICMS: currency read fVL_BC_ICMS write fVL_BC_ICMS;
    property VL_ICMS: currency read fVL_ICMS write fVL_ICMS;
  end;

  TRegistroD697List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD697; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroD697); /// SetItem
  public
    function New: TRegistroD697;
    property Items[Index: Integer]: TRegistroD697 read GetItem write SetItem;
  end;

  /// Registro D990 - ENCERRAMENTO DO BLOCO D

  TRegistroD990 = class
  private
    fQTD_LIN_D: Integer; /// Quantidade total de linhas do Bloco D
  public
    property QTD_LIN_D: Integer read fQTD_LIN_D write fQTD_LIN_D;
  end;

implementation

{ TRegistroD100List }

function TRegistroD100List.GetItem(Index: Integer): TRegistroD100;
begin
  Result := TRegistroD100(Inherited Items[Index]);
end;

function TRegistroD100List.New(AOwner: TRegistroD001): TRegistroD100;
begin
  Result := TRegistroD100.Create(AOwner);
  Add(Result);
end;

procedure TRegistroD100List.SetItem(Index: Integer; const Value: TRegistroD100);
begin
  Put(Index, Value);
end;

{ TRegistroD101List }

function TRegistroD101List.GetItem(Index: Integer): TRegistroD101;
begin
  Result := TRegistroD101(Inherited Items[Index]);
end;

function TRegistroD101List.New: TRegistroD101;
begin
  Result := TRegistroD101.Create;
  Add(Result);
end;

procedure TRegistroD101List.SetItem(Index: Integer; const Value: TRegistroD101);
begin
  Put(Index, Value);
end;

{ TRegistroD110List }

function TRegistroD590List.GetItem(Index: Integer): TRegistroD590;
begin
  Result := TRegistroD590(Inherited Items[Index]);
end;

function TRegistroD590List.New: TRegistroD590;
begin
  Result := TRegistroD590.Create;
  Add(Result);
end;

procedure TRegistroD590List.SetItem(Index: Integer; const Value: TRegistroD590);
begin
  Put(Index, Value);
end;



function TRegistroD110List.GetItem(Index: Integer): TRegistroD110;
begin
  Result := TRegistroD110(Inherited Items[Index]);
end;

function TRegistroD110List.New(AOwner: TRegistroD100): TRegistroD110;
begin
  Result := TRegistroD110.Create(AOwner);
  Add(Result);
end;

procedure TRegistroD110List.SetItem(Index: Integer; const Value: TRegistroD110);
begin
  Put(Index, Value);
end;

{ TRegistroD120List }

function TRegistroD120List.GetItem(Index: Integer): TRegistroD120;
begin
  Result := TRegistroD120(Inherited Items[Index]);
end;

function TRegistroD120List.New: TRegistroD120;
begin
  Result := TRegistroD120.Create;
  Add(Result);
end;

procedure TRegistroD120List.SetItem(Index: Integer; const Value: TRegistroD120);
begin
  Put(Index, Value);
end;

{ TRegistroD130List }

function TRegistroD130List.GetItem(Index: Integer): TRegistroD130;
begin
  Result := TRegistroD130(Inherited Items[Index]);
end;

function TRegistroD130List.New: TRegistroD130;
begin
  Result := TRegistroD130.Create;
  Add(Result);
end;

procedure TRegistroD130List.SetItem(Index: Integer; const Value: TRegistroD130);
begin
  Put(Index, Value);
end;

{ TRegistroD140List }

function TRegistroD140List.GetItem(Index: Integer): TRegistroD140;
begin
  Result := TRegistroD140(Inherited Items[Index]);
end;

function TRegistroD140List.New: TRegistroD140;
begin
  Result := TRegistroD140.Create;
  Add(Result);
end;

procedure TRegistroD140List.SetItem(Index: Integer; const Value: TRegistroD140);
begin
  Put(Index, Value);
end;

{ TRegistroD150List }

function TRegistroD150List.GetItem(Index: Integer): TRegistroD150;
begin
  Result := TRegistroD150(Inherited Items[Index]);
end;

function TRegistroD150List.New: TRegistroD150;
begin
  Result := TRegistroD150.Create;
  Add(Result);
end;

procedure TRegistroD150List.SetItem(Index: Integer; const Value: TRegistroD150);
begin
  Put(Index, Value);
end;

{ TRegistroD160List }

function TRegistroD160List.GetItem(Index: Integer): TRegistroD160;
begin
  Result := TRegistroD160(Inherited Items[Index]);
end;

function TRegistroD160List.New: TRegistroD160;
begin
  Result := TRegistroD160.Create;
  Add(Result);
end;

procedure TRegistroD160List.SetItem(Index: Integer; const Value: TRegistroD160);
begin
  Put(Index, Value);
end;

{ TRegistroD161List }

function TRegistroD161List.GetItem(Index: Integer): TRegistroD161;
begin
  Result := TRegistroD161(Inherited Items[Index]);
end;

function TRegistroD161List.New: TRegistroD161;
begin
  Result := TRegistroD161.Create;
  Add(Result);
end;

procedure TRegistroD161List.SetItem(Index: Integer; const Value: TRegistroD161);
begin
  Put(Index, Value);
end;

{ TRegistroD162List }

function TRegistroD162List.GetItem(Index: Integer): TRegistroD162;
begin
  Result := TRegistroD162(Inherited Items[Index]);
end;

function TRegistroD162List.New: TRegistroD162;
begin
  Result := TRegistroD162.Create;
  Add(Result);
end;

procedure TRegistroD162List.SetItem(Index: Integer; const Value: TRegistroD162);
begin
  Put(Index, Value);
end;

{ TRegistroD170List }

function TRegistroD170List.GetItem(Index: Integer): TRegistroD170;
begin
  Result := TRegistroD170(Inherited Items[Index]);
end;

function TRegistroD170List.New: TRegistroD170;
begin
  Result := TRegistroD170.Create;
  Add(Result);
end;

procedure TRegistroD170List.SetItem(Index: Integer; const Value: TRegistroD170);
begin
  Put(Index, Value);
end;

{ TRegistroD180List }

function TRegistroD180List.GetItem(Index: Integer): TRegistroD180;
begin
  Result := TRegistroD180(Inherited Items[Index]);
end;

function TRegistroD180List.New: TRegistroD180;
begin
  Result := TRegistroD180.Create;
  Add(Result);
end;

procedure TRegistroD180List.SetItem(Index: Integer; const Value: TRegistroD180);
begin
  Put(Index, Value);
end;

{ TRegistroD190List }

function TRegistroD190List.GetItem(Index: Integer): TRegistroD190;
begin
  Result := TRegistroD190(Inherited Items[Index]);
end;

function TRegistroD190List.New: TRegistroD190;
begin
  Result := TRegistroD190.Create;
  Add(Result);
end;

procedure TRegistroD190List.SetItem(Index: Integer; const Value: TRegistroD190);
begin
  Put(Index, Value);
end;



{ TRegistroD195List }

function TRegistroD195List.GetItem(Index: Integer): TRegistroD195;
begin
  Result := TRegistroD195(Inherited Items[Index]);
end;

function TRegistroD195List.New: TRegistroD195;
begin
  Result := TRegistroD195.Create;
  Add(Result);
end;

procedure TRegistroD195List.SetItem(Index: Integer; const Value: TRegistroD195);
begin
  Put(Index, Value);
end;

{ TRegistroD197List }

function TRegistroD197List.GetItem(Index: Integer): TRegistroD197;
begin
  Result := TRegistroD197(Inherited Items[Index]);
end;

function TRegistroD197List.New: TRegistroD197;
begin
  Result := TRegistroD197.Create;
  Add(Result);
end;

procedure TRegistroD197List.SetItem(Index: Integer; const Value: TRegistroD197);
begin
  Put(Index, Value);
end;


{ TRegistroD300List }

function TRegistroD300List.GetItem(Index: Integer): TRegistroD300;
begin
  Result := TRegistroD300(Inherited Items[Index]);
end;

function TRegistroD300List.New: TRegistroD300;
begin
  Result := TRegistroD300.Create;
  Add(Result);
end;

procedure TRegistroD300List.SetItem(Index: Integer; const Value: TRegistroD300);
begin
  Put(Index, Value);
end;

{ TRegistroD301List }

function TRegistroD301List.GetItem(Index: Integer): TRegistroD301;
begin
  Result := TRegistroD301(Inherited Items[Index]);
end;

function TRegistroD301List.New: TRegistroD301;
begin
  Result := TRegistroD301.Create;
  Add(Result);
end;

procedure TRegistroD301List.SetItem(Index: Integer; const Value: TRegistroD301);
begin
  Put(Index, Value);
end;

{ TRegistroD310List }

function TRegistroD310List.GetItem(Index: Integer): TRegistroD310;
begin
  Result := TRegistroD310(Inherited Items[Index]);
end;

function TRegistroD310List.New: TRegistroD310;
begin
  Result := TRegistroD310.Create;
  Add(Result);
end;

procedure TRegistroD310List.SetItem(Index: Integer; const Value: TRegistroD310);
begin
  Put(Index, Value);
end;

{ TRegistroD350List }

function TRegistroD350List.GetItem(Index: Integer): TRegistroD350;
begin
  Result := TRegistroD350(Inherited Items[Index]);
end;

function TRegistroD350List.New: TRegistroD350;
begin
  Result := TRegistroD350.Create;
  Add(Result);
end;

procedure TRegistroD350List.SetItem(Index: Integer; const Value: TRegistroD350);
begin
  Put(Index, Value);
end;

{ TRegistroD355List }

function TRegistroD355List.GetItem(Index: Integer): TRegistroD355;
begin
  Result := TRegistroD355(Inherited Items[Index]);
end;

function TRegistroD355List.New: TRegistroD355;
begin
  Result := TRegistroD355.Create;
  Add(Result);
end;

procedure TRegistroD355List.SetItem(Index: Integer; const Value: TRegistroD355);
begin
  Put(Index, Value);
end;

{ TRegistroD360List }

function TRegistroD360List.GetItem(Index: Integer): TRegistroD360;
begin
  Result := TRegistroD360(Inherited Items[Index]);
end;

function TRegistroD360List.New: TRegistroD360;
begin
  Result := TRegistroD360.Create;
  Add(Result);
end;

procedure TRegistroD360List.SetItem(Index: Integer; const Value: TRegistroD360);
begin
  Put(Index, Value);
end;

{ TRegistroD365List }

function TRegistroD365List.GetItem(Index: Integer): TRegistroD365;
begin
  Result := TRegistroD365(Inherited Items[Index]);
end;

function TRegistroD365List.New: TRegistroD365;
begin
  Result := TRegistroD365.Create;
  Add(Result);
end;

procedure TRegistroD365List.SetItem(Index: Integer; const Value: TRegistroD365);
begin
  Put(Index, Value);
end;

{ TRegistroD370List }

function TRegistroD370List.GetItem(Index: Integer): TRegistroD370;
begin
  Result := TRegistroD370(Inherited Items[Index]);
end;

function TRegistroD370List.New: TRegistroD370;
begin
  Result := TRegistroD370.Create;
  Add(Result);
end;

procedure TRegistroD370List.SetItem(Index: Integer; const Value: TRegistroD370);
begin
  Put(Index, Value);
end;

{ TRegistroD390List }

function TRegistroD390List.GetItem(Index: Integer): TRegistroD390;
begin
  Result := TRegistroD390(Inherited Items[Index]);
end;

function TRegistroD390List.New: TRegistroD390;
begin
  Result := TRegistroD390.Create;
  Add(Result);
end;

procedure TRegistroD390List.SetItem(Index: Integer; const Value: TRegistroD390);
begin
  Put(Index, Value);
end;

{ TRegistroD400List }

function TRegistroD400List.GetItem(Index: Integer): TRegistroD400;
begin
  Result := TRegistroD400(Inherited Items[Index]);
end;

function TRegistroD400List.New: TRegistroD400;
begin
  Result := TRegistroD400.Create;
  Add(Result);
end;

procedure TRegistroD400List.SetItem(Index: Integer; const Value: TRegistroD400);
begin
  Put(Index, Value);
end;

{ TRegistroD410List }

function TRegistroD410List.GetItem(Index: Integer): TRegistroD410;
begin
  Result := TRegistroD410(Inherited Items[Index]);
end;

function TRegistroD410List.New: TRegistroD410;
begin
  Result := TRegistroD410.Create;
  Add(Result);
end;

procedure TRegistroD410List.SetItem(Index: Integer; const Value: TRegistroD410);
begin
  Put(Index, Value);
end;

{ TRegistroD411List }

function TRegistroD411List.GetItem(Index: Integer): TRegistroD411;
begin
  Result := TRegistroD411(Inherited Items[Index]);
end;

function TRegistroD411List.New: TRegistroD411;
begin
  Result := TRegistroD411.Create;
  Add(Result);
end;

procedure TRegistroD411List.SetItem(Index: Integer; const Value: TRegistroD411);
begin
  Put(Index, Value);
end;

{ TRegistroD420List }

function TRegistroD420List.GetItem(Index: Integer): TRegistroD420;
begin
  Result := TRegistroD420(Inherited Items[Index]);
end;

function TRegistroD420List.New: TRegistroD420;
begin
  Result := TRegistroD420.Create;
  Add(Result);
end;

procedure TRegistroD420List.SetItem(Index: Integer; const Value: TRegistroD420);
begin
  Put(Index, Value);
end;

{ TRegistroD500List }

function TRegistroD500List.GetItem(Index: Integer): TRegistroD500;
begin
  Result := TRegistroD500(Inherited Items[Index]);
end;

function TRegistroD500List.New: TRegistroD500;
begin
  Result := TRegistroD500.Create;
  Add(Result);
end;

procedure TRegistroD500List.SetItem(Index: Integer; const Value: TRegistroD500);
begin
  Put(Index, Value);
end;

{ TRegistroD510List }

function TRegistroD510List.GetItem(Index: Integer): TRegistroD510;
begin
  Result := TRegistroD510(Inherited Items[Index]);
end;

function TRegistroD510List.New: TRegistroD510;
begin
  Result := TRegistroD510.Create;
  Add(Result);
end;

procedure TRegistroD510List.SetItem(Index: Integer; const Value: TRegistroD510);
begin
  Put(Index, Value);
end;

{ TRegistroD530List }

function TRegistroD530List.GetItem(Index: Integer): TRegistroD530;
begin
  Result := TRegistroD530(Inherited Items[Index]);
end;

function TRegistroD530List.New: TRegistroD530;
begin
  Result := TRegistroD530.Create;
  Add(Result);
end;

procedure TRegistroD530List.SetItem(Index: Integer; const Value: TRegistroD530);
begin
  Put(Index, Value);
end;

{ TRegistroD600List }

function TRegistroD600List.GetItem(Index: Integer): TRegistroD600;
begin
  Result := TRegistroD600(Inherited Items[Index]);
end;

function TRegistroD600List.New: TRegistroD600;
begin
  Result := TRegistroD600.Create;
  Add(Result);
end;

procedure TRegistroD600List.SetItem(Index: Integer; const Value: TRegistroD600);
begin
  Put(Index, Value);
end;

{ TRegistroD610List }

function TRegistroD610List.GetItem(Index: Integer): TRegistroD610;
begin
  Result := TRegistroD610(Inherited Items[Index]);
end;

function TRegistroD610List.New: TRegistroD610;
begin
  Result := TRegistroD610.Create;
  Add(Result);
end;

procedure TRegistroD610List.SetItem(Index: Integer; const Value: TRegistroD610);
begin
  Put(Index, Value);
end;

{ TRegistroD690List }

function TRegistroD690List.GetItem(Index: Integer): TRegistroD690;
begin
  Result := TRegistroD690(Inherited Items[Index]);
end;

function TRegistroD690List.New: TRegistroD690;
begin
  Result := TRegistroD690.Create;
  Add(Result);
end;

procedure TRegistroD690List.SetItem(Index: Integer; const Value: TRegistroD690);
begin
  Put(Index, Value);
end;

{ TRegistroD695List }

function TRegistroD695List.GetItem(Index: Integer): TRegistroD695;
begin
  Result := TRegistroD695(Inherited Items[Index]);
end;

function TRegistroD695List.New: TRegistroD695;
begin
  Result := TRegistroD695.Create;
  Add(Result);
end;

procedure TRegistroD695List.SetItem(Index: Integer; const Value: TRegistroD695);
begin
  Put(Index, Value);
end;

{ TRegistroD696List }

function TRegistroD696List.GetItem(Index: Integer): TRegistroD696;
begin
  Result := TRegistroD696(Inherited Items[Index]);
end;

function TRegistroD696List.New: TRegistroD696;
begin
  Result := TRegistroD696.Create;
  Add(Result);
end;

procedure TRegistroD696List.SetItem(Index: Integer; const Value: TRegistroD696);
begin
  Put(Index, Value);
end;

{ TRegistroD697List }

function TRegistroD697List.GetItem(Index: Integer): TRegistroD697;
begin
  Result := TRegistroD697(Inherited Items[Index]);
end;

function TRegistroD697List.New: TRegistroD697;
begin
  Result := TRegistroD697.Create;
  Add(Result);
end;

procedure TRegistroD697List.SetItem(Index: Integer; const Value: TRegistroD697);
begin
  Put(Index, Value);
end;

{ TRegistroD500 }

constructor TRegistroD500.Create;
begin
  FRegistroD510 := TRegistroD510List.Create;
  FRegistroD530 := TRegistroD530List.Create;
  FRegistroD590 := TRegistroD590List.Create;  /// BLOCO D - Lista de RegistroD590 (FILHO)
end;

destructor TRegistroD500.Destroy;
begin
  FRegistroD510.Free;
  FRegistroD530.Free;
  FRegistroD590.Free;
  inherited;
end;

{ TRegistroC195 }

constructor TRegistroD195.Create;
begin
   FRegistroD197 := TRegistroD197List.Create;
end;

destructor TRegistroD195.Destroy;
begin
  FRegistroD197.Free;
  inherited;
end;

{ TRegistroD100 }

constructor TRegistroD100.Create(AOwner: TRegistroD001);
begin
  FRegistroD101 := TRegistroD101List.Create;
  FRegistroD110 := TRegistroD110List.Create;
  FRegistroD130 := TRegistroD130List.Create;
  FRegistroD140 := TRegistroD140List.Create;
  FRegistroD150 := TRegistroD150List.Create;
  FRegistroD160 := TRegistroD160List.Create;
  FRegistroD170 := TRegistroD170List.Create;
  FRegistroD180 := TRegistroD180List.Create;
  FRegistroD190 := TRegistroD190List.Create;  /// BLOCO D - Lista de RegistroD190 (FILHO)
  FRegistroD195 := TRegistroD195List.Create;  /// BLOCO D - Lista de RegistroD195 
end;

destructor TRegistroD100.Destroy;
begin
  FRegistroD101.Free;
  FRegistroD110.Free;
  FRegistroD130.Free;
  FRegistroD140.Free;
  FRegistroD150.Free;
  FRegistroD160.Free;
  FRegistroD170.Free;
  FRegistroD180.Free;
  FRegistroD190.Free;
  FRegistroD195.Free;
  inherited;
end;

{ TRegistroD001 }

constructor TRegistroD001.Create;
begin
  FRegistroD100 := TRegistroD100List.Create;
  FRegistroD300 := TRegistroD300List.Create;
  FRegistroD350 := TRegistroD350List.Create;
  FRegistroD400 := TRegistroD400List.Create;
  FRegistroD500 := TRegistroD500List.Create;
  FRegistroD600 := TRegistroD600List.Create;
  FRegistroD695 := TRegistroD695List.Create;
  //
  IND_MOV := imSemDados;
end;

destructor TRegistroD001.Destroy;
begin
  FRegistroD100.Free;
  FRegistroD300.Free;
  FRegistroD350.Free;
  FRegistroD400.Free;
  FRegistroD500.Free;
  FRegistroD600.Free;
  FRegistroD695.Free;
  inherited;
end;

{ TRegistroD110 }

constructor TRegistroD110.Create(AOwner: TRegistroD100);
begin
  FRegistroD120 := TRegistroD120List.Create;
end;

destructor TRegistroD110.Destroy;
begin
  FRegistroD120.Free;
  inherited;
end;

{ TRegistroD300 }

constructor TRegistroD300.Create;
begin
  FRegistroD301 := TRegistroD301List.Create;
  FRegistroD310 := TRegistroD310List.Create;
end;

destructor TRegistroD300.Destroy;
begin
  RegistroD310.Free;
  inherited;
end;

{ TRegistroD350 }

constructor TRegistroD350.Create;
begin
  FRegistroD355 := TRegistroD355List.Create;
end;

destructor TRegistroD350.Destroy;
begin
  RegistroD355.Free;
  inherited;
end;

{ TRegistroD355 }

constructor TRegistroD355.Create;
begin
  FRegistroD360 := TRegistroD360List.Create;
  FRegistroD365 := TRegistroD365List.Create;
  FRegistroD390 := TRegistroD390List.Create;
end;

destructor TRegistroD355.Destroy;
begin
  FRegistroD360.Free;
  FRegistroD365.Free;
  FRegistroD390.Free;
  inherited;
end;

{ TRegistroD365 }

constructor TRegistroD365.Create;
begin
  FRegistroD370 := TRegistroD370List.Create;
end;

destructor TRegistroD365.Destroy;
begin
  RegistroD370.Free;
  inherited;
end;

{ TRegistroD400 }

constructor TRegistroD400.Create;
begin
  FRegistroD410 := TRegistroD410List.Create;
  FRegistroD420 := TRegistroD420List.Create;
end;

destructor TRegistroD400.Destroy;
begin
  FRegistroD410.Free;
  FRegistroD420.Free;
  inherited;
end;

{ TRegistroD410 }

constructor TRegistroD410.Create;
begin
  FRegistroD411 := TRegistroD411List.Create;
end;

destructor TRegistroD410.Destroy;
begin
  FRegistroD411.Free;
  inherited;
end;

{ TRegistroD600 }

constructor TRegistroD600.Create;
begin
  FRegistroD610 := TRegistroD610List.Create;
  FRegistroD690 := TRegistroD690List.Create;
end;

destructor TRegistroD600.Destroy;
begin
  FRegistroD610.Free;
  FRegistroD690.Free;
  inherited;
end;

{ TRegistroD695 }

constructor TRegistroD695.Create;
begin
  FRegistroD696 := TRegistroD696List.Create;
end;

destructor TRegistroD695.Destroy;
begin
  FRegistroD696.Free;
  inherited;
end;

{ TRegistroD696 }

constructor TRegistroD696.Create;
begin
  FRegistroD697 := TRegistroD697List.Create;
end;

destructor TRegistroD696.Destroy;
begin
  FRegistroD697.Free;
  inherited;
end;

{ TRegistroD160 }

constructor TRegistroD160.Create;
begin
  FRegistroD161 := TRegistroD161List.Create;
  FRegistroD162 := TRegistroD162List.Create;
end;

destructor TRegistroD160.Destroy;
begin
  FRegistroD161.Free;
  FRegistroD162.Free;
  inherited;
end;

{ TRegistroD190 }

procedure TRegistroD190.SetCFOP(const Value: String);
begin
  if FCFOP <> Value then
     FCFOP := TiraPontos(Value);
end;

end.
