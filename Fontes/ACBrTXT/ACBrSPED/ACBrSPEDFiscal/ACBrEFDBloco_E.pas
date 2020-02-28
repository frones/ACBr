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

unit ACBrEFDBloco_E;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEFDBlocos;

type
  TRegistroE100List = class;
  TRegistroE110 = class;
  TRegistroE111List = class;
  TRegistroE112List = class;
  TRegistroE113List = class;
  TRegistroE115List = class;
  TRegistroE116List = class;
  TRegistroE200List = class;
  TRegistroE210List = class;
  TRegistroE220List = class;
  TRegistroE230List = class;
  TRegistroE240List = class;
  TRegistroE250List = class;
  TRegistroE300List = class;
  TRegistroE310List = class;
  TRegistroE311List = class;
  TRegistroE312List = class;
  TRegistroE313List = class;
  TRegistroE316List = class;
  TRegistroE500List = class;
  TRegistroE510List = class;
  TRegistroE520List = class;
  TRegistroE530List = class;
  TRegistroE531List = class;

  /// Registro E001 - ABERTURA DO BLOCO E

  TRegistroE001 = class(TOpenBlocos)
  private
    FRegistroE100: TRegistroE100List;
    FRegistroE200: TRegistroE200List;
    FRegistroE300: TRegistroE300List;
    FRegistroE500: TRegistroE500List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroE100: TRegistroE100List read FRegistroE100 write FRegistroE100;
    property RegistroE200: TRegistroE200List read FRegistroE200 write FRegistroE200;
    property RegistroE300: TRegistroE300List read FRegistroE300 write FRegistroE300;
    property RegistroE500: TRegistroE500List read FRegistroE500 write FRegistroE500;
  end;

  /// Registro E100 - PERÍODO DA APURAÇÃO DO ICMS

  TRegistroE100 = class
  private
    fDT_INI: TDateTime;       /// Data inicial a que a apuração se refere:
    fDT_FIN: TDateTime;       /// Data final a que a apuração se refere:

    FRegistroE110: TRegistroE110;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;

    property RegistroE110: TRegistroE110 read FRegistroE110 write FRegistroE110;
  end;

  TRegistroE100List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroE100;              /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE100); /// SetItem
  public
    function New(): TRegistroE100;
    property Items[Index: Integer]: TRegistroE100 read GetItem write SetItem;
  end;

  /// Registro E110 - APURAÇÃO DO ICMS - OPERAÇÕES PRÓPRIAS

  TRegistroE110 = class
  private
    fVL_TOT_DEBITOS: Currency;            /// Valor total dos débitos por "Saídas e prestações com débito do imposto"
    fVL_AJ_DEBITOS: Currency;             /// Valor total dos ajustes a débito decorrentes do documento fiscal.
    fVL_TOT_AJ_DEBITOS: Currency;         /// Valor total de "Ajustes a débito"
    fVL_ESTORNOS_CRED: Currency;          /// Valor total de Ajustes “Estornos de créditos”
    fVL_TOT_CREDITOS: Currency;           /// Valor total dos créditos por "Entradas e aquisições com crédito do imposto"
    fVL_AJ_CREDITOS: Currency;            /// Valor total dos ajustes a crédito decorrentes do documento fiscal.
    fVL_TOT_AJ_CREDITOS: Currency;        /// Valor total de "Ajustes a crédito"
    fVL_ESTORNOS_DEB: Currency;           /// Valor total de Ajustes “Estornos de Débitos”
    fVL_SLD_CREDOR_ANT: Currency;         /// Valor total de "Saldo credor do período anterior"
    fVL_SLD_APURADO: Currency;            /// Valor total de "Saldo devedor (02+03+04+05-06-07-08-09-10) antes das deduções"
    fVL_TOT_DED: Currency;                /// Valor total de "Deduções"
    fVL_ICMS_RECOLHER: Currency;          /// Valor total de "ICMS a recolher (11-12)
    fVL_SLD_CREDOR_TRANSPORTAR: Currency; /// Valor total de "Saldo credor a transportar para o período seguinte”
    fDEB_ESP: Currency;                   /// Valores recolhidos ou a recolher, extra-apuração.

    FRegistroE111: TRegistroE111List;
    FRegistroE115: TRegistroE115List;
    FRegistroE116: TRegistroE116List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property VL_TOT_DEBITOS: Currency read fVL_TOT_DEBITOS write fVL_TOT_DEBITOS;
    property VL_AJ_DEBITOS: Currency read fVL_AJ_DEBITOS write fVL_AJ_DEBITOS;
    property VL_TOT_AJ_DEBITOS: Currency read fVL_TOT_AJ_DEBITOS write fVL_TOT_AJ_DEBITOS;
    property VL_ESTORNOS_CRED: Currency read fVL_ESTORNOS_CRED write fVL_ESTORNOS_CRED;
    property VL_TOT_CREDITOS: Currency read fVL_TOT_CREDITOS write fVL_TOT_CREDITOS;
    property VL_AJ_CREDITOS: Currency read fVL_AJ_CREDITOS write fVL_AJ_CREDITOS;
    property VL_TOT_AJ_CREDITOS: Currency read fVL_TOT_AJ_CREDITOS write fVL_TOT_AJ_CREDITOS;
    property VL_ESTORNOS_DEB: Currency read fVL_ESTORNOS_DEB write fVL_ESTORNOS_DEB;
    property VL_SLD_CREDOR_ANT: Currency read fVL_SLD_CREDOR_ANT write fVL_SLD_CREDOR_ANT;
    property VL_SLD_APURADO: Currency read fVL_SLD_APURADO write fVL_SLD_APURADO;
    property VL_TOT_DED: Currency read fVL_TOT_DED write fVL_TOT_DED;
    property VL_ICMS_RECOLHER: Currency read fVL_ICMS_RECOLHER write fVL_ICMS_RECOLHER;
    property VL_SLD_CREDOR_TRANSPORTAR: Currency read fVL_SLD_CREDOR_TRANSPORTAR write fVL_SLD_CREDOR_TRANSPORTAR;
    property DEB_ESP: Currency read fDEB_ESP write fDEB_ESP;

    property RegistroE111: TRegistroE111List read FRegistroE111 write FRegistroE111;
    property RegistroE115: TRegistroE115List read FRegistroE115 write FRegistroE115;
    property RegistroE116: TRegistroE116List read FRegistroE116 write FRegistroE116;
  end;

  /// Registro E111 - AJUSTE/BENEFÍCIO/INCENTIVO DA APURAÇÃO DO ICMS

  TRegistroE111 = class
  private
    fCOD_AJ_APUR: String;     /// Código do ajuste da apuração e dedução, conforme a Tabela indicada no item 5.1.1.
    fDESCR_COMPL_AJ: String;  /// Descrição complementar do ajuste da apuração.
    fVL_AJ_APUR: currency;        /// Valor do ajuste da apuração

    FRegistroE112: TRegistroE112List;
    FRegistroE113: TRegistroE113List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_AJ_APUR: String read fCOD_AJ_APUR write fCOD_AJ_APUR;
    property DESCR_COMPL_AJ: String read fDESCR_COMPL_AJ write fDESCR_COMPL_AJ;
    property VL_AJ_APUR: currency read fVL_AJ_APUR write fVL_AJ_APUR;

    property RegistroE112: TRegistroE112List read FRegistroE112 write FRegistroE112;
    property RegistroE113: TRegistroE113List read FRegistroE113 write FRegistroE113;
  end;

  /// Registro E111 - Lista

  TRegistroE111List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE111; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE111); /// SetItem
  public
    function New(): TRegistroE111;
    property Items[Index: Integer]: TRegistroE111 read GetItem write SetItem;
  end;

  /// Registro E112 - INFORMAÇÕES ADICIONAIS DOS AJUSTES DA APURAÇÃO DO ICMS

  TRegistroE112 = class
  private
    fNUM_DA: String;                /// Número do documento de arrecadação estadual, se houver
    fNUM_PROC: String;              /// Número do processo ao qual o ajuste está vinculado, se houver
    fIND_PROC: TACBrOrigemProcesso; /// Indicador da origem do processo: 0- Sefaz; 1- Justiça Federal; 2- Justiça Estadual; 9- Outros
    fPROC: String;                  /// Descrição resumida do processo que embasou o lançamento
    fTXT_COMPL: String;             /// Código de referência à observação (campo 02 do Registro 0460)
  public
    constructor Create(AOwner: TRegistroE111); virtual; /// Create

    property NUM_DA: String read fNUM_DA write fNUM_DA;
    property NUM_PROC: String read fNUM_PROC write fNUM_PROC;
    property IND_PROC: TACBrOrigemProcesso read fIND_PROC write fIND_PROC;
    property PROC: String read fPROC write fPROC;
    property TXT_COMPL: String read fTXT_COMPL write fTXT_COMPL;
  end;

  /// Registro E112 - Lista

  TRegistroE112List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE112; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE112); /// SetItem
  public
    function New(AOwner: TRegistroE111): TRegistroE112;
    property Items[Index: Integer]: TRegistroE112 read GetItem write SetItem;
  end;

  /// Registro E113 - INFORMAÇÕES ADICIONAIS DOS AJUSTES DA APURAÇÃO DO ICMS - IDENTIFICAÇÃO DOS DOCUMENTOS FISCAIS

  TRegistroE113 = class
  private
    fCOD_PART: String;        /// Código do participante (campo 02 do Registro 0150): Do emitente do documento ou do remetente das mercadorias, no caso de entradas; Do adquirente, no caso de saídas
    fCOD_MOD: String;         /// Código do modelo do documento fiscal, conforme a Tabela 4.1.1
    fSER: String;             /// Série do documento fiscal
    fSUB: String;             /// Subserie do documento fiscal
    fNUM_DOC: String;         /// Número do documento fiscal
    fDT_DOC: TDateTime;       /// Data da emissão do documento fiscal
    fCHV_NFE: String;         /// Chave da Nota Fiscal Eletrônica - Versões abaixo de 004
    fCOD_ITEM: String;        /// Código do item (campo 02 do Registro 0200)
    fVL_AJ_ITEM: currency;    /// Valor do ajuste para a operação/item
  public
    constructor Create(AOwner: TRegistroE111); virtual; /// Create

    property COD_PART: String read fCOD_PART write fCOD_PART;
    property COD_MOD: String read fCOD_MOD write fCOD_MOD;
    property SER: String read fSER write fSER;
    property SUB: String read fSUB write fSUB;
    property NUM_DOC: String read fNUM_DOC write fNUM_DOC;
    property DT_DOC: TDateTime read fDT_DOC write fDT_DOC;
    property CHV_NFE: String read fCHV_NFE write fCHV_NFE;
    property COD_ITEM: String read fCOD_ITEM write fCOD_ITEM;
    property VL_AJ_ITEM: currency read fVL_AJ_ITEM write fVL_AJ_ITEM;
  end;

  /// Registro E113 - Lista

  TRegistroE113List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE113; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE113); /// SetItem
  public
    function New(AOwner: TRegistroE111): TRegistroE113;
    property Items[Index: Integer]: TRegistroE113 read GetItem write SetItem;
  end;

  /// Registro E115 - INFORMAÇÕES ADICIONAIS DA APURAÇÃO - VALORES DECLARATÓRIOS.

  TRegistroE115 = class
  private
    fCOD_INF_ADIC: String;    /// Código da informação adicional conforme tabela a ser definida pelas SEFAZ, conforme tabela definida no item 5.2.
    fVL_INF_ADIC: currency;   /// Valor referente à informação adicional
    fDESCR_COMPL_AJ: String;  /// Descrição complementar do ajuste
  public
    constructor Create(AOwner: TRegistroE110); virtual; /// Create

    property COD_INF_ADIC: String read fCOD_INF_ADIC write fCOD_INF_ADIC;
    property VL_INF_ADIC: currency read fVL_INF_ADIC write fVL_INF_ADIC;
    property DESCR_COMPL_AJ: String read fDESCR_COMPL_AJ write fDESCR_COMPL_AJ;
  end;

  /// Registro E115 - Lista

  TRegistroE115List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE115; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE115); /// SetItem
  public
    function New(AOwner: TRegistroE110): TRegistroE115;
    property Items[Index: Integer]: TRegistroE115 read GetItem write SetItem;
  end;

  /// Registro E116 - OBRIGAÇÕES DO ICMS A RECOLHER - OPERAÇÕES PRÓPRIAS

  TRegistroE116 = class
  private
    fCOD_OR: String;                /// Código da obrigação a recolher, conforme a Tabela 5.4
    fVL_OR: currency;               /// Valor da obrigação a recolher
    fDT_VCTO: TDateTime;            /// Data de vencimento da obrigação
    fCOD_REC: String;               /// Código de receita referente à obrigação, próprio da unidade da federação, conforme legislação estadual,
    fNUM_PROC: String;              /// Número do processo ou auto de infração ao qual a obrigação está vinculada, se houver.
    fIND_PROC: TACBrOrigemProcesso; /// Indicador da origem do processo: 0- Sefaz; 1- Justiça Federal; 2- Justiça Estadual; 9- Outros
    fPROC: String;                  /// Descrição resumida do processo que embasou o lançamento
    fTXT_COMPL: String;             /// Descrição complementar das obrigações a recolher.
    fMES_REF: string;               /// VERSÃO 103 : Informe o mês de referência no formato “mmaaaa”
  public
    constructor Create(AOwner: TRegistroE110); virtual; /// Create

    property COD_OR: String read fCOD_OR write fCOD_OR;
    property VL_OR: currency read fVL_OR write fVL_OR;
    property DT_VCTO: TDateTime read fDT_VCTO write fDT_VCTO;
    property COD_REC: String read fCOD_REC write fCOD_REC;
    property NUM_PROC: String read fNUM_PROC write fNUM_PROC;
    property IND_PROC: TACBrOrigemProcesso read fIND_PROC write fIND_PROC;
    property PROC: String read fPROC write fPROC;
    property TXT_COMPL: String read fTXT_COMPL write fTXT_COMPL;
    property MES_REF: String read fMES_REF write fMES_REF;  // VERSÃO 103
  end;

  /// Registro E116 - Lista

  TRegistroE116List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE116; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE116); /// SetItem
  public
    function New(AOwner: TRegistroE110): TRegistroE116;
    property Items[Index: Integer]: TRegistroE116 read GetItem write SetItem;
  end;

  /// Registro E200 - PERÍODO DA APURAÇÃO DO ICMS - SUBSTITUIÇÃO TRIBUTÁRIA

  TRegistroE200 = class
  private
    fUF: String;      /// Sigla da unidade da federação a que se refere a apuração do ICMS ST
    fDT_INI: TDateTime;   /// Data inicial a que a apuração se refere
    fDT_FIN: TDateTime;   /// Data final a que a apuração se refere

    fRegistroE210: TRegistroE210List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property UF: String read fUF write fUF;
    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    // Registro FILHO
    property RegistroE210:TRegistroE210List read fRegistroE210 write fRegistroE210;
  end;

  TRegistroE200List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroE200;              /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE200); /// SetItem
  public
    function New(): TRegistroE200;
    property Items[Index: Integer]: TRegistroE200 read GetItem write SetItem;
  end;

  /// Registro E210 - APURAÇÃO DO ICMS - SUBSTITUIÇÃO TRIBUTÁRIA

  TRegistroE210 = class
  private
    fIND_MOV_ST: TACBRMovimentoST;           /// Indicador de movimento: 0 - Sem operações com ST 1 - Com operações de ST
    fVL_SLD_CRED_ANT_ST: currency;           /// Valor do "Saldo credor de período anterior - Substituição Tributária"
    fVL_DEVOL_ST: currency;                  /// Valor total do ICMS ST de devolução de mercadorias
    fVL_RESSARC_ST: currency;                /// Valor total do ICMS ST de ressarcimentos
    fVL_OUT_CRED_ST: currency;               /// Valor total de Ajustes "Outros créditos ST" e “Estorno de débitos ST”
    fVL_AJ_CREDITOS_ST: currency;            /// Valor total dos ajustes a crédito de ICMS ST, provenientes de ajustes do documento fiscal.
    fVL_RETENCAO_ST: currency;               /// Valor Total do ICMS retido por Substituição Tributária
    fVL_OUT_DEB_ST: currency;                /// Valor Total dos ajustes "Outros débitos ST" " e “Estorno de créditos ST”
    fVL_AJ_DEBITOS_ST: currency;             /// Valor total dos ajustes a débito de ICMS ST, provenientes de ajustes do documento fiscal.
    fVL_SLD_DEV_ANT_ST: currency;            /// Valor total de "Saldo devedor antes das deduções" = (08+09+10)-(03+04+05+06+07)]
    fVL_DEDUCOES_ST: currency;               /// Valor total dos ajustes "Deduções ST"
    fVL_ICMS_RECOL_ST: currency;             /// Imposto a recolher ST (11-12)
    fVL_SLD_CRED_ST_TRANSPORTAR: currency;   /// Saldo credor de ST a transportar para o período seguinte [(03+04+05+06+07)- (08+09+10)].
    fDEB_ESP_ST: currency;                   /// Valores recolhidos ou a recolher, extra-apuração.

    fRegistroE220: TRegistroE220List;
    fRegistroE250: TRegistroE250List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_MOV_ST: TACBRMovimentoST read fIND_MOV_ST write fIND_MOV_ST;
    property VL_SLD_CRED_ANT_ST: currency read fVL_SLD_CRED_ANT_ST write fVL_SLD_CRED_ANT_ST;
    property VL_DEVOL_ST: currency read fVL_DEVOL_ST write fVL_DEVOL_ST;
    property VL_RESSARC_ST: currency read fVL_RESSARC_ST write fVL_RESSARC_ST;
    property VL_OUT_CRED_ST: currency read fVL_OUT_CRED_ST write fVL_OUT_CRED_ST;
    property VL_AJ_CREDITOS_ST: currency read fVL_AJ_CREDITOS_ST write fVL_AJ_CREDITOS_ST;
    property VL_RETENCAO_ST: currency read fVL_RETENCAO_ST write fVL_RETENCAO_ST;
    property VL_OUT_DEB_ST: currency read fVL_OUT_DEB_ST write fVL_OUT_DEB_ST;
    property VL_AJ_DEBITOS_ST: currency read fVL_AJ_DEBITOS_ST write fVL_AJ_DEBITOS_ST;
    property VL_SLD_DEV_ANT_ST: currency read fVL_SLD_DEV_ANT_ST write fVL_SLD_DEV_ANT_ST;
    property VL_DEDUCOES_ST: currency read fVL_DEDUCOES_ST write fVL_DEDUCOES_ST;
    property VL_ICMS_RECOL_ST: currency read fVL_ICMS_RECOL_ST write fVL_ICMS_RECOL_ST;
    property VL_SLD_CRED_ST_TRANSPORTAR: currency read fVL_SLD_CRED_ST_TRANSPORTAR write fVL_SLD_CRED_ST_TRANSPORTAR;
    property DEB_ESP_ST: currency read fDEB_ESP_ST write fDEB_ESP_ST;
    // Registro FILHO
    property RegistroE220: TRegistroE220List read fRegistroE220 write fRegistroE220;
    property RegistroE250: TRegistroE250List read fRegistroE250 write fRegistroE250;
  end;

  /// Registro E210 - Lista

  TRegistroE210List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE210; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE210); /// SetItem
  public
    function New(): TRegistroE210;
    property Items[Index: Integer]: TRegistroE210 read GetItem write SetItem;
  end;

  /// Registro E220 - AJUSTE/BENEFÍCIO/INCENTIVO DA APURAÇÃO DO ICMS SUBSTITUIÇÃO TRIBUTÁRIA

  TRegistroE220 = class
  private
    fCOD_AJ_APUR: String;     /// Código do ajuste da apuração e dedução, conforme a Tabela indicada no item 5.1.1
    fDESCR_COMPL_AJ: String;  /// Descrição complementar do ajuste da apuração
    fVL_AJ_APUR: currency;        /// Valor do ajuste da apuração

    FRegistroE230: TRegistroE230List;
    FRegistroE240: TRegistroE240List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_AJ_APUR: String read fCOD_AJ_APUR write fCOD_AJ_APUR;
    property DESCR_COMPL_AJ: String read fDESCR_COMPL_AJ write fDESCR_COMPL_AJ;
    property VL_AJ_APUR: currency read fVL_AJ_APUR write fVL_AJ_APUR;

    property RegistroE230: TRegistroE230List read FRegistroE230 write FRegistroE230;
    property RegistroE240: TRegistroE240List read FRegistroE240 write FRegistroE240;
  end;

  /// Registro E220 - Lista

  TRegistroE220List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE220; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE220); /// SetItem
  public
    function New(): TRegistroE220;
    property Items[Index: Integer]: TRegistroE220 read GetItem write SetItem;
  end;

  /// Registro E230 - INFORMAÇÕES ADICIONAIS DOS AJUSTES DA APURAÇÃO DO ICMS SUBSTITUIÇÃO TRIBUTÁRIA

  TRegistroE230 = class
  private
    fNUM_DA: String;                /// Número do documento de arrecadação estadual, se houver
    fNUM_PROC: String;              /// Número do processo ao qual o ajuste está vinculado, se houver
    fIND_PROC: TACBrOrigemProcesso; /// Indicador da origem do processo: 0- Sefaz; 1- Justiça Federal; 2- Justiça Estadual; 9- Outros
    fPROC: String;                  /// Descrição resumida do processo que embasou o lançamento
    fTXT_COMPL: String;             /// Código de referência à observação (campo 02 do Registro 0460)
  public
    constructor Create(AOwner: TRegistroE220); virtual; /// Create

    property NUM_DA: String read fNUM_DA write fNUM_DA;
    property NUM_PROC: String read fNUM_PROC write fNUM_PROC;
    property IND_PROC: TACBrOrigemProcesso read fIND_PROC write fIND_PROC;
    property PROC: String read fPROC write fPROC;
    property TXT_COMPL: String read fTXT_COMPL write fTXT_COMPL;
  end;

  /// Registro E230 - Lista

  TRegistroE230List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE230; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE230); /// SetItem
  public
    function New(AOwner: TRegistroE220): TRegistroE230;
    property Items[Index: Integer]: TRegistroE230 read GetItem write SetItem;
  end;

  /// Registro E240 - INFORMAÇÕES ADICIONAIS DOS AJUSTES DA APURAÇÃO DO ICMS SUBSTITUIÇÃO TRIBUTÁRIA - IDENTIFICAÇÃO DOS DOCUMENTOS FISCAIS

  TRegistroE240 = class
  private
    fCOD_PART: String;     /// Código do participante (campo 02 do Registro 0150): Do emitente do documento ou do remetente das mercadorias, no caso de entradas; Do adquirente, no caso de saídas
    fCOD_MOD: String;      /// Código do modelo do documento fiscal, conforme a Tabela 4.1.1
    fSER: String;          /// Série do documento fiscal
    fSUB: String;          /// Subserie do documento fiscal
    fNUM_DOC: String;      /// Número do documento fiscal
    fDT_DOC: TDateTime;    /// Data da emissão do documento fiscal
    fCHV_NFE: String;      /// Chave da Nota Fiscal Eletrônica
    fCOD_ITEM: String;     /// Código do item (campo 02 do Registro 0200)
    fVL_AJ_ITEM: currency; /// Valor do ajuste para a operação/item
  public
    constructor Create(AOwner: TRegistroE220); virtual; /// Create

    property COD_PART: String read fCOD_PART write fCOD_PART;
    property COD_MOD: String read fCOD_MOD write fCOD_MOD;
    property SER: String read fSER write fSER;
    property SUB: String read fSUB write fSUB;
    property NUM_DOC: String read fNUM_DOC write fNUM_DOC;
    property DT_DOC: TDateTime read fDT_DOC write fDT_DOC;
    property CHV_NFE: String read fCHV_NFE write fCHV_NFE;
    property COD_ITEM: String read fCOD_ITEM write fCOD_ITEM;
    property VL_AJ_ITEM: currency read fVL_AJ_ITEM write fVL_AJ_ITEM;
  end;

  /// Registro E240 - Lista

  TRegistroE240List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE240; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE240); /// SetItem
  public
    function New(AOwner: TRegistroE220): TRegistroE240;
    property Items[Index: Integer]: TRegistroE240 read GetItem write SetItem;
  end;

  /// Registro E250 - OBRIGAÇÕES DO ICMS A RECOLHER - SUBSTITUIÇÃO TRIBUTÁRIA

  TRegistroE250 = class
  private
    fCOD_OR: String;                /// Código da obrigação a recolher, conforme a Tabela 5.4
    fVL_OR: currency;               /// Valor da obrigação ICMS ST a recolher
    fDT_VCTO: TDateTime;            /// Data de vencimento da obrigação
    fCOD_REC: String;               /// Código de receita referente à obrigação, próprio da unidade da federação
    fNUM_PROC: String;              /// Número do processo ou auto de infração ao qual a obrigação está vinculada, se houver
    fIND_PROC: TACBrOrigemProcesso; /// Indicador da origem do processo: 0- Sefaz; 1- Justiça Federal; 2- Justiça Estadual; 9- Outros
    fPROC: String;                  /// Descrição resumida do processo que embasou o lançamento
    fTXT_COMPL: String;             /// Descrição complementar das obrigações a recolher
    fMES_REF: string;               /// VERSÃO 103 : Informe o mês de referência no formato “mmaaaa”
  public
    constructor Create(AOwner: TRegistroE210); virtual; /// Create

    property COD_OR: String read fCOD_OR write fCOD_OR;
    property VL_OR: currency read fVL_OR write fVL_OR;
    property DT_VCTO: TDateTime read fDT_VCTO write fDT_VCTO;
    property COD_REC: String read fCOD_REC write fCOD_REC;
    property NUM_PROC: String read fNUM_PROC write fNUM_PROC;
    property IND_PROC: TACBrOrigemProcesso read fIND_PROC write fIND_PROC;
    property PROC: String read fPROC write fPROC;
    property TXT_COMPL: String read fTXT_COMPL write fTXT_COMPL;
    property MES_REF: String read fMES_REF write fMES_REF;  // VERSÃO 103
  end;

  /// Registro E250 - Lista

  TRegistroE250List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE250; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE250); /// SetItem
  public
    function New(AOwner: TRegistroE210): TRegistroE250;
    property Items[Index: Integer]: TRegistroE250 read GetItem write SetItem;
  end;


  /// Registro E300 - PERÍODO DA APURAÇÃO DO ICMS DIFERENCIAL DE ALÍQUOTA - UF ORIGEM/DESTINO EC 87/15
  TRegistroE300 = class
  private
    fUF: String;      /// Sigla da unidade da federação a que se refere a apuração
    fDT_INI: TDateTime;   /// Data inicial a que a apuração se refere
    fDT_FIN: TDateTime;   /// Data final a que a apuração se refere

    fRegistroE310: TRegistroE310List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property UF: String read fUF write fUF;
    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    // Registro FILHO
    property RegistroE310:TRegistroE310List read fRegistroE310 write fRegistroE310;
  end;

  TRegistroE300List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroE300;              /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE300); /// SetItem
  public
    function New(): TRegistroE300;
    property Items[Index: Integer]: TRegistroE300 read GetItem write SetItem;
  end;

  /// Registro E310 - APURAÇÃO DO ICMS DIFERENCIAL DE ALÍQUOTA - UF ORIGEM/DESTINO EC 87/15
  TRegistroE310 = class
  private
    fIND_MOV_DIFAL: TACBrMovimentoDIFAL;
    fVL_SLD_CRED_ANT_DIF: currency;
    fVL_TOT_DEBITOS_DIFAL: currency;
    fVL_OUT_DEB_DIFAL: currency;
    fVL_TOT_DEB_FCP: currency;
    fVL_TOT_CREDITOS_DIFAL: currency;
    fVL_TOT_CRED_FCP: currency;
    fVL_OUT_CRED_DIFAL: currency;
    fVL_SLD_DEV_ANT_DIFAL: currency;
    fVL_DEDUCOES_DIFAL: currency;
    fVL_RECOL: currency;
    fVL_SLD_CRED_TRANSPORTAR: currency;
    fDEB_ESP_DIFAL: currency;
    fRegistroE311: TRegistroE311List;
    fRegistroE316: TRegistroE316List;
    FVL_SLD_DEV_ANT_FCP: Currency;
    FVL_RECOL_DIFAL: Currency;
    FVL_SLD_CRED_TRANSPORTAR_DIFAL: Currency;
    FVL_SLD_CRED_ANT_FCP: Currency;
    FVL_RECOL_FCP: Currency;
    FVL_SLD_CRED_TRANSPORTAR_FCP: Currency;
    FVL_OUT_CRED_FCP: Currency;
    FVL_OUT_DEB_FCP: Currency;
    FVL_DEDUCOES_FCP: Currency;
    FDEB_ESP_FCP: Currency;
  public
    constructor Create(); virtual;
    destructor Destroy; override;
    property IND_MOV_DIFAL: TACBrMovimentoDIFAL read fIND_MOV_DIFAL write fIND_MOV_DIFAL;

    property VL_SLD_CRED_ANT_DIF: currency read fVL_SLD_CRED_ANT_DIF write fVL_SLD_CRED_ANT_DIF;
    property VL_TOT_DEBITOS_DIFAL: currency read fVL_TOT_DEBITOS_DIFAL write fVL_TOT_DEBITOS_DIFAL;
    property VL_OUT_DEB_DIFAL: currency read fVL_OUT_DEB_DIFAL write fVL_OUT_DEB_DIFAL;
    property VL_TOT_CREDITOS_DIFAL: currency read fVL_TOT_CREDITOS_DIFAL write fVL_TOT_CREDITOS_DIFAL;
    property VL_OUT_CRED_DIFAL: currency read fVL_OUT_CRED_DIFAL write fVL_OUT_CRED_DIFAL;
    property VL_SLD_DEV_ANT_DIFAL: currency read fVL_SLD_DEV_ANT_DIFAL write fVL_SLD_DEV_ANT_DIFAL;
    property VL_DEDUCOES_DIFAL: currency read fVL_DEDUCOES_DIFAL write fVL_DEDUCOES_DIFAL;
    property DEB_ESP_DIFAL: currency read fDEB_ESP_DIFAL write fDEB_ESP_DIFAL;

    // apartir de 01/01/2017
    property VL_RECOL_DIFAL: Currency read FVL_RECOL_DIFAL write FVL_RECOL_DIFAL;
    property VL_SLD_CRED_TRANSPORTAR_DIFAL: Currency read FVL_SLD_CRED_TRANSPORTAR_DIFAL write FVL_SLD_CRED_TRANSPORTAR_DIFAL;

    property VL_SLD_CRED_ANT_FCP: Currency read FVL_SLD_CRED_ANT_FCP write FVL_SLD_CRED_ANT_FCP;
    property VL_TOT_DEB_FCP: currency read fVL_TOT_DEB_FCP write fVL_TOT_DEB_FCP;
    property VL_OUT_DEB_FCP: Currency read FVL_OUT_DEB_FCP write FVL_OUT_DEB_FCP;
    property VL_TOT_CRED_FCP: currency read fVL_TOT_CRED_FCP write fVL_TOT_CRED_FCP;
    property VL_OUT_CRED_FCP: Currency read FVL_OUT_CRED_FCP write FVL_OUT_CRED_FCP;
    property VL_SLD_DEV_ANT_FCP: Currency read FVL_SLD_DEV_ANT_FCP write FVL_SLD_DEV_ANT_FCP;
    property VL_DEDUCOES_FCP: Currency read FVL_DEDUCOES_FCP write FVL_DEDUCOES_FCP;
    property VL_RECOL_FCP: Currency read FVL_RECOL_FCP write FVL_RECOL_FCP;
    property VL_SLD_CRED_TRANSPORTAR_FCP: Currency read FVL_SLD_CRED_TRANSPORTAR_FCP write FVL_SLD_CRED_TRANSPORTAR_FCP;
    property DEB_ESP_FCP: Currency read FDEB_ESP_FCP write FDEB_ESP_FCP;

    // ate 31/12/2016
    property VL_RECOL: currency read fVL_RECOL write fVL_RECOL;
    property VL_SLD_CRED_TRANSPORTAR: currency read fVL_SLD_CRED_TRANSPORTAR write fVL_SLD_CRED_TRANSPORTAR;

    // Registro FILHO
    property RegistroE311: TRegistroE311List read fRegistroE311 write fRegistroE311;
    property RegistroE316: TRegistroE316List read fRegistroE316 write fRegistroE316;
  end;

  /// Registro E310 - Lista

  TRegistroE310List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE310; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE310); /// SetItem
  public
    function New(): TRegistroE310;
    property Items[Index: Integer]: TRegistroE310 read GetItem write SetItem;
  end;

  /// Registro E311 - AJUSTE/BENEFÍCIO/INCENTIVO DA APURAÇÃO DO ICMS DIFERENCIAL DE ALÍQUOTA UF ORIGEM/DESTINO EC 87/15
  TRegistroE311 = class
  private
    fCOD_AJ_APUR: String;     /// Código do ajuste da apuração e dedução, conforme a Tabela indicada no item 5.1.1
    fDESCR_COMPL_AJ: String;  /// Descrição complementar do ajuste da apuração
    fVL_AJ_APUR: currency;        /// Valor do ajuste da apuração

    FRegistroE312: TRegistroE312List;
    FRegistroE313: TRegistroE313List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_AJ_APUR: String read fCOD_AJ_APUR write fCOD_AJ_APUR;
    property DESCR_COMPL_AJ: String read fDESCR_COMPL_AJ write fDESCR_COMPL_AJ;
    property VL_AJ_APUR: currency read fVL_AJ_APUR write fVL_AJ_APUR;

    property RegistroE312: TRegistroE312List read FRegistroE312 write FRegistroE312;
    property RegistroE313: TRegistroE313List read FRegistroE313 write FRegistroE313;
  end;

  /// Registro E312 - Lista

  TRegistroE311List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE311; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE311); /// SetItem
  public
    function New(): TRegistroE311;
    property Items[Index: Integer]: TRegistroe311 read GetItem write SetItem;
  end;

  /// Registro E312 - INFORMAÇÕES ADICIONAIS DOS AJUSTES DA APURAÇÃO DO ICMS DIFERENCIAL DE ALÍQUOTA UF ORIGEM/DESTINO EC 87/15
  TRegistroE312 = class
  private
    fNUM_DA: String;                /// Número do documento de arrecadação estadual, se houver
    fNUM_PROC: String;              /// Número do processo ao qual o ajuste está vinculado, se houver
    fIND_PROC: TACBrOrigemProcesso; /// Indicador da origem do processo: 0- Sefaz; 1- Justiça Federal; 2- Justiça Estadual; 9- Outros
    fPROC: String;                  /// Descrição resumida do processo que embasou o lançamento
    fTXT_COMPL: String;             /// Descrição Complementar
  public
    constructor Create(AOwner: TRegistroE311); virtual; /// Create

    property NUM_DA: String read fNUM_DA write fNUM_DA;
    property NUM_PROC: String read fNUM_PROC write fNUM_PROC;
    property IND_PROC: TACBrOrigemProcesso read fIND_PROC write fIND_PROC;
    property PROC: String read fPROC write fPROC;
    property TXT_COMPL: String read fTXT_COMPL write fTXT_COMPL;
  end;

  /// Registro E312 - Lista
  TRegistroE312List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE312; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE312); /// SetItem
  public
    function New(AOwner: TRegistroE311): TRegistroE312;
    property Items[Index: Integer]: TRegistroE312 read GetItem write SetItem;
  end;

  /// Registro E313 - INFORMAÇÕES ADICIONAIS DOS AJUSTES DA APURAÇÃO DO ICMS DIFERENCIAL DE ALÍQUOTA UF ORIGEM/DESTINO EC 87/15 - IDENTIFICAÇÃO DOS DOCUMENTOS FISCAIS
  TRegistroE313 = class
  private
    fCOD_PART: String;     /// Código do participante (campo 02 do Registro 0150): Do emitente do documento ou do remetente das mercadorias, no caso de entradas; Do adquirente, no caso de saídas
    fCOD_MOD: String;      /// Código do modelo do documento fiscal, conforme a Tabela 4.1.1
    fSER: String;          /// Série do documento fiscal
    fSUB: String;          /// Subserie do documento fiscal
    fNUM_DOC: String;      /// Número do documento fiscal
    fDT_DOC: TDateTime;    /// Data da emissão do documento fiscal
    FCHV_DOCe: String;     /// Chave do Documento Eletrônico
    fCOD_ITEM: String;     /// Código do item (campo 02 do Registro 0200)
    fVL_AJ_ITEM: currency; /// Valor do ajuste para a operação/item
  public
    constructor Create(AOwner: TRegistroE311); virtual; /// Create

    property COD_PART: String read fCOD_PART write fCOD_PART;
    property COD_MOD: String read fCOD_MOD write fCOD_MOD;
    property SER: String read fSER write fSER;
    property SUB: String read fSUB write fSUB;
    property NUM_DOC: String read fNUM_DOC write fNUM_DOC;
    property CHV_DOCe: String read fCHV_DOCe write fCHV_DOCe;
    property DT_DOC: TDateTime read fDT_DOC write fDT_DOC;
    property COD_ITEM: String read fCOD_ITEM write fCOD_ITEM;
    property VL_AJ_ITEM: currency read fVL_AJ_ITEM write fVL_AJ_ITEM;
  end;

  /// Registro E313 - Lista
  TRegistroE313List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE313; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE313); /// SetItem
  public
    function New(AOwner: TRegistroE311): TRegistroE313;
    property Items[Index: Integer]: TRegistroE313 read GetItem write SetItem;
  end;

  /// Registro E316 - OBRIGAÇÕES DO ICMS A RECOLHER - DIFERENCIAL DE ALIQUOTA UF ORIGEM/DESTINO EC 87/15
  TRegistroE316 = class
  private
    fCOD_OR: String;                /// Código da obrigação a recolher, conforme a Tabela 5.4
    fVL_OR: currency;               /// Valor da obrigação ICMS ST a recolher
    fDT_VCTO: TDateTime;            /// Data de vencimento da obrigação
    fCOD_REC: String;               /// Código de receita referente à obrigação, próprio da unidade da federação
    fNUM_PROC: String;              /// Número do processo ou auto de infração ao qual a obrigação está vinculada, se houver
    fIND_PROC: TACBrOrigemProcesso; /// Indicador da origem do processo: 0- Sefaz; 1- Justiça Federal; 2- Justiça Estadual; 9- Outros
    fPROC: String;                  /// Descrição resumida do processo que embasou o lançamento
    fTXT_COMPL: String;             /// Descrição complementar das obrigações a recolher
    fMES_REF: string;               /// Informe o mês de referência no formato “mmaaaa”
  public
    constructor Create(AOwner: TRegistroE310); virtual; /// Create

    property COD_OR: String read fCOD_OR write fCOD_OR;
    property VL_OR: currency read fVL_OR write fVL_OR;
    property DT_VCTO: TDateTime read fDT_VCTO write fDT_VCTO;
    property COD_REC: String read fCOD_REC write fCOD_REC;
    property NUM_PROC: String read fNUM_PROC write fNUM_PROC;
    property IND_PROC: TACBrOrigemProcesso read fIND_PROC write fIND_PROC;
    property PROC: String read fPROC write fPROC;
    property TXT_COMPL: String read fTXT_COMPL write fTXT_COMPL;
    property MES_REF: String read fMES_REF write fMES_REF;
  end;

  /// Registro E316 - Lista

  TRegistroE316List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE316; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE316); /// SetItem
  public
    function New(AOwner: TRegistroE310): TRegistroE316;
    property Items[Index: Integer]: TRegistroE316 read GetItem write SetItem;
  end;

  /// Registro E500 - PERÍODO DE APURAÇÃO DO IPI
  TRegistroE500 = class
  private
    fIND_APUR: TACBrApuracaoIPI;  /// Indicador de período de apuração do IPI: 0 - Mensal; 1 - Decendial
    fDT_INI: TDateTime;           /// Data inicial a que a apuração se refere
    fDT_FIN: TDateTime;           /// Data final a que a apuração se refere

    fRegistroE510: TRegistroE510List;
    fRegistroE520: TRegistroE520List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_APUR: TACBrApuracaoIPI read fIND_APUR write fIND_APUR;
    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;

    property RegistroE510: TRegistroE510List read FRegistroE510 write FRegistroE510;
    property RegistroE520: TRegistroE520List read FRegistroE520 write FRegistroE520;
  end;

  /// Registro E500 - Lista

  TRegistroE500List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE500; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE500); /// SetItem
  public
    function New(): TRegistroE500;
    property Items[Index: Integer]: TRegistroE500 read GetItem write SetItem;
  end;

  /// Registro E510 - CONSOLIDAÇÃO DOS VALORES DO IPI

  TRegistroE510 = class
  private
    fCFOP: String;                  /// Código Fiscal de Operação e Prestação do agrupamento de itens
//    fCST_IPI: TACBrSituacaoTribIPI; /// Código da Situação Tributária referente ao IPI, conforme a Tabela indicada no item 4.3.2.
    fCST_IPI: string;               /// Código da Situação Tributária referente ao IPI, conforme a Tabela indicada no item 4.3.2.
    fVL_CONT_IPI: currency;         /// Parcela correspondente ao "Valor Contábil" referente ao CFOP e ao Código de Tributação do IPI
    fVL_BC_IPI: currency;           /// Parcela correspondente ao "Valor da base de cálculo do IPI" referente ao CFOP e ao Código de Tributação do IPI, para operações tributadas
    fVL_IPI: currency;              /// Parcela correspondente ao "Valor do IPI" referente ao CFOP e ao Código de Tributação do IPI, para operações tributadas
  public
    constructor Create(AOwner: TRegistroE500); virtual; /// Create

    property CFOP: String                  read fCFOP        write fCFOP;
//    property CST_IPI: TACBrSituacaoTribIPI read fCST_IPI     write fCST_IPI;
    property CST_IPI: string               read fCST_IPI     write fCST_IPI;
    property VL_CONT_IPI: currency         read fVL_CONT_IPI write fVL_CONT_IPI;
    property VL_BC_IPI: currency           read fVL_BC_IPI   write fVL_BC_IPI;
    property VL_IPI: currency              read fVL_IPI      write fVL_IPI;
  end;

  /// Registro E510 - Lista

  TRegistroE510List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE510; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE510); /// SetItem
  public
    function New(AOwner: TRegistroE500): TRegistroE510;
    property Items[Index: Integer]: TRegistroE510 read GetItem write SetItem;
  end;

  /// Registro E520 - APURAÇÃO DO IPI

  TRegistroE520 = class
  private
    fVL_SD_ANT_IPI: currency; /// Saldo credor do IPI transferido do período anterior
    fVL_DEB_IPI: currency;    /// Valor total dos débitos por "Saídas com débito do imposto"
    fVL_CRED_IPI: currency;   /// Valor total dos créditos por "Entradas e aquisições com crédito do imposto"
    fVL_OD_IPI: currency;     /// Valor de "Outros débitos" do IPI (inclusive estornos de crédito)
    fVL_OC_IPI: currency;     /// Valor de "Outros créditos" do IPI (inclusive estornos de débitos)
    fVL_SC_IPI: currency;     /// Valor do saldo credor do IPI a transportar para o período seguinte
    fVL_SD_IPI: currency;     /// Valor do saldo devedor do IPI a recolher

    fRegistroE530: TRegistroE530List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property VL_SD_ANT_IPI: currency read fVL_SD_ANT_IPI write fVL_SD_ANT_IPI;
    property VL_DEB_IPI: currency read fVL_DEB_IPI write fVL_DEB_IPI;
    property VL_CRED_IPI: currency read fVL_CRED_IPI write fVL_CRED_IPI;
    property VL_OD_IPI: currency read fVL_OD_IPI write fVL_OD_IPI;
    property VL_OC_IPI: currency read fVL_OC_IPI write fVL_OC_IPI;
    property VL_SC_IPI: currency read fVL_SC_IPI write fVL_SC_IPI;
    property VL_SD_IPI: currency read fVL_SD_IPI write fVL_SD_IPI;

    property RegistroE530: TRegistroE530List read FRegistroE530 write FRegistroE530;
  end;

  /// Registro E520 - Lista

  TRegistroE520List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE520; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE520); /// SetItem
  public
    function New(): TRegistroE520;
    property Items[Index: Integer]: TRegistroE520 read GetItem write SetItem;
  end;

  /// Registro E530 - AJUSTES DA APURAÇÃO DO IPI

  TRegistroE530 = class
  private
    fIND_AJ: TACBrTipoAjuste;    /// Indicador do tipo de ajuste: 0- Ajuste a débito; 1- Ajuste a crédito
    fVL_AJ: currency;            /// Valor do ajuste
    fCOD_AJ: String;             /// Código do ajuste da apuração, conforme a Tabela indicada no item 4.5.4.
    fIND_DOC: TACBrOrigemDocto;  /// Indicador da origem do documento vinculado ao ajuste: 0 - Processo Judicial; 1 - Processo Administrativo; 2 - PER/DCOMP; 9 - Outros.
    fNUM_DOC: String;            /// Número do documento / processo / declaração ao qual o ajuste está vinculado, se houver
    fDESCR_AJ: String;           /// Descrição resumida do ajuste.

    fRegistroE531: TRegistroE531List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_AJ: TACBrTipoAjuste read fIND_AJ write fIND_AJ;
    property VL_AJ: currency read fVL_AJ write fVL_AJ;
    property COD_AJ: String read fCOD_AJ write fCOD_AJ;
    property IND_DOC: TACBrOrigemDocto read fIND_DOC write fIND_DOC;
    property NUM_DOC: String read fNUM_DOC write fNUM_DOC;
    property DESCR_AJ: String read fDESCR_AJ write fDESCR_AJ;

    property RegistroE531: TRegistroE531List read FRegistroE531 write FRegistroE531;
  end;

  /// Registro E530 - Lista

  TRegistroE530List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE530; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE530); /// SetItem
  public
    function New(): TRegistroE530;
    property Items[Index: Integer]: TRegistroE530 read GetItem write SetItem;
  end;

  /// Registro E531 - INFORMAÇÕES ADICIONAIS DOS AJUSTES DA APURAÇÃO DO IPI –
  //                  IDENTIFICAÇÃO DOS DOCUMENTOS FISCAIS (01 e 55)

  TRegistroE531 = class
  private
    fCOD_PART: String;        /// Código do participante (campo 02 do Registro 0150): Do emitente do documento ou do remetente das mercadorias, no caso de entradas; Do adquirente, no caso de saídas
    fCOD_MOD: String;         /// Código do modelo do documento fiscal, conforme a Tabela 4.1.1
    fSER: String;             /// Série do documento fiscal
    fSUB: String;             /// Subserie do documento fiscal
    fNUM_DOC: String;         /// Número do documento fiscal
    fDT_DOC: TDateTime;       /// Data da emissão do documento fiscal
    fCHV_NFE: String;         /// Chave da Nota Fiscal Eletrônica - Versões abaixo de 004
    fCOD_ITEM: String;        /// Código do item (campo 02 do Registro 0200)
    fVL_AJ_ITEM: currency;    /// Valor do ajuste para a operação/item
  public
    constructor Create(AOwner: TRegistroE530); virtual; /// Create

    property COD_PART: String read fCOD_PART write fCOD_PART;
    property COD_MOD: String read fCOD_MOD write fCOD_MOD;
    property SER: String read fSER write fSER;
    property SUB: String read fSUB write fSUB;
    property NUM_DOC: String read fNUM_DOC write fNUM_DOC;
    property DT_DOC: TDateTime read fDT_DOC write fDT_DOC;
    property CHV_NFE: String read fCHV_NFE write fCHV_NFE;
    property COD_ITEM: String read fCOD_ITEM write fCOD_ITEM;
    property VL_AJ_ITEM: currency read fVL_AJ_ITEM write fVL_AJ_ITEM;
  end;

  /// Registro E531 - Lista

  TRegistroE531List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE531; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroE531); /// SetItem
  public
    function New(AOwner: TRegistroE530): TRegistroE531;
    property Items[Index: Integer]: TRegistroE531 read GetItem write SetItem;
  end;

  /// Registro E990 - ENCERRAMENTO DO BLOCO E

  TRegistroE990 = class
  private
    fQTD_LIN_E: Integer; /// Quantidade total de linhas do Bloco E
  public
    property QTD_LIN_E: Integer read fQTD_LIN_E write fQTD_LIN_E;
  end;


implementation

{ TRegistroE001 }

{ TRegistroE111List }

function TRegistroE111List.GetItem(Index: Integer): TRegistroE111;
begin
  Result := TRegistroE111(Inherited Items[Index]);
end;

function TRegistroE111List.New(): TRegistroE111;
begin
  Result := TRegistroE111.Create();
  Add(Result);
end;

procedure TRegistroE111List.SetItem(Index: Integer; const Value: TRegistroE111);
begin
  Put(Index, Value);
end;

{ TRegistroE112List }

function TRegistroE112List.GetItem(Index: Integer): TRegistroE112;
begin
  Result := TRegistroE112(Inherited Items[Index]);
end;

function TRegistroE112List.New(AOwner: TRegistroE111): TRegistroE112;
begin
  Result := TRegistroE112.Create(AOwner);
  Add(Result);
end;

procedure TRegistroE112List.SetItem(Index: Integer; const Value: TRegistroE112);
begin
  Put(Index, Value);
end;

{ TRegistroE113List }

function TRegistroE113List.GetItem(Index: Integer): TRegistroE113;
begin
  Result := TRegistroE113(Inherited Items[Index]);
end;

function TRegistroE113List.New(AOwner: TRegistroE111): TRegistroE113;
begin
  Result := TRegistroE113.Create(AOwner);
  Add(Result);
end;

procedure TRegistroE113List.SetItem(Index: Integer; const Value: TRegistroE113);
begin
  Put(Index, Value);
end;

{ TRegistroE115List }

function TRegistroE115List.GetItem(Index: Integer): TRegistroE115;
begin
  Result := TRegistroE115(Inherited Items[Index]);
end;

function TRegistroE115List.New(AOwner: TRegistroE110): TRegistroE115;
begin
  Result := TRegistroE115.Create(AOwner);
  Add(Result);
end;

procedure TRegistroE115List.SetItem(Index: Integer; const Value: TRegistroE115);
begin
  Put(Index, Value);
end;

{ TRegistroE116List }

function TRegistroE116List.GetItem(Index: Integer): TRegistroE116;
begin
  Result := TRegistroE116(Inherited Items[Index]);
end;

function TRegistroE116List.New(AOwner: TRegistroE110): TRegistroE116;
begin
  Result := TRegistroE116.Create(AOwner);
  Add(Result);
end;

procedure TRegistroE116List.SetItem(Index: Integer; const Value: TRegistroE116);
begin
  Put(Index, Value);
end;

{ TRegistroE210List }

function TRegistroE210List.GetItem(Index: Integer): TRegistroE210;
begin
  Result := TRegistroE210(Inherited Items[Index]);
end;

function TRegistroE210List.New(): TRegistroE210;
begin
  Result := TRegistroE210.Create();
  Add(Result);
end;

procedure TRegistroE210List.SetItem(Index: Integer; const Value: TRegistroE210);
begin
  Put(Index, Value);
end;

{ TRegistroE220List }

function TRegistroE220List.GetItem(Index: Integer): TRegistroE220;
begin
  Result := TRegistroE220(Inherited Items[Index]);
end;

function TRegistroE220List.New(): TRegistroE220;
begin
  Result := TRegistroE220.Create();
  Add(Result);
end;

procedure TRegistroE220List.SetItem(Index: Integer; const Value: TRegistroE220);
begin
  Put(Index, Value);
end;

{ TRegistroE230List }

function TRegistroE230List.GetItem(Index: Integer): TRegistroE230;
begin
  Result := TRegistroE230(Inherited Items[Index]);
end;

function TRegistroE230List.New(AOwner: TRegistroE220): TRegistroE230;
begin
  Result := TRegistroE230.Create(AOwner);
  Add(Result);
end;

procedure TRegistroE230List.SetItem(Index: Integer; const Value: TRegistroE230);
begin
  Put(Index, Value);
end;

{ TRegistroE240List }

function TRegistroE240List.GetItem(Index: Integer): TRegistroE240;
begin
  Result := TRegistroE240(Inherited Items[Index]);
end;

function TRegistroE240List.New(AOwner: TRegistroE220): TRegistroE240;
begin
  Result := TRegistroE240.Create(AOwner);
  Add(Result);
end;

procedure TRegistroE240List.SetItem(Index: Integer; const Value: TRegistroE240);
begin
  Put(Index, Value);
end;

{ TRegistroE250List }

function TRegistroE250List.GetItem(Index: Integer): TRegistroE250;
begin
  Result := TRegistroE250(Inherited Items[Index]);
end;

function TRegistroE250List.New(AOwner: TRegistroE210): TRegistroE250;
begin
  Result := TRegistroE250.Create(AOwner);
  Add(Result);
end;

procedure TRegistroE250List.SetItem(Index: Integer; const Value: TRegistroE250);
begin
  Put(Index, Value);
end;

{ TRegistroE500List }

function TRegistroE500List.GetItem(Index: Integer): TRegistroE500;
begin
  Result := TRegistroE500(Inherited Items[Index]);
end;

function TRegistroE500List.New(): TRegistroE500;
begin
  Result := TRegistroE500.Create();
  Add(Result);
end;

procedure TRegistroE500List.SetItem(Index: Integer; const Value: TRegistroE500);
begin
  Put(Index, Value);
end;

{ TRegistroE510List }

function TRegistroE510List.GetItem(Index: Integer): TRegistroE510;
begin
  Result := TRegistroE510(Inherited Items[Index]);
end;

function TRegistroE510List.New(AOwner: TRegistroE500): TRegistroE510;
begin
  Result := TRegistroE510.Create(AOwner);
  Add(Result);
end;

procedure TRegistroE510List.SetItem(Index: Integer; const Value: TRegistroE510);
begin
  Put(Index, Value);
end;

{ TRegistroE520List }

function TRegistroE520List.GetItem(Index: Integer): TRegistroE520;
begin
  Result := TRegistroE520(Inherited Items[Index]);
end;

function TRegistroE520List.New(): TRegistroE520;
begin
  Result := TRegistroE520.Create();
  Add(Result);
end;

procedure TRegistroE520List.SetItem(Index: Integer; const Value: TRegistroE520);
begin
  Put(Index, Value);
end;

{ TRegistroE530List }

function TRegistroE530List.GetItem(Index: Integer): TRegistroE530;
begin
  Result := TRegistroE530(Inherited Items[Index]);
end;

function TRegistroE530List.New(): TRegistroE530;
begin
  Result := TRegistroE530.Create();
  Add(Result);
end;

procedure TRegistroE530List.SetItem(Index: Integer; const Value: TRegistroE530);
begin
  Put(Index, Value);
end;

{ TRegistroE531List }

function TRegistroE531List.GetItem(Index: Integer): TRegistroE531;
begin
  Result := TRegistroE531(Inherited Items[Index]);
end;

function TRegistroE531List.New(AOwner: TRegistroE530): TRegistroE531;
begin
  Result := TRegistroE531.Create(AOwner);
  Add(Result);
end;

procedure TRegistroE531List.SetItem(Index: Integer; const Value: TRegistroE531);
begin
  Put(Index, Value);
end;

{ TRegistroE200List }

function TRegistroE200List.GetItem(Index: Integer): TRegistroE200;
begin
  Result := TRegistroE200(Inherited Items[Index]);
end;

function TRegistroE200List.New(): TRegistroE200;
begin
  Result := TRegistroE200.Create();
  Add(Result);
end;

procedure TRegistroE200List.SetItem(Index: Integer; const Value: TRegistroE200 );
begin
  Put(Index, Value);
end;

{ TRegistroE200 }

constructor TRegistroE200.Create();
begin
   inherited Create;
   FRegistroE210 := TRegistroE210List.Create;   /// BLOCO E - Lista de RegistroE210 (FILHO)
end;

destructor TRegistroE200.Destroy;
begin
  FRegistroE210.Free;
  inherited;
end;

{ TRegistroE210 }

constructor TRegistroE210.Create();
begin
   inherited Create;
   FRegistroE220 := TRegistroE220List.Create;   /// BLOCO E - Lista de RegistroE220 (FILHO)
   FRegistroE250 := TRegistroE250List.Create;   /// BLOCO E - Lista de RegistroE250 (FILHO)
end;

destructor TRegistroE210.Destroy;
begin
  FRegistroE220.Free;
  FRegistroE250.Free;
  inherited;
end;

{ TRegistroE510 }

constructor TRegistroE500.Create();
begin
   inherited Create;
   FRegistroE510 := TRegistroE510List.Create;   /// BLOCO E - Lista de RegistroE510 (FILHO)
   FRegistroE520 := TRegistroE520List.Create;   /// BLOCO E - Lista de RegistroE520 (FILHO)
end;

destructor TRegistroE500.Destroy;
begin
  FRegistroE510.Free;
  FRegistroE520.Free;
  inherited;
end;

{ TRegistroE510 }

constructor TRegistroE510.Create(AOwner: TRegistroE500);
begin
end;

{ TRegistroE100 }

constructor TRegistroE100.Create();
begin
   inherited Create;
   FRegistroE110 := TRegistroE110.Create();
end;

destructor TRegistroE100.Destroy;
begin
  FRegistroE110.Free;
  inherited;
end;

{ TRegistroE110 }

constructor TRegistroE110.Create();
begin
   inherited Create;
   FRegistroE111 := TRegistroE111List.Create;
   FRegistroE115 := TRegistroE115List.Create;
   FRegistroE116 := TRegistroE116List.Create;
end;

destructor TRegistroE110.Destroy;
begin
  FRegistroE111.Free;
  FRegistroE115.Free;
  FRegistroE116.Free;
  inherited;
end;

{ TRegistroE111 }

constructor TRegistroE111.Create();
begin
   inherited Create;
   FRegistroE112 := TRegistroE112List.Create;
   FRegistroE113 := TRegistroE113List.Create;
end;

destructor TRegistroE111.Destroy;
begin
  FRegistroE112.Free;
  FRegistroE113.Free;
  inherited;
end;

{ TRegistroE220 }

constructor TRegistroE220.Create();
begin
   inherited Create;
   FRegistroE230 := TRegistroE230List.Create;
   FRegistroE240 := TRegistroE240List.Create;
end;

destructor TRegistroE220.Destroy;
begin
  FRegistroE230.Free;
  FRegistroE240.Free;
  inherited;
end;

{ TRegistroE520 }

constructor TRegistroE520.Create();
begin
   inherited Create;
   fRegistroE530 := TRegistroE530List.Create;
end;

destructor TRegistroE520.Destroy;
begin
  FRegistroE530.Free;
  inherited;
end;

{ TRegistroE001 }

constructor TRegistroE001.Create;
begin
   inherited Create;
   FRegistroE100 := TRegistroE100List.Create;
   FRegistroE200 := TRegistroE200List.Create;
   FRegistroE300 := TRegistroE300List.Create;
   FRegistroE500 := TRegistroE500List.Create;
   //
   IND_MOV := imSemDados;
end;

destructor TRegistroE001.Destroy;
begin
  FRegistroE100.Free;
  FRegistroE200.Free;
  FRegistroE300.Free;
  FRegistroE500.Free;
  inherited;
end;

{ TRegistroE100List }

function TRegistroE100List.GetItem(Index: Integer): TRegistroE100;
begin
  Result := TRegistroE100(Inherited Items[Index]);
end;

function TRegistroE100List.New(): TRegistroE100;
begin
  Result := TRegistroE100.Create();
  Add(Result);
end;

procedure TRegistroE100List.SetItem(Index: Integer; const Value: TRegistroE100);
begin
  Put(Index, Value);
end;


{ TRegistroE112 }

constructor TRegistroE112.Create(AOwner: TRegistroE111);
begin
end;

{ TRegistroE113 }

constructor TRegistroE113.Create(AOwner: TRegistroE111);
begin
end;

{ TRegistroE115 }

constructor TRegistroE115.Create(AOwner: TRegistroE110);
begin
end;

{ TRegistroE116 }

constructor TRegistroE116.Create(AOwner: TRegistroE110);
begin
end;

{ TRegistroE230 }

constructor TRegistroE230.Create(AOwner: TRegistroE220);
begin
end;

{ TRegistroE240 }

constructor TRegistroE240.Create(AOwner: TRegistroE220);
begin
end;

{ TRegistroE250 }

constructor TRegistroE250.Create(AOwner: TRegistroE210);
begin
end;

{ TRegistroE530 }

constructor TRegistroE530.Create();
begin
   inherited Create;
   fRegistroE531 := TRegistroE531List.Create;
end;

destructor TRegistroE530.Destroy;
begin
  FRegistroE531.Free;
  inherited;
end;

{ TRegistroE310 }

constructor TRegistroE310.Create();
begin
  inherited Create;
  FRegistroE311 := TRegistroE311List.Create;   /// BLOCO E - Lista de RegistroE311 (FILHO)
  FRegistroE316 := TRegistroE316List.Create;   /// BLOCO E - Lista de RegistroE316 (FILHO)
end;

destructor TRegistroE310.Destroy;
begin
  FRegistroE311.Free;
  FRegistroE316.Free;

  inherited;
end;

{ TRegistroE300List }

function TRegistroE300List.GetItem(Index: Integer): TRegistroE300;
begin
  Result := TRegistroE300(Inherited Items[Index]);
end;

function TRegistroE300List.New(): TRegistroE300;
begin
  Result := TRegistroE300.Create();
  Add(Result);
end;

procedure TRegistroE300List.SetItem(Index: Integer; const Value: TRegistroE300);
begin
  Put(Index, Value);
end;

{ TRegistroE300 }

constructor TRegistroE300.Create();
begin
  inherited Create;
  FRegistroE310 := TRegistroE310List.Create;   /// BLOCO E - Lista de RegistroE310 (FILHO)
end;

destructor TRegistroE300.Destroy;
begin
  FRegistroE310.Free;

  inherited;
end;

{ TRegistroE310List }

function TRegistroE310List.GetItem(Index: Integer): TRegistroE310;
begin
  Result := TRegistroE310(Inherited Items[Index]);
end;

function TRegistroE310List.New(): TRegistroE310;
begin
  Result := TRegistroE310.Create();
  Add(Result);
end;

procedure TRegistroE310List.SetItem(Index: Integer; const Value: TRegistroE310);
begin
  Put(Index, Value);
end;

{ TRegistroE311 }

constructor TRegistroE311.Create();
begin
  inherited Create;
  FRegistroE312 := TRegistroE312List.Create;
  FRegistroE313 := TRegistroE313List.Create;
end;

destructor TRegistroE311.Destroy;
begin
  FRegistroE312.Free;
  FRegistroE313.Free;

  inherited;
end;

{ TRegistroE311List }

function TRegistroE311List.GetItem(Index: Integer): TRegistroE311;
begin
  Result := TRegistroE311(Inherited Items[Index]);
end;

function TRegistroE311List.New(): TRegistroE311;
begin
  Result := TRegistroE311.Create();
  Add(Result);
end;

procedure TRegistroE311List.SetItem(Index: Integer; const Value: TRegistroE311);
begin
  Put(Index, Value);
end;

{ TRegistroE312 }

constructor TRegistroE312.Create(AOwner: TRegistroE311);
begin
end;

{ TRegistroE312List }

function TRegistroE312List.GetItem(Index: Integer): TRegistroE312;
begin
  Result := TRegistroE312(Inherited Items[Index]);
end;

function TRegistroE312List.New(AOwner: TRegistroE311): TRegistroE312;
begin
  Result := TRegistroE312.Create(AOwner);
  Add(Result);
end;

procedure TRegistroE312List.SetItem(Index: Integer; const Value: TRegistroE312);
begin
  Put(Index, Value);
end;

{ TRegistroE313 }

constructor TRegistroE313.Create(AOwner: TRegistroE311);
begin
end;

{ TRegistroE313List }

function TRegistroE313List.GetItem(Index: Integer): TRegistroE313;
begin
  Result := TRegistroE313(Inherited Items[Index]);
end;

function TRegistroE313List.New(AOwner: TRegistroE311): TRegistroE313;
begin
  Result := TRegistroE313.Create(AOwner);
  Add(Result);
end;

procedure TRegistroE313List.SetItem(Index: Integer; const Value: TRegistroE313);
begin
  Put(Index, Value);
end;

{ TRegistroE316 }

constructor TRegistroE316.Create(AOwner: TRegistroE310);
begin
end;

{ TRegistroE316List }

function TRegistroE316List.GetItem(Index: Integer): TRegistroE316;
begin
  Result := TRegistroE316(Inherited Items[Index]);
end;

function TRegistroE316List.New(AOwner: TRegistroE310): TRegistroE316;
begin
  Result := TRegistroE316.Create(AOwner);
  Add(Result);
end;

procedure TRegistroE316List.SetItem(Index: Integer; const Value: TRegistroE316);
begin
  Put(Index, Value);
end;

{ TRegistroE531 }

constructor TRegistroE531.Create(AOwner: TRegistroE530);
begin
end;

end.
