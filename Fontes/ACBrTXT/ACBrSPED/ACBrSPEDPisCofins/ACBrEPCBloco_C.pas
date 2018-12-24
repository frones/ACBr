{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010   Isaque Pinheiro                      }
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
|* 14/12/2010: Isaque Pinheiro e Claudio Roberto de Souza
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrEPCBloco_C;

interface

uses
 Classes, Contnrs, ACBrEPCBlocos;

type
  TRegistroC010List = class;
  TRegistroC100List = class;
  TRegistroC110List = class;
  TRegistroC111List = class;
  TRegistroC120List = class;
  TRegistroC170List = class;
  TRegistroC175List = class;
  TRegistroC180List = class;
  TRegistroC181List = class;
  TRegistroC185List = class;
  TRegistroC188List = class;
  TRegistroC190List = class;
  TRegistroC191List = class;
  TRegistroC195List = class;
  TRegistroC198List = class;
  TRegistroC199List = class;
  TRegistroC380List = class;
  TRegistroC381List = class;
  TRegistroC385List = class;
  TRegistroC395List = class;
  TRegistroC396List = class;
  TRegistroC400List = class;
  TRegistroC405List = class;
  TRegistroC481List = class;
  TRegistroC485List = class;
  TRegistroC489List = class;
  TRegistroC490List = class;
  TRegistroC491List = class;
  TRegistroC495List = class;
  TRegistroC499List = class;
  TRegistroC500List = class;
  TRegistroC501List = class;
  TRegistroC505List = class;
  TRegistroC509List = class;
  TRegistroC600List = class;
  TRegistroC601List = class;
  TRegistroC605List = class;
  TRegistroC609List = class;
  TRegistroC800List = class;
  TRegistroC810List = class;
  TRegistroC820List = class;
  TRegistroC830List = class;
  TRegistroC860List = class;
  TRegistroC870List = class;
  TRegistroC880List = class;
  TRegistroC890List = class;


  //REGISTRO C001: ABERTURA DO BLOCO C
  TRegistroC001 = class(TOpenBlocos)
  private
    FRegistroC010         : TRegistroC010List;
  public
    constructor Create; virtual;  // Create
    destructor Destroy; override; // Destroy

    property RegistroC010 : TRegistroC010List read FRegistroC010 write FRegistroC010;
  end;

  //REGISTRO C010: IDENTIFICAÇÃO DO ESTABELECIMENTO

  { TRegistroC010 }

  TRegistroC010 = class
  private
    fCNPJ                 : string;               //02	CNPJ	Número de inscrição do estabelecimento no CNPJ.	N	014*	-
    fIND_ESCRI            : TACBrIndEscrituracao; //03	IND_ESCRI	Indicador da apuração das contribuições e créditos, na escrituração das operações por NF-e e ECF, no período:1 – IndEscriConsolidado     - Apuração com base nos registros de consolidação das operações por NF-e (C180 e C190) e por ECF (C490); 2 – IndEscriIndividualizado - Apuração com base no registro individualizado de NF-e (C100 e C170) e de ECF (C400)

    FRegistroC100         : TRegistroC100List;
    FRegistroC180         : TRegistroC180List;
    FRegistroC190         : TRegistroC190List;
    FRegistroC380         : TRegistroC380List;
    FRegistroC395         : TRegistroC395List;
    FRegistroC400         : TRegistroC400List;
    FRegistroC490         : TRegistroC490List;
    FRegistroC500         : TRegistroC500List;
    FRegistroC600         : TRegistroC600List;
    FRegistroC800         : TRegistroC800List;
    FRegistroC860         : TRegistroC860List;
  public
    constructor Create;  virtual;                 // Create
    destructor  Destroy; override;                // Destroy

    property CNPJ         : string               read FCNPJ         write FCNPJ;
    property IND_ESCRI    : TACBrIndEscrituracao read FIND_ESCRI    write FIND_ESCRI;

    property RegistroC100 : TRegistroC100List    read FRegistroC100 write FRegistroC100;
    property RegistroC180 : TRegistroC180List    read FRegistroC180 write FRegistroC180;
    property RegistroC190 : TRegistroC190List    read FRegistroC190 write FRegistroC190;
    property RegistroC380 : TRegistroC380List    read FRegistroC380 write FRegistroC380;
    property RegistroC395 : TRegistroC395List    read FRegistroC395 write FRegistroC395;
    property RegistroC400 : TRegistroC400List    read FRegistroC400 write FRegistroC400;
    property RegistroC490 : TRegistroC490List    read FRegistroC490 write FRegistroC490;
    property RegistroC500 : TRegistroC500List    read FRegistroC500 write FRegistroC500;
    property RegistroC600 : TRegistroC600List    read FRegistroC600 write FRegistroC600;
    property RegistroC800 : TRegistroC800List    read FRegistroC800 write FRegistroC800;
    property RegistroC860 : TRegistroC860List    read FRegistroC860 write FRegistroC860;
  end;

  // Registro C010 - Lista
  TRegistroC010List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC010;
    procedure SetItem(Index: Integer; const Value: TRegistroC010);
  public
    function New: TRegistroC010;
    property Items[Index: Integer]: TRegistroC010 read GetItem write SetItem;
  end;

  //REGISTRO C100: DOCUMENTO - NOTA FISCAL (CÓDIGO 01), NOTA FISCAL AVULSA (CÓDIGO 1B), NOTA FISCAL DE PRODUTOR (CÓDIGO 04) e NF-e (CÓDIGO 55)

  { TRegistroC100 }

  TRegistroC100 = class
  private
    fIND_OPER              : TACBrTipoOperacao;       /// Indicador do tipo de operação: 0- Entrada; 1- Saída
    fIND_EMIT              : TACBrEmitente;           /// Indicador do emitente do documento fiscal: 0- Emissão própria; 1- Terceiros
    fCOD_PART              : String;                  /// Código do participante (campo 02 do Registro 0150):
    fCOD_MOD               : String;                  /// Código do modelo do documento fiscal, conforme a Tabela 4.1.1
    fCOD_SIT               : TACBrCodSit;             /// Código da situação do documento fiscal, conforme a Tabela 4.1.2
    fSER                   : String;                  /// Série do documento fiscal
    fNUM_DOC               : String;                  /// Número do documento fiscal
    fCHV_NFE               : String;                  /// Chave da Nota Fiscal Eletrônica
    fDT_DOC                : TDateTime;               /// Data da emissão do documento fiscal
    fDT_E_S                : TDateTime;               /// Data da entrada ou da saída
    fVL_DOC                : currency;                /// Valor total do documento fiscal
    fIND_PGTO              : TACBrTipoPagamento;      /// Indicador do tipo de pagamento:
    fVL_DESC               : currency;                /// Valor total do desconto // Prates
    fVL_ABAT_NT            : currency;                /// Abatimento não tributado e não comercial Ex. desconto ICMS nas remessas para ZFM: // Prates
    fVL_MERC               : currency;                /// Valor das mercadorias constantes no documento fiscal
    fIND_FRT               : TACBrTipoFrete;          /// Indicador do tipo do frete:
    fVL_FRT                : currency;                /// Valor do frete indicado no documento fiscal
    fVL_SEG                : currency;                /// Valor do seguro indicado no documento fiscal
    fVL_OUT_DA             : currency;                /// Valor de outras despesas acessórias
    fVL_BC_ICMS            : currency;                /// Valor da base de cálculo do ICMS
    fVL_ICMS               : currency;                /// Valor do ICMS
    fVL_BC_ICMS_ST         : currency;                /// Valor da base de cálculo do ICMS substituição tributária
    fVL_ICMS_ST            : currency;                /// Valor do ICMS retido por substituição tributária
    fVL_IPI                : currency;                /// Valor total do IPI
    fVL_PIS                : currency;                /// Valor total do PIS
    fVL_COFINS             : currency;                /// Valor total da COFINS
    fVL_PIS_ST             : currency;                /// Valor total do PIS retido por substituição tributária
    fVL_COFINS_ST          : currency;                /// Valor total da COFINS retido por substituição tributária

    FRegistroC110          : TRegistroC110List;       /// BLOCO C - Lista de RegistroC110 (FILHO)
    FRegistroC111          : TRegistroC111List;       /// BLOCO C - Lista de RegistroC111 (FILHO)
    FRegistroC120          : TRegistroC120List;       /// BLOCO C - Lista de RegistroC120 (FILHO)
    FRegistroC170          : TRegistroC170List;       /// BLOCO C - Lista de RegistroC170 (FILHO)
    FRegistroC175          : TRegistroC175List;       /// BLOCO C - Lista de RegistroC175 (FILHO)
  public
    constructor Create; virtual;   /// Create
    destructor  Destroy; override; /// Destroy

    property IND_OPER      : TACBrTipoOperacao  read FIND_OPER      write FIND_OPER;
    property IND_EMIT      : TACBrEmitente      read FIND_EMIT      write FIND_EMIT;
    property COD_PART      : String             read FCOD_PART      write FCOD_PART;
    property COD_MOD       : String             read FCOD_MOD       write FCOD_MOD;
    property COD_SIT       : TACBrCodSit        read FCOD_SIT       write FCOD_SIT;
    property SER           : String             read FSER           write FSER;
    property NUM_DOC       : String             read FNUM_DOC       write FNUM_DOC;
    property CHV_NFE       : String             read FCHV_NFE       write FCHV_NFE;
    property DT_DOC        : TDateTime          read FDT_DOC        write FDT_DOC;
    property DT_E_S        : TDateTime          read FDT_E_S        write FDT_E_S;
    property VL_DOC        : currency           read FVL_DOC        write FVL_DOC;
    property IND_PGTO      : TACBrTipoPagamento read FIND_PGTO      write FIND_PGTO;
    property VL_DESC       : currency           read FVL_DESC       write FVL_DESC;
    property VL_ABAT_NT    : currency           read FVL_ABAT_NT    write FVL_ABAT_NT;
    property VL_MERC       : currency           read FVL_MERC       write FVL_MERC;
    property IND_FRT       : TACBrTipoFrete     read FIND_FRT       write FIND_FRT;
    property VL_FRT        : currency           read FVL_FRT        write FVL_FRT;
    property VL_SEG        : currency           read FVL_SEG        write FVL_SEG;
    property VL_OUT_DA     : currency           read FVL_OUT_DA     write FVL_OUT_DA;
    property VL_BC_ICMS    : currency           read FVL_BC_ICMS    write FVL_BC_ICMS;
    property VL_ICMS       : currency           read FVL_ICMS       write FVL_ICMS;
    property VL_BC_ICMS_ST : currency           read FVL_BC_ICMS_ST write FVL_BC_ICMS_ST;
    property VL_ICMS_ST    : currency           read FVL_ICMS_ST    write FVL_ICMS_ST;
    property VL_IPI        : currency           read FVL_IPI        write FVL_IPI;
    property VL_PIS        : currency           read FVL_PIS        write FVL_PIS;
    property VL_COFINS     : currency           read FVL_COFINS     write FVL_COFINS;
    property VL_PIS_ST     : currency           read FVL_PIS_ST     write FVL_PIS_ST;
    property VL_COFINS_ST  : currency           read FVL_COFINS_ST  write FVL_COFINS_ST;
    /// Registros FILHOS
    property RegistroC110  : TRegistroC110List  read FRegistroC110  write FRegistroC110;
    property RegistroC111  : TRegistroC111List  read FRegistroC111  write FRegistroC111;
    property RegistroC120  : TRegistroC120List  read FRegistroC120  write FRegistroC120;
    property RegistroC170  : TRegistroC170List  read FRegistroC170  write FRegistroC170;
    property RegistroC175  : TRegistroC175List  read FRegistroC175  write FRegistroC175;
  end;

  /// Registro C100 - Lista

  TRegistroC100List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC100;              /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroC100); /// SetItem
  public
    function New: TRegistroC100;
    property Items[Index: Integer]: TRegistroC100 read GetItem write SetItem;
  end;

  //REGISTRO C110: COMPLEMENTO DO DOCUMENTO - INFORMAÇÃO COMPLEMENTAR DA NOTA FISCAL
  TRegistroC110 = class
  private
    fCOD_INF              : String;            /// Código da informação complementar do documento fiscal (campo 02 do Registro 0450)
    fTXT_COMPL            : String;            /// Descrição complementar do código de referência.
  public
    property COD_INF      : String read FCOD_INF   write FCOD_INF;
    property TXT_COMPL    : String read FTXT_COMPL write FTXT_COMPL;
  end;

  /// Registro C110 - Lista

  TRegistroC110List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC110;              /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroC110); /// SetItem
  public
    function New: TRegistroC110;
    property Items[Index: Integer]: TRegistroC110 read GetItem write SetItem;
  end;

  //REGISTRO C111: PROCESSO REFERENCIADO
  TRegistroC111 = class
  private
    fNUM_PROC         : string;               //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso;  //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil;9 – Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro C111 - Lista
  TRegistroC111List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC111;
    procedure SetItem(Index: Integer; const Value: TRegistroC111);
  public
    function New: TRegistroC111;
    property Items[Index: Integer]: TRegistroC111 read GetItem write SetItem;
  end;

  //REGISTRO C120: COMPLEMENTO DO DOCUMENTO - OPERAÇÕES DE IMPORTAÇÃO (CÓDIGO 01)
  TRegistroC120 = class
  private
    fCOD_DOC_IMP          : TACBrDoctoImporta;  /// Documento de importação:
    fNUM_DOC__IMP         : String;             /// Número do documento de Importação.
    fPIS_IMP              : currency;           /// Valor pago de PIS na importação
    fCOFINS_IMP           : currency;           /// Valor pago de COFINS na importação
    fNUM_ACDRAW           : String;             /// Número do Ato Concessório do regime Drawback

  public
    property COD_DOC_IMP  : TACBrDoctoImporta read FCOD_DOC_IMP  write FCOD_DOC_IMP;
    property NUM_DOC__IMP : String            read FNUM_DOC__IMP write FNUM_DOC__IMP;
    property PIS_IMP      : currency          read FPIS_IMP      write FPIS_IMP;
    property COFINS_IMP   : currency          read FCOFINS_IMP   write FCOFINS_IMP;
    property NUM_ACDRAW   : String            read FNUM_ACDRAW   write FNUM_ACDRAW;
  end;

  /// Registro C120 - Lista

  TRegistroC120List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC120;              /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroC120); /// SetItem
  public
    function New: TRegistroC120;
    property Items[Index: Integer]: TRegistroC120 read GetItem write SetItem;
  end;

  //REGISTRO C170: COMPLEMENTO DO DOCUMENTO - ITENS DO DOCUMENTO (CÓDIGOS 01, 1B, 04 e 55)

  { TRegistroC170 }

  TRegistroC170 = class
  private
    fNUM_ITEM                 : String;                    /// Número seqüencial do item no documento fiscal
    fCOD_ITEM                 : String;                    /// Código do item (campo 02 do Registro 0200)
    fDESCR_COMPL              : String;                    /// Descrição complementar do item como adotado no documento fiscal
    fQTD                      : Variant;                   /// Quantidade do item
    fUNID                     : String;                    /// Unidade do item(Campo 02 do registro 0190)
    fVL_ITEM                  : currency;                  /// Valor total do item
    fVL_DESC                  : currency;                  /// Valor do desconto comercial
    fIND_MOV                  : TACBrMovimentacaoFisica;   /// Movimentação física do ITEM/PRODUTO: 0 - SIM; 1- NÃO
    fCST_ICMS                 : TACBrCstIcms;              /// Código da Situação Tributária referente ao ICMS, conforme a Tabela indicada no item 4.3.1 //sticmsTributadaIntegralmente - '00' Tributada integralmente; sticmsTributadaComCobracaPorST - '10' Tributada e com cobrança do ICMS por substituição tributária; sticmsComReducao - '20' Com redução de base de cálculo; sticmsIsentaComCobracaPorST - '30' Isenta ou não tributada e com cobrança do ICMS por substituição tributária; sticmsIsenta - '40' Isenta; sticmsNaoTributada - '41' Não tributada; sticmsSuspensao - '50' Suspensão; sticmsDiferimento - '51' Diferimento; sticmsCobradoAnteriormentePorST - '60' ICMS cobrado anteriormente por substituição tributária; sticmsComReducaoPorST - '70' Com redução de base de cálculo e cobrança do ICMS por substituição tributária; sticmsOutros - '90' Outros
    fCFOP                     : String;                    /// Código Fiscal de Operação e Prestação
    fCOD_NAT                  : String;                    /// Código da natureza da operação (campo 02 do Registro 0400)
    fVL_BC_ICMS               : currency;                  /// Valor da base de cálculo do ICMS
    fALIQ_ICMS                : currency;                  /// Alíquota do ICMS
    fVL_ICMS                  : currency;                  /// Valor do ICMS creditado/debitado
    fVL_BC_ICMS_ST            : currency;                  /// Valor da base de cálculo referente à substituição tributária
    fALIQ_ST                  : currency;                  /// Alíquota do ICMS da substituição tributária na unidade da federação de destino
    fVL_ICMS_ST               : currency;                  /// Valor do ICMS referente à substituição tributária
    fIND_APUR                 : TACBrApuracaoIPI;          /// Indicador de período de apuração do IPI: 0 - iaMensal - Mensal; 1 - iaDecendial - Decendial
    fCST_IPI                  : TACBrCstIpi;               /// Código da Situação Tributária referente ao IPI, conforme a Tabela indicada no item 4.3.2.
    fCOD_ENQ                  : String;                    /// Código de enquadramento legal do IPI, conforme tabela indicada no item 4.5.3.
    fVL_BC_IPI                : currency;                  /// Valor da base de cálculo do IPI
    fALIQ_IPI                 : currency;                  /// Alíquota do IPI
    fVL_IPI                   : currency;                  /// Valor do IPI creditado/debitado
    fCST_PIS                  : TACBrCstPis;               /// Código da Situação Tributária referente ao PIS.
    fVL_BC_PIS                : Variant;                   /// Valor da base de cálculo do PIS
    fALIQ_PIS_PERC            : Variant;                   /// Alíquota do PIS (em percentual)
    fQUANT_BC_PIS             : Variant;                   /// Quantidade - Base de cálculo PIS
    fALIQ_PIS_R               : Variant;                   /// Alíquota do PIS (em reais)
    fVL_PIS                   : Variant;                   /// Valor do PIS
    fCST_COFINS               : TACBrSituacaoTribCOFINS;   /// Código da Situação Tributária referente ao COFINS.
    fVL_BC_COFINS             : Variant;                   /// Valor da base de cálculo da COFINS
    fALIQ_COFINS_PERC         : Variant;                   /// Alíquota do COFINS (em percentual)
    fQUANT_BC_COFINS          : Variant;                   /// Quantidade - Base de cálculo COFINS
    fALIQ_COFINS_R            : Variant;                   /// Alíquota da COFINS (em reais)
    fVL_COFINS                : Variant;                   /// Valor da COFINS
    fCOD_CTA                  : String;                    /// Código da conta analítica contábil debitada/creditada
  public
    property NUM_ITEM         : String                    read FNUM_ITEM         write FNUM_ITEM;
    property COD_ITEM         : String                    read FCOD_ITEM         write FCOD_ITEM;
    property DESCR_COMPL      : String                    read FDESCR_COMPL      write FDESCR_COMPL;
    property QTD              : Variant                   read FQTD              write FQTD;
    property UNID             : String                    read FUNID             write FUNID;
    property VL_ITEM          : currency                  read FVL_ITEM          write FVL_ITEM;
    property VL_DESC          : currency                  read FVL_DESC          write FVL_DESC;
    property IND_MOV          : TACBrMovimentacaoFisica   read FIND_MOV          write FIND_MOV;
    property CST_ICMS         : TACBrCstIcms              read FCST_ICMS         write FCST_ICMS;
    property CFOP             : String                    read FCFOP             write FCFOP;
    property COD_NAT          : String                    read FCOD_NAT          write FCOD_NAT;
    property VL_BC_ICMS       : currency                  read FVL_BC_ICMS       write FVL_BC_ICMS;
    property ALIQ_ICMS        : currency                  read FALIQ_ICMS        write FALIQ_ICMS;
    property VL_ICMS          : currency                  read FVL_ICMS          write FVL_ICMS;
    property VL_BC_ICMS_ST    : currency                  read FVL_BC_ICMS_ST    write FVL_BC_ICMS_ST;
    property ALIQ_ST          : currency                  read FALIQ_ST          write FALIQ_ST;
    property VL_ICMS_ST       : currency                  read FVL_ICMS_ST       write FVL_ICMS_ST;
    property IND_APUR         : TACBrApuracaoIPI          read FIND_APUR         write FIND_APUR;
    property CST_IPI          : TACBrCstIpi               read FCST_IPI          write FCST_IPI;
    property COD_ENQ          : String                    read FCOD_ENQ          write FCOD_ENQ;
    property VL_BC_IPI        : currency                  read FVL_BC_IPI        write FVL_BC_IPI;
    property ALIQ_IPI         : currency                  read FALIQ_IPI         write FALIQ_IPI;
    property VL_IPI           : currency                  read FVL_IPI           write FVL_IPI;
    property CST_PIS          : TACBrCstPis               read FCST_PIS          write FCST_PIS;
    property VL_BC_PIS        : Variant                   read FVL_BC_PIS        write FVL_BC_PIS;
    property ALIQ_PIS_PERC    : Variant                   read FALIQ_PIS_PERC    write FALIQ_PIS_PERC;
    property QUANT_BC_PIS     : Variant                   read FQUANT_BC_PIS     write FQUANT_BC_PIS;
    property ALIQ_PIS_R       : Variant                   read FALIQ_PIS_R       write FALIQ_PIS_R;
    property VL_PIS           : Variant                   read FVL_PIS           write FVL_PIS;
    property CST_COFINS       : TACBrSituacaoTribCOFINS   read FCST_COFINS       write FCST_COFINS;
    property VL_BC_COFINS     : Variant                   read FVL_BC_COFINS     write FVL_BC_COFINS;
    property ALIQ_COFINS_PERC : Variant                   read FALIQ_COFINS_PERC write FALIQ_COFINS_PERC;
    property QUANT_BC_COFINS  : Variant                   read FQUANT_BC_COFINS  write FQUANT_BC_COFINS;
    property ALIQ_COFINS_R    : Variant                   read FALIQ_COFINS_R    write FALIQ_COFINS_R;
    property VL_COFINS        : Variant                   read FVL_COFINS        write FVL_COFINS;
    property COD_CTA          : String                    read FCOD_CTA          write FCOD_CTA;
  end;

  /// Registro C170 - Lista

  TRegistroC170List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC170;              /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroC170); /// SetItem
  public
    function New: TRegistroC170;
    property Items[Index: Integer]: TRegistroC170 read GetItem write SetItem;
  end;

  //REGISTRO C175: REGISTRO ANALÍTICO DO DOCUMENTO (CÓDIGO 65)

  TRegistroC175 = class
  private
    fALIQ_COFINS       : Variant;
    fALIQ_COFINS_QUANT : Variant;
    fALIQ_PIS          : Variant;
    fALIQ_PIS_QUANT    : Variant;
    fCFOP              : string;
    fCOD_CTA           : string;
    fCST_COFINS        : TACBrSituacaoTribCOFINS;
    fCST_PIS           : TACBrCstPis;
    fINFO_COMPL        : string;
    fQUANT_BC_COFINS   : Variant;
    fQUANT_BC_PIS      : Variant;
    fVL_BC_COFINS      : Variant;
    fVL_BC_PIS         : Variant;
    fVL_COFINS         : Variant;
    fVL_DESC           : Variant;
    fVL_OPR            : Variant;
    fVL_PIS            : Variant;
  public

    property CFOP                 : string  read fCFOP                  write fCFOP;
    property VL_OPR               : Variant read fVL_OPR                write fVL_OPR;
    property VL_DESC              : Variant read fVL_DESC               write fVL_DESC;
    property CST_PIS              : TACBrCstPis read fCST_PIS           write fCST_PIS;
    property VL_BC_PIS            : Variant read fVL_BC_PIS             write fVL_BC_PIS;
    property ALIQ_PIS             : Variant read fALIQ_PIS              write fALIQ_PIS;
    property QUANT_BC_PIS         : Variant read fQUANT_BC_PIS          write fQUANT_BC_PIS;
    property ALIQ_PIS_QUANT       : Variant read fALIQ_PIS_QUANT        write fALIQ_PIS_QUANT;
    property VL_PIS               : Variant read fVL_PIS                write fVL_PIS;
    property CST_COFINS           : TACBrSituacaoTribCOFINS   read fCST_COFINS write fCST_COFINS;
    property VL_BC_COFINS         : Variant read fVL_BC_COFINS          write fVL_BC_COFINS;
    property ALIQ_COFINS          : Variant read fALIQ_COFINS           write fALIQ_COFINS;
    property QUANT_BC_COFINS      : Variant read fQUANT_BC_COFINS       write fQUANT_BC_COFINS;
    property ALIQ_COFINS_QUANT    : Variant read fALIQ_COFINS_QUANT     write fALIQ_COFINS_QUANT;
    property VL_COFINS            : Variant read fVL_COFINS             write fVL_COFINS;
    property COD_CTA              : string  read fCOD_CTA               write fCOD_CTA;
    property INFO_COMPL           : string  read fINFO_COMPL            write fINFO_COMPL;
  end;

  /// Registro C175 - Lista

  TRegistroC175List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC175;              /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroC175); /// SetItem
  public
    function New: TRegistroC175;
    property Items[Index: Integer]: TRegistroC175 read GetItem write SetItem;
  end;

  //REGISTRO C180: CONSOLIDAÇÃO DE NOTAS FISCAIS ELETRÕNICAS EMITIDAS PELA PESSOA JURÍDICA (CÓDIGO 55) – OPERAÇÕES DE VENDAS
  TRegistroC180 = class
  private
    fCOD_MOD              : string;              //02	COD_MOD	Texto fixo contendo "55" Código da Nota Fiscal Eletrônica, modelo 55, conforme a Tabela 4.1.1.	C	002*	-
    fDT_DOC_INI           : TDateTime;           //03	DT_DOC_INI	Data de Emissão Inicial dos Documentos	N	008*	-
    fDT_DOC_FIN           : TDateTime;           //04	DT_DOC_FIN	Data de Emissão Final dos Documentos	N	008*	-
    fCOD_ITEM             : string;              //05	COD_ITEM	Código do Item (campo 02 do Registro 0200)	C	060	-
    fCOD_NCM              : string;              //06	COD_NCM	Código da Nomenclatura Comum do Mercosul	C	008*	-
    fEX_IPI               : string;              //07	EX_IPI	Código EX, conforme a TIPI	C	003	-
    fVL_TOT_ITEM          : Currency;            //08	VL_TOT_ITEM	Valor Total do Item	N	-	02

    FRegistroC181         : TRegistroC181List;
    FRegistroC185         : TRegistroC185List;
    FRegistroC188         : TRegistroC188List;
  public
    constructor Create;  virtual;   /// Create
    destructor  Destroy; override;  /// Destroy

    property COD_MOD      : string            read FCOD_MOD      write FCOD_MOD;
    property DT_DOC_INI   : TDateTime         read FDT_DOC_INI   write FDT_DOC_INI;
    property DT_DOC_FIN   : TDateTime         read FDT_DOC_FIN   write FDT_DOC_FIN;
    property COD_ITEM     : string            read FCOD_ITEM     write FCOD_ITEM;
    property COD_NCM      : string            read FCOD_NCM      write FCOD_NCM;
    property EX_IPI       : string            read FEX_IPI       write FEX_IPI;
    property VL_TOT_ITEM  : Currency          read FVL_TOT_ITEM  write FVL_TOT_ITEM;

    property RegistroC181 : TRegistroC181List read FRegistroC181 write FRegistroC181;
    property RegistroC185 : TRegistroC185List read FRegistroC185 write FRegistroC185;
    property RegistroC188 : TRegistroC188List read FRegistroC188 write FRegistroC188;
  end;

  // Registro C180 - Lista
  TRegistroC180List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC180;
    procedure SetItem(Index: Integer; const Value: TRegistroC180);
  public
    function New: TRegistroC180;
    property Items[Index: Integer]: TRegistroC180 read GetItem write SetItem;
  end;

  //REGISTRO C181: DETALHAMENTO DA CONSOLIDAÇÃO – OPERAÇÕES DE VENDAS – PIS/PASEP
  TRegistroC181 = class
  private
    fCST_PIS                : TACBrCstPis;          //02	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP, conforme a Tabela indicada no item 4.3.3.	N	002*	-
    fCFOP                   : String;               //03	CFOP	Código fiscal de operação e prestação	N	004*	-
    fVL_ITEM                : Currency;             //04	VL_ITEM	Valor do item	N	-	02
    fVL_DESC                : Variant;              //05	VL_DESC	Valor do desconto comercial	N	-	02
    fVL_BC_PIS              : Variant;              //06	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	 -	02
    fALIQ_PIS               : Variant;              //07	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fQUANT_BC_PIS           : Variant;              //08	QUANT_BC_PIS	Quantidade – Base de cálculo PIS/PASEP	N	 -	03
    fALIQ_PIS_QUANT         : Variant;              //09	ALIQ_PIS_QUANT	Alíquota do PIS/PASEP (em reais)	N	 -	04
    fVL_PIS                 : Variant;              //10	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCOD_CTA                : string;               //11	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_PIS        : TACBrCstPis read FCST_PIS        write FCST_PIS;
    property CFOP           : string      read FCFOP           write FCFOP;
    property VL_ITEM        : Currency    read FVL_ITEM        write FVL_ITEM;
    property VL_DESC        : Variant     read FVL_DESC        write FVL_DESC;
    property VL_BC_PIS      : Variant     read FVL_BC_PIS      write FVL_BC_PIS;
    property ALIQ_PIS       : Variant     read FALIQ_PIS       write FALIQ_PIS;
    property QUANT_BC_PIS   : Variant     read FQUANT_BC_PIS   write FQUANT_BC_PIS;
    property ALIQ_PIS_QUANT : Variant     read FALIQ_PIS_QUANT write FALIQ_PIS_QUANT;
    property VL_PIS         : Variant     read FVL_PIS         write FVL_PIS;
    property COD_CTA        : string      read FCOD_CTA        write FCOD_CTA;
  end;

  // Registro C181 - Lista
  TRegistroC181List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC181;
    procedure SetItem(Index: Integer; const Value: TRegistroC181);
  public
    function New: TRegistroC181;
    property Items[Index: Integer]: TRegistroC181 read GetItem write SetItem;
  end;

  //REGISTRO C185: DETALHAMENTO DA CONSOLIDAÇÃO – OPERAÇÕES DE VENDAS – COFINS
  TRegistroC185 = class
  private
    fCST_COFINS                : TACBrSituacaoTribCOFINS;  //02	CST_COFINS	Código da Situação Tributária referente a COFINS, conforme a Tabela indicada no item 4.3.4.	N	002*	-
    fCFOP                      : string;                   //03	CFOP	Código fiscal de operação e prestação	N	004*	-
    fVL_ITEM                   : Currency;                 //04	VL_ITEM	Valor do item	N	-	02
    fVL_DESC                   : Variant;                  //05	VL_DESC	Valor do desconto comercial	N	-	02
    fVL_BC_COFINS              : Variant;                  //06	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	 -	02
    fALIQ_COFINS               : Variant;                  //07	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008	04
    fQUANT_BC_COFINS           : Variant;                  //08	QUANT_BC_COFINS	Quantidade – Base de cálculo da COFINS	N	 -	03
    fALIQ_COFINS_QUANT         : Variant;                  //09	ALIQ_COFINS_QUANT	Alíquota da COFINS (em reais)	N	 -	04
    fVL_COFINS                 : Variant;                  //10	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA                   : string;                   //11	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_COFINS        : TACBrSituacaoTribCOFINS read FCST_COFINS        write FCST_COFINS;
    property CFOP              : string                  read FCFOP              write FCFOP;
    property VL_ITEM           : Currency                read FVL_ITEM           write FVL_ITEM;
    property VL_DESC           : Variant                 read FVL_DESC           write FVL_DESC;
    property VL_BC_COFINS      : Variant                 read FVL_BC_COFINS      write FVL_BC_COFINS;
    property ALIQ_COFINS       : Variant                 read FALIQ_COFINS       write FALIQ_COFINS;
    property QUANT_BC_COFINS   : Variant                 read FQUANT_BC_COFINS   write FQUANT_BC_COFINS;
    property ALIQ_COFINS_QUANT : Variant                 read FALIQ_COFINS_QUANT write FALIQ_COFINS_QUANT;
    property VL_COFINS         : Variant                 read FVL_COFINS         write FVL_COFINS;
    property COD_CTA           : string                  read FCOD_CTA           write FCOD_CTA;
  end;

  // Registro C185 - Lista
  TRegistroC185List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC185;
    procedure SetItem(Index: Integer; const Value: TRegistroC185);
  public
    function New: TRegistroC185;
    property Items[Index: Integer]: TRegistroC185 read GetItem write SetItem;
  end;

  //REGISTRO C188: PROCESSO REFERENCIADO
  TRegistroC188 = class
  private
    fNUM_PROC         : string;              //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso; //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 - Secretaria da Receita Federal do Brasil;9 – Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro C188 - Lista
  TRegistroC188List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC188;
    procedure SetItem(Index: Integer; const Value: TRegistroC188);
  public
    function New: TRegistroC188;
    property Items[Index: Integer]: TRegistroC188 read GetItem write SetItem;
  end;

  //REGISTRO C190: CONSOLIDAÇÃO DE NOTAS FISCAIS ELETRÔNICAS (CÓDIGO 55) – OPERAÇÕES DE AQUISIÇÃO COM DIREITO A CRÉDITO, E OPERAÇÕES DE DEVOLUÇÃO DE COMPRAS E VENDAS
  TRegistroC190 = class
  private
    fCOD_MOD      : string;                //02	COD_MOD	Texto fixo contendo "55" Código da Nota Fiscal Eletrônica, modelo 55, conforme a Tabela 4.1.1.	C	002*	-
    fDT_REF_INI   : TDateTime;             //03	DT_REF_INI	Data Inicial de Referencia da Consolidação	N	008*	-
    fDT_REF_FIN   : TDateTime;             //04	DT_REF_FIN	Data Final de Referencia da Consolidação	N	008*	-
    fCOD_ITEM     : string;                //05	COD_ITEM	Código do item (campo 02 do Registro 0200)	C	060	-
    fCOD_NCM      : string;                //06	COD_NCM	Código da Nomenclatura Comum do Mercosul	C	008*	-
    fEX_IPI       : string;                //07	EX_IPI	Código EX, conforme a TIPI	C	003	-
    fVL_TOT_ITEM  : Currency;              //08	VL_TOT_ITEM	Valor Total do Item	N	-	02

    FRegistroC191 : TRegistroC191List;     /// BLOCO C - Lista de RegistroC190 (FILHO)
    FRegistroC195 : TRegistroC195List;     /// BLOCO C - Lista de RegistroC195 (FILHO)
    FRegistroC198 : TRegistroC198List;     /// BLOCO C - Lista de RegistroC195 (FILHO)
    FRegistroC199 : TRegistroC199List;     /// BLOCO C - Lista de RegistroC195 (FILHO)
  public
    constructor Create;  virtual;  /// Create
    destructor  Destroy; override; /// Destroy

    property COD_MOD      : string            read FCOD_MOD      write FCOD_MOD;
    property DT_REF_INI   : TDateTime         read FDT_REF_INI   write FDT_REF_INI;
    property DT_REF_FIN   : TDateTime         read FDT_REF_FIN   write FDT_REF_FIN;
    property COD_ITEM     : string            read FCOD_ITEM     write FCOD_ITEM;
    property COD_NCM      : string            read FCOD_NCM      write FCOD_NCM;
    property EX_IPI       : string            read FEX_IPI       write FEX_IPI;
    property VL_TOT_ITEM  : Currency          read FVL_TOT_ITEM  write FVL_TOT_ITEM;

    property RegistroC191 : TRegistroC191List read FRegistroC191 write FRegistroC191;
    property RegistroC195 : TRegistroC195List read FRegistroC195 write FRegistroC195;
    property RegistroC198 : TRegistroC198List read FRegistroC198 write FRegistroC198;
    property RegistroC199 : TRegistroC199List read FRegistroC199 write FRegistroC199;
  end;

  // Registro C190 - Lista
  TRegistroC190List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC190;
    procedure SetItem(Index: Integer; const Value: TRegistroC190);
  public
    function New: TRegistroC190;
    property Items[Index: Integer]: TRegistroC190 read GetItem write SetItem;
  end;

  //REGISTRO C191: DETALHAMENTO DA CONSOLIDAÇÃO – OPERAÇÕES DE AQUISIÇÃO COM DIREITO A CRÉDITO, E OPERAÇÕES DE DEVOLUÇÃO DE COMPRAS E VENDAS – PIS/PASEP
  TRegistroC191 = class
  private
    fCNPJ_CPF_PART   : string;        //02	COD_PART	Código do Participante (campo 02 do Registro 0150) do emitente dos documentos ou do remetente das mercadorias.	C	060	-
    fCST_PIS         : TACBrCstPis;   //03	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fCFOP            : Integer;       //04	CFOP	Código fiscal de operação e prestação	N	004*	-
    fVL_ITEM         : Currency;       //05	VL_ITEM	Valor do item	N	-	02
    fVL_DESC         : Variant;       //06	VL_DESC	Valor do desconto comercial	N	-	02
    fVL_BC_PIS       : Variant;       //07	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	-	02
    fALIQ_PIS        : Variant;       //08	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fQUANT_BC_PIS    : Variant;       //09	QUANT_BC_PIS	Quantidade – Base de cálculo PIS/PASEP	N	-	03
    fALIQ_PIS_QUANT  : Variant;       //10	ALIQ_PIS_QUANT	Alíquota do PIS/PASEP (em reais)	N	-	04
    fVL_PIS          : Variant;       //11	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCOD_CTA         : string;        //12	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CNPJ_CPF_PART  : string      read FCNPJ_CPF_PART  write FCNPJ_CPF_PART;
    property CST_PIS        : TACBrCstPis read FCST_PIS        write FCST_PIS;
    property CFOP           : Integer     read FCFOP           write FCFOP;
    property VL_ITEM        : Currency    read FVL_ITEM        write FVL_ITEM;
    property VL_DESC        : Variant     read FVL_DESC        write FVL_DESC;
    property VL_BC_PIS      : Variant     read FVL_BC_PIS      write FVL_BC_PIS;
    property ALIQ_PIS       : Variant     read FALIQ_PIS       write FALIQ_PIS;
    property QUANT_BC_PIS   : Variant     read FQUANT_BC_PIS   write FQUANT_BC_PIS;
    property ALIQ_PIS_QUANT : Variant     read FALIQ_PIS_QUANT write FALIQ_PIS_QUANT;
    property VL_PIS         : Variant     read FVL_PIS         write FVL_PIS;
    property COD_CTA        : string      read FCOD_CTA        write FCOD_CTA;
  end;

  // Registro C191 - Lista
  TRegistroC191List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC191;
    procedure SetItem(Index: Integer; const Value: TRegistroC191);
  public
    function New: TRegistroC191;
    property Items[Index: Integer]: TRegistroC191 read GetItem write SetItem;
  end;

  //REGISTRO C195: DETALHAMENTO DA CONSOLIDAÇÃO - OPERAÇÕES DE AQUISIÇÃO COM DIREITO A CRÉDITO, E OPERAÇÕES DE DEVOLUÇÃO DE COMPRAS E VENDAS – COFINS
  TRegistroC195 = class
  private
    fCNPJ_CPF_PART             : string;                  //02	COD_PART	Código do Participante (campo 02 do Registro 0150) do emitente dos documentos ou do remetente das mercadorias.	C	060	-
    fCST_COFINS                : TACBrSituacaoTribCOFINS; //03	CST_COFINS	Código da Situação Tributária referente a COFINS.	N	002*	-
    fCFOP                      : Integer;                 //04	CFOP	Código fiscal de operação e prestação	N	004*	-
    fVL_ITEM                   : Currency;                //05	VL_ITEM	Valor do item	N	-	02
    fVL_DESC                   : Variant;                 //06	VL_DESC	Valor do desconto comercial	N	-	02
    fVL_BC_COFINS              : Variant;                 //07	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	-	02
    fALIQ_COFINS               : Variant;                 //08	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008	04
    fQUANT_BC_COFINS           : Variant;                 //09	QUANT_BC_COFINS	Quantidade – Base de cálculo da COFINS	N	-	03
    fALIQ_COFINS_QUANT         : Variant;                 //10	ALIQ_COFINS_QUANT	Alíquota da COFINS (em reais)	N	-	04
    fVL_COFINS                 : Currency;                //11	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA                   : string;                  //12	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CNPJ_CPF_PART     : String                  read FCNPJ_CPF_PART     write FCNPJ_CPF_PART;
    property CST_COFINS        : TACBrSituacaoTribCOFINS read FCST_COFINS        write FCST_COFINS;
    property CFOP              : Integer                 read FCFOP              write FCFOP;
    property VL_ITEM           : Currency                read FVL_ITEM           write FVL_ITEM;
    property VL_DESC           : Variant                 read FVL_DESC           write FVL_DESC;
    property VL_BC_COFINS      : Variant                 read FVL_BC_COFINS      write FVL_BC_COFINS;
    property ALIQ_COFINS       : Variant                 read FALIQ_COFINS       write FALIQ_COFINS;
    property QUANT_BC_COFINS   : Variant                 read FQUANT_BC_COFINS   write FQUANT_BC_COFINS;
    property ALIQ_COFINS_QUANT : Variant                 read FALIQ_COFINS_QUANT write FALIQ_COFINS_QUANT;
    property VL_COFINS         : Currency                read FVL_COFINS         write FVL_COFINS;
    property COD_CTA           : string                  read FCOD_CTA           write FCOD_CTA;
  end;

  // Registro C195 - Lista
  TRegistroC195List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC195;
    procedure SetItem(Index: Integer; const Value: TRegistroC195);
  public
    function New: TRegistroC195;
    property Items[Index: Integer]: TRegistroC195 read GetItem write SetItem;
  end;

  //REGISTRO C198: PROCESSO REFERENCIADO
  TRegistroC198 = class
  private
    fNUM_PROC         : string;              //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso; //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil;9 – Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro C198 - Lista
  TRegistroC198List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC198;
    procedure SetItem(Index: Integer; const Value: TRegistroC198);
  public
    function New: TRegistroC198;
    property Items[Index: Integer]: TRegistroC198 read GetItem write SetItem;
  end;

  //REGISTRO C199: COMPLEMENTO DO DOCUMENTO - OPERAÇÕES DE IMPORTAÇÃO (CÓDIGO 55)
  TRegistroC199 = class
  private
    fCOD_DOC_IMP           : TACBrDoctoImporta;    //02	COD_DOC_IMP	Documento de importação:0 – Declaração de Importação;1 – Declaração Simplificada de Importação.	C	001*	-
    fNUM_DOC__IMP          : string;               //03	NUM_DOC__IMP	Número do documento de Importação.	C	010	-
    fVL_PIS_IMP            : Currency;             //04	VL_PIS_IMP	Valor pago de PIS na importação	N	-	02
    fVL_COFINS_IMP         : Currency;             //05	VL_COFINS_IMP	Valor pago de COFINS na importação	N	-	02
    fNUM_ACDRAW            : string;               //06	NUM_ACDRAW	Número do Ato Concessório do regime Drawback 	C	011	-
  public
    property COD_DOC_IMP   : TACBrDoctoImporta read FCOD_DOC_IMP   write FCOD_DOC_IMP;
    property NUM_DOC__IMP  : string            read FNUM_DOC__IMP  write FNUM_DOC__IMP;
    property VL_PIS_IMP    : Currency          read FVL_PIS_IMP    write FVL_PIS_IMP;
    property VL_COFINS_IMP : Currency          read FVL_COFINS_IMP write FVL_COFINS_IMP;
    property NUM_ACDRAW    : string            read FNUM_ACDRAW    write FNUM_ACDRAW;
  end;

  // Registro C199 - Lista
  TRegistroC199List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC199;
    procedure SetItem(Index: Integer; const Value: TRegistroC199);
  public
    function New: TRegistroC199;
    property Items[Index: Integer]: TRegistroC199 read GetItem write SetItem;
  end;

  //REGISTRO C380: NOTA FISCAL DE VENDA A CONSUMIDOR (CÓDIGO 02) - CONSOLIDAÇÃO DE DOCUMENTOS EMITIDOS
  TRegistroC380 = class
  private
    fCOD_MOD              : string;                //02	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1 (Código 02 – Nota Fiscal de Venda a Consumidor)	C	002*	-
    fDT_DOC_INI           : TDateTime;             //03	DT_DOC_INI	Data de Emissão Inicial dos Documentos	N	008*	-
    fDT_DOC_FIN           : TDateTime;             //04	DT_DOC_FIN	Data de Emissão Final dos Documentos	N	008*	-
    fNUM_DOC_INI          : Integer;               //05	NUM_DOC_INI	Número do documento fiscal inicial	N	006	-
    fNUM_DOC_FIN          : Integer;               //06	NUM_DOC_FIN	Número do documento fiscal final	N	006	-
    fVL_DOC               : Currency;              //07	VL_DOC	Valor total dos documentos emitidos	N	-	02
    fVL_DOC_CANC          : Currency;              //08	VL_DOC_CANC	Valor total dos documentos cancelados	N	-	02

    FRegistroC381         : TRegistroC381List;
    FRegistroC385         : TRegistroC385List;
  public
    constructor Create;  virtual;                  /// Create
    destructor  Destroy; override;                 /// Destroy

    property COD_MOD      : string            read FCOD_MOD      write FCOD_MOD;
    property DT_DOC_INI   : TDateTime         read FDT_DOC_INI   write FDT_DOC_INI;
    property DT_DOC_FIN   : TDateTime         read FDT_DOC_FIN   write FDT_DOC_FIN;
    property NUM_DOC_INI  : Integer           read FNUM_DOC_INI  write FNUM_DOC_INI;
    property NUM_DOC_FIN  : Integer           read FNUM_DOC_FIN  write FNUM_DOC_FIN;
    property VL_DOC       : Currency          read FVL_DOC       write FVL_DOC;
    property VL_DOC_CANC  : Currency          read FVL_DOC_CANC  write FVL_DOC_CANC;

    property RegistroC381 : TRegistroC381List read FRegistroC381 write FRegistroC381;
    property RegistroC385 : TRegistroC385List read FRegistroC385 write FRegistroC385;
  end;

  // Registro C380 - Lista
  TRegistroC380List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC380;
    procedure SetItem(Index: Integer; const Value: TRegistroC380);
  public
    function New : TRegistroC380;
    property Items[Index: Integer]: TRegistroC380 read GetItem write SetItem;
  end;

  //REGISTRO C381: DETALHAMENTO DA CONSOLIDAÇÃO – PIS/PASEP
  TRegistroC381 = class
  private
    fCST_PIS                : TACBrCstPis;           //02	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fCOD_ITEM               : string;                //03	COD_ITEM	Código do item (campo 02 do Registro 0200)	C	060	-
    fVL_ITEM                : Currency;              //04	VL_ITEM	Valor total dos itens	N	-	02
    fVL_BC_PIS              : Variant;               //05	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	 -	02
    fALIQ_PIS               : Variant;               //06	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fQUANT_BC_PIS           : Variant;               //07	QUANT_BC_PIS	Quantidade – Base de cálculo do PIS/PASEP	N	 	03
    fALIQ_PIS_QUANT         : Variant;               //08	ALIQ_PIS_QUANT	Alíquota do PIS/PASEP (em reais)	N	 -	04
    fVL_PIS                 : Variant;              //09	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCOD_CTA                : string;                //10	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_PIS        : TACBrCstPis read FCST_PIS        write FCST_PIS;
    property COD_ITEM       : string      read FCOD_ITEM       write FCOD_ITEM;
    property VL_ITEM        : Currency    read FVL_ITEM        write FVL_ITEM;
    property VL_BC_PIS      : Variant     read FVL_BC_PIS      write FVL_BC_PIS;
    property ALIQ_PIS       : Variant     read FALIQ_PIS       write FALIQ_PIS;
    property QUANT_BC_PIS   : Variant     read FQUANT_BC_PIS   write FQUANT_BC_PIS;
    property ALIQ_PIS_QUANT : Variant     read FALIQ_PIS_QUANT write FALIQ_PIS_QUANT;
    property VL_PIS         : Variant     read FVL_PIS         write FVL_PIS;
    property COD_CTA        : string      read FCOD_CTA        write FCOD_CTA;
  end;

  // Registro C381 - Lista
  TRegistroC381List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC381;
    procedure SetItem(Index: Integer; const Value: TRegistroC381);
  public
    function New: TRegistroC381;
    property Items[Index: Integer]: TRegistroC381 read GetItem write SetItem;
  end;

  //REGISTRO C385: DETALHAMENTO DA CONSOLIDAÇÃO – COFINS
  TRegistroC385 = class
  private
    fCST_COFINS                : TACBrSituacaoTribCOFINS;   //02	CST_COFINS	Código da Situação Tributária referente a COFINS.	N	002*	-
    fCOD_ITEM                  : string;                    //03	COD_ITEM	Código do item (campo 02 do Registro 0200)	C	060	-
    fVL_ITEM                   : Currency;                  //04	VL_ITEM	Valor total dos itens	N	-	02
    fVL_BC_COFINS              : Variant;                   //05	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	 	02
    fALIQ_COFINS               : Variant;                   //06	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008	04
    fQUANT_BC_COFINS           : Variant;                   //07	QUANT_BC_COFINS	Quantidade – Base de cálculo da COFINS	N	 	03
    fALIQ_COFINS_QUANT         : Variant;                   //08	ALIQ_COFINS_QUANT	Alíquota da COFINS (em reais)	N	 	04
    fVL_COFINS                 : Variant;                   //09	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA                   : string;                    //10	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_COFINS        : TACBrSituacaoTribCOFINS   read FCST_COFINS        write FCST_COFINS;
    property COD_ITEM          : string                    read FCOD_ITEM          write FCOD_ITEM;
    property VL_ITEM           : Currency                  read FVL_ITEM           write FVL_ITEM;
    property VL_BC_COFINS      : Variant                   read FVL_BC_COFINS      write FVL_BC_COFINS;
    property ALIQ_COFINS       : Variant                   read FALIQ_COFINS       write FALIQ_COFINS;
    property QUANT_BC_COFINS   : Variant                   read FQUANT_BC_COFINS   write FQUANT_BC_COFINS;
    property ALIQ_COFINS_QUANT : Variant                   read FALIQ_COFINS_QUANT write FALIQ_COFINS_QUANT;
    property VL_COFINS         : Variant                   read FVL_COFINS         write FVL_COFINS;
    property COD_CTA           : string                    read FCOD_CTA           write FCOD_CTA;
  end;

  // Registro C385 - Lista
  TRegistroC385List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC385;
    procedure SetItem(Index: Integer; const Value: TRegistroC385);
  public
    function New: TRegistroC385;
    property Items[Index: Integer]: TRegistroC385 read GetItem write SetItem;
  end;

  //REGISTRO C395: NOTAS FISCAIS DE VENDA A CONSUMIDOR (CÓDIGOS 02, 2D, 2E e 59) – AQUISIÇÕES/ENTRADAS COM CRÉDITO
  TRegistroC395 = class
  private
    fCOD_MOD              : string;              //02	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1 	C	002*	-
    fCOD_PART             : string;              //03	COD_PART	Código do participante emitente do documento (campo 02 do Registro 0150).	C	060	-
    fSER                  : string;              //04	SER	Série do documento fiscal	C	003	-
    fSUB_SER              : string;              //05	SUB_SER	Subsérie do documento fiscal	C	003	-
    fNUM_DOC              : string;              //06	NUM_DOC	Número do documento fiscal	C	006	-
    fDT_DOC               : TDateTime;           //07	DT_DOC	Data da emissão do documento fiscal	N	008*	-
    fVL_DOC               : Currency;            //08	VL_DOC	Valor total do documento fiscal	N	-	02

    FRegistroC396         : TRegistroC396List;
  public
    constructor Create;  virtual;                /// Create
    destructor  Destroy; override;               /// Destroy

    property COD_MOD      : string            read FCOD_MOD      write FCOD_MOD;
    property COD_PART     : string            read FCOD_PART     write FCOD_PART;
    property SER          : string            read FSER          write FSER;
    property SUB_SER      : string            read FSUB_SER      write FSUB_SER;
    property NUM_DOC      : string            read FNUM_DOC      write FNUM_DOC;
    property DT_DOC       : TDateTime         read FDT_DOC       write FDT_DOC;
    property VL_DOC       : Currency          read FVL_DOC       write FVL_DOC;

    property RegistroC396 : TRegistroC396List read FRegistroC396 write FRegistroC396;
  end;

  // Registro C395 - Lista
  TRegistroC395List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC395;
    procedure SetItem(Index: Integer; const Value: TRegistroC395);
  public
    function New: TRegistroC395;
    property Items[Index: Integer]: TRegistroC395 read GetItem write SetItem;
  end;

  //REGISTRO C396: ITENS DO DOCUMENTO (CÓDIGOS 02, 2D, 2E e 59) – AQUISIÇÕES/ENTRADAS COM CRÉDITO
  TRegistroC396 = class
  private
    fCOD_ITEM             : string;                  //02	COD_ITEM	Código do item (campo 02 do Registro 0200)	C	060	-
    fVL_ITEM              : Currency;                //03	VL_ITEM	Valor total do item (mercadorias ou serviços)	N	-	02
    fVL_DESC              : Currency;                //04	VL_DESC	Valor do desconto comercial do item	N	-	02
    fNAT_BC_CRED          : TACBrBaseCalculoCredito; //05	NAT_BC_CRED	Código da Base de Cálculo do Crédito, conforme a Tabela indicada no item 4.3.7.	C	002*	-
    fCST_PIS              : TACBrCstPis;             //06	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fVL_BC_PIS            : Currency;                //07	VL_BC_PIS	Valor da base de cálculo do crédito de PIS/PASEP	N	 	02
    fALIQ_PIS             : Currency;                //08	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fVL_PIS               : Currency;                //09	VL_PIS	Valor do crédito de PIS/PASEP	N	-	02
    fCST_COFINS           : TACBrSituacaoTribCOFINS; //10	CST_COFINS	Código da Situação Tributária referente a COFINS	N	002*	-
    fVL_BC_COFINS         : Currency;                //11	VL_BC_COFINS	Valor da base de cálculo do crédito de COFINS	N	 	02
    fALIQ_COFINS          : Currency;                //12	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008	04
    fVL_COFINS            : Currency;                //13	VL_COFINS	Valor do crédito de COFINS	N	-	02
    fCOD_CTA              : string;                  //14	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property COD_ITEM     : string                  read FCOD_ITEM     write FCOD_ITEM;
    property VL_ITEM      : Currency                read FVL_ITEM      write FVL_ITEM;
    property VL_DESC      : Currency                read FVL_DESC      write FVL_DESC;
    property NAT_BC_CRED  : TACBrBaseCalculoCredito read FNAT_BC_CRED  write FNAT_BC_CRED;
    property CST_PIS      : TACBrCstPis             read FCST_PIS      write FCST_PIS;
    property VL_BC_PIS    : Currency                read FVL_BC_PIS    write FVL_BC_PIS;
    property ALIQ_PIS     : Currency                read FALIQ_PIS     write FALIQ_PIS;
    property VL_PIS       : Currency                read FVL_PIS       write FVL_PIS;
    property CST_COFINS   : TACBrSituacaoTribCOFINS read FCST_COFINS   write FCST_COFINS;
    property VL_BC_COFINS : Currency                read FVL_BC_COFINS write FVL_BC_COFINS;
    property ALIQ_COFINS  : Currency                read FALIQ_COFINS  write FALIQ_COFINS;
    property VL_COFINS    : Currency                read FVL_COFINS    write FVL_COFINS;
    property COD_CTA      : string                  read FCOD_CTA      write FCOD_CTA;
  end;

  // Registro C396 - Lista
  TRegistroC396List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC396;
    procedure SetItem(Index: Integer; const Value: TRegistroC396);
  public
    function New: TRegistroC396;
    property Items[Index: Integer]: TRegistroC396 read GetItem write SetItem;
  end;

  //REGISTRO C400: EQUIPAMENTO ECF (CÓDIGOS 02 e 2D)
  TRegistroC400 = class
  private
    fCOD_MOD              : string;          //02	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1	C	002*	-
    fECF_MOD              : string;          //03	ECF_MOD	Modelo do equipamento	C	020	-
    fECF_FAB              : string;          //04	ECF_FAB	Número de série de fabricação do ECF	C	020	-
    fECF_CX               : Integer;         //05	ECF_CX	Número do caixa atribuído ao ECF	N	003	-

    FRegistroC405         : TRegistroC405List;
    FRegistroC489         : TRegistroC489List;
  public
    constructor Create;  virtual;             /// Create
    destructor  Destroy; override;            /// Destroy

    property COD_MOD      : string            read FCOD_MOD      write FCOD_MOD;
    property ECF_MOD      : string            read FECF_MOD      write FECF_MOD;
    property ECF_FAB      : string            read FECF_FAB      write FECF_FAB;
    property ECF_CX       : Integer           read FECF_CX       write FECF_CX;

    property RegistroC405 : TRegistroC405List read FRegistroC405 write FRegistroC405;
    property RegistroC489 : TRegistroC489List read FRegistroC489 write FRegistroC489;
  end;

  // Registro C400 - Lista
  TRegistroC400List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC400;
    procedure SetItem(Index: Integer; const Value: TRegistroC400);
  public
    function New: TRegistroC400;
    property Items[Index: Integer]: TRegistroC400 read GetItem write SetItem;
  end;

  //REGISTRO C405: REDUÇÃO Z (CÓDIGOS 02 e 2D)
  TRegistroC405 = class
  private
    fDT_DOC               : TDateTime;             //02	DT_DOC	Data do movimento a que se refere a Redução Z	N	008*	-
    fCRO                  : Integer;               //03	CRO	Posição do Contador de Reinício de Operação	N	003	-
    fCRZ                  : Integer;               //04	CRZ	Posição do Contador de Redução Z	N	006	-
    fNUM_COO_FIN          : Integer;               //05	NUM_COO_FIN	Número do Contador de Ordem de Operação do último documento emitido no dia (Número do COO na Redução Z)	N	006	-
    fGT_FIN               : Currency;              //06	GT_FIN	Valor do Grande Total final	N	-	02
    fVL_BRT               : Currency;              //07	VL_BRT	Valor da venda bruta	N	-	02

    FRegistroC481         : TRegistroC481List;
    FRegistroC485         : TRegistroC485List;
  public
    constructor Create;  virtual;                   /// Create
    destructor  Destroy; override;                  /// Destroy

    property DT_DOC       : TDateTime         read FDT_DOC       write FDT_DOC;
    property CRO          : Integer           read FCRO          write FCRO;
    property CRZ          : Integer           read FCRZ          write FCRZ;
    property NUM_COO_FIN  : Integer           read FNUM_COO_FIN  write FNUM_COO_FIN;
    property GT_FIN       : Currency          read FGT_FIN       write FGT_FIN;
    property VL_BRT       : Currency          read FVL_BRT       write FVL_BRT;

    property RegistroC481 : TRegistroC481List read FRegistroC481 write FRegistroC481;
    property RegistroC485 : TRegistroC485List read FRegistroC485 write FRegistroC485;
  end;

  // Registro C405 - Lista
  TRegistroC405List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC405;
    procedure SetItem(Index: Integer; const Value: TRegistroC405);
  public
    function New: TRegistroC405;
    property Items[Index: Integer]: TRegistroC405 read GetItem write SetItem;
  end;

  //REGISTRO C481: RESUMO DIÁRIO DE DOCUMENTOS EMITIDOS POR ECF – PIS/PASEP (CÓDIGOS 02 e 2D)
  TRegistroC481 = class
  private
    fCST_PIS                : TACBrCstPis;   //02	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fVL_ITEM                : Currency;      //03	VL_ITEM	Valor total dos itens	N	-	02
    fVL_BC_PIS              : Variant;       //04	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	 -	02
    fALIQ_PIS               : Variant;       //05	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fQUANT_BC_PIS           : Variant;       //06	QUANT_BC_PIS	Quantidade – Base de cálculo PIS/PASEP	N	 -	03
    fALIQ_PIS_QUANT         : Variant;       //07	ALIQ_PIS_QUANT	Alíquota do PIS/PASEP (em reais)	N	 -	04
    fVL_PIS                 : Variant;       //08	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCOD_ITEM               : string;        //09	COD_ITEM	Código do item (campo 02 do Registro 0200)	C	060	-
    fCOD_CTA                : string;        //10	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_PIS        : TACBrCstPis read FCST_PIS        write FCST_PIS;
    property VL_ITEM        : Currency    read FVL_ITEM        write FVL_ITEM;
    property VL_BC_PIS      : Variant     read FVL_BC_PIS      write FVL_BC_PIS;
    property ALIQ_PIS       : Variant     read FALIQ_PIS       write FALIQ_PIS;
    property QUANT_BC_PIS   : Variant     read FQUANT_BC_PIS   write FQUANT_BC_PIS;
    property ALIQ_PIS_QUANT : Variant     read FALIQ_PIS_QUANT write FALIQ_PIS_QUANT;
    property VL_PIS         : Variant     read FVL_PIS         write FVL_PIS;
    property COD_ITEM       : string      read FCOD_ITEM       write FCOD_ITEM;
    property COD_CTA        : string      read FCOD_CTA        write FCOD_CTA;
  end;

  // Registro C481 - Lista
  TRegistroC481List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC481;
    procedure SetItem(Index: Integer; const Value: TRegistroC481);
  public
    function New: TRegistroC481;
    property Items[Index: Integer]: TRegistroC481 read GetItem write SetItem;
  end;

  //REGISTRO C485: RESUMO DIÁRIO DE DOCUMENTOS EMITIDOS POR ECF – COFINS (CÓDIGOS 02 e 2D)
  TRegistroC485 = class
  private
    fCST_COFINS                : TACBrSituacaoTribCOFINS; //02	CST_COFINS	Código da Situação Tributária referente a COFINS.	N	002*	-
    fVL_ITEM                   : Currency;                //03	VL_ITEM	Valor total dos itens	N	-	02
    fVL_BC_COFINS              : Variant;                 //04	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	 -	02
    fALIQ_COFINS               : Variant;                 //05	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008	04
    fQUANT_BC_COFINS           : Variant;                 //06	QUANT_BC_COFINS	Quantidade – Base de cálculo da COFINS	N	 -	03
    fALIQ_COFINS_QUANT         : Variant;                 //07	ALIQ_COFINS_QUANT	Alíquota da COFINS (em reais)	N	 -	04
    fVL_COFINS                 : Variant;                 //08	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_ITEM                  : string;                  //09	COD_ITEM	Código do item (campo 02 do Registro 0200)	C	060	-
    fCOD_CTA                   : string;                  //10	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_COFINS        : TACBrSituacaoTribCOFINS  read FCST_COFINS        write FCST_COFINS;
    property VL_ITEM           : Currency                 read FVL_ITEM           write FVL_ITEM;
    property VL_BC_COFINS      : Variant                  read FVL_BC_COFINS      write FVL_BC_COFINS;
    property ALIQ_COFINS       : Variant                  read FALIQ_COFINS       write FALIQ_COFINS;
    property QUANT_BC_COFINS   : Variant                  read FQUANT_BC_COFINS   write FQUANT_BC_COFINS;
    property ALIQ_COFINS_QUANT : Variant                  read FALIQ_COFINS_QUANT write FALIQ_COFINS_QUANT;
    property VL_COFINS         : Variant                  read FVL_COFINS         write FVL_COFINS;
    property COD_ITEM          : string                   read FCOD_ITEM          write FCOD_ITEM;
    property COD_CTA           : string                   read FCOD_CTA           write FCOD_CTA;
  end;

  // Registro C485 - Lista
  TRegistroC485List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC485;
    procedure SetItem(Index: Integer; const Value: TRegistroC485);
  public
    function New: TRegistroC485;
    property Items[Index: Integer]: TRegistroC485 read GetItem write SetItem;
  end;

  //REGISTRO C489: PROCESSO REFERENCIADO
  TRegistroC489 = class
  private
    fNUM_PROC         : string;              //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso; //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil;9 - Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro C489 - Lista
  TRegistroC489List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC489;
    procedure SetItem(Index: Integer; const Value: TRegistroC489);
  public
    function New: TRegistroC489;
    property Items[Index: Integer]: TRegistroC489 read GetItem write SetItem;
  end;

  //REGISTRO C490: CONSOLIDAÇÃO DE DOCUMENTOS EMITIDOS POR ECF (CÓDIGOS 02, 2D e 59)
  TRegistroC490 = class
  private
    fDT_DOC_INI           : TDateTime;     //02	DT_DOC_INI	Data de Emissão Inicial dos Documentos	N	008*	-
    fDT_DOC_FIN           : TDateTime;     //03	DT_DOC_FIN	Data de Emissão Final dos Documentos	N	008*	-
    fCOD_MOD              : string;        //04	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1	C	002*	-

    FRegistroC491         : TRegistroC491List;
    FRegistroC495         : TRegistroC495List;
    FRegistroC499         : TRegistroC499List;
  public
    constructor Create;  virtual;           /// Create
    destructor  Destroy; override;          /// Destroy

    property DT_DOC_INI   : TDateTime         read FDT_DOC_INI   write FDT_DOC_INI;
    property DT_DOC_FIN   : TDateTime         read FDT_DOC_FIN   write FDT_DOC_FIN;
    property COD_MOD      : string            read FCOD_MOD      write FCOD_MOD;

    property RegistroC491 : TRegistroC491List read FRegistroC491 write FRegistroC491;
    property RegistroC495 : TRegistroC495List read FRegistroC495 write FRegistroC495;
    property RegistroC499 : TRegistroC499List read FRegistroC499 write FRegistroC499;
  end;

  // Registro C490 - Lista
  TRegistroC490List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC490;
    procedure SetItem(Index: Integer; const Value: TRegistroC490);
  public
    function New: TRegistroC490;
    property Items[Index: Integer]: TRegistroC490 read GetItem write SetItem;
  end;

  //REGISTRO C491: DETALHAMENTO DA CONSOLIDAÇÃO DE DOCUMENTOS EMITIDOS POR ECF (CÓDIGOS 02, 2D e 59) – PIS/PASEP
  TRegistroC491 = class
  private
    fCOD_ITEM               : string;               //02	COD_ITEM	Código do item (campo 02 do Registro 0200)	C	060	-
    fCST_PIS                : TACBrCstPis;          //03	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fCFOP                   : Integer;              //04	CFOP	Código fiscal de operação e prestação	N	004*	-
    fVL_ITEM                : Currency;             //05	VL_ITEM	Valor total dos itens	N	-	02
    fVL_BC_PIS              : Variant;              //06	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	 -	02
    fALIQ_PIS               : Variant;              //07	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fQUANT_BC_PIS           : Variant;              //08	QUANT_BC_PIS	Quantidade – Base de cálculo PIS/PASEP	N	 -	03
    fALIQ_PIS_QUANT         : Variant;              //09	ALIQ_PIS_QUANT	Alíquota do PIS/PASEP (em reais)	N	 -	04
    fVL_PIS                 : Variant;              //10	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCOD_CTA                : string;               //11	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property COD_ITEM       : string      read FCOD_ITEM       write FCOD_ITEM;
    property CST_PIS        : TACBrCstPis read FCST_PIS        write FCST_PIS;
    property CFOP           : Integer     read FCFOP           write FCFOP;
    property VL_ITEM        : Currency    read FVL_ITEM        write FVL_ITEM;
    property VL_BC_PIS      : Variant     read FVL_BC_PIS      write FVL_BC_PIS;
    property ALIQ_PIS       : Variant     read FALIQ_PIS       write FALIQ_PIS;
    property QUANT_BC_PIS   : Variant     read FQUANT_BC_PIS   write FQUANT_BC_PIS;
    property ALIQ_PIS_QUANT : Variant     read FALIQ_PIS_QUANT write FALIQ_PIS_QUANT;
    property VL_PIS         : Variant     read FVL_PIS         write FVL_PIS;
    property COD_CTA        : string      read FCOD_CTA        write FCOD_CTA;
  end;

  // Registro C491 - Lista
  TRegistroC491List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC491;
    procedure SetItem(Index: Integer; const Value: TRegistroC491);
  public
    function New: TRegistroC491;
    property Items[Index: Integer]: TRegistroC491 read GetItem write SetItem;
  end;

  //REGISTRO C495: DETALHAMENTO DA CONSOLIDAÇÃO DE DOCUMENTOS EMITIDOS POR ECF (CÓDIGOS 02, 2D e 59) – COFINS
  TRegistroC495 = class
  private
    fCOD_ITEM                  : string;                  //02	COD_ITEM	Código do item (campo 02 do Registro 0200)	C	060	-
    fCST_COFINS                : TACBrSituacaoTribCOFINS; //03	CST_COFINS	Código da Situação Tributária referente a COFINS.	N	002*	-
    fCFOP                      : Integer;                 //04	CFOP	Código fiscal de operação e prestação	N	004*	-
    fVL_ITEM                   : Currency;                //05	VL_ITEM	Valor total dos itens	N	-	02
    fVL_BC_COFINS              : Variant;                 //06	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	 -	02
    fALIQ_COFINS               : Variant;                 //07	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008	04
    fQUANT_BC_COFINS           : Variant;                 //08	QUANT_BC_COFINS	Quantidade – Base de cálculo da COFINS	N	 -	03
    fALIQ_COFINS_QUANT         : Variant;                 //09	ALIQ_COFINS_QUANT	Alíquota da COFINS (em reais)	N	 -	04
    fVL_COFINS                 : Variant;                 //10	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA                   : string;                  //11	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property COD_ITEM          : string                  read FCOD_ITEM          write FCOD_ITEM;
    property CST_COFINS        : TACBrSituacaoTribCOFINS read FCST_COFINS        write FCST_COFINS;
    property CFOP              : Integer                 read FCFOP              write FCFOP;
    property VL_ITEM           : Currency                read FVL_ITEM           write FVL_ITEM;
    property VL_BC_COFINS      : Variant                 read FVL_BC_COFINS      write FVL_BC_COFINS;
    property ALIQ_COFINS       : Variant                 read FALIQ_COFINS       write FALIQ_COFINS;
    property QUANT_BC_COFINS   : Variant                 read FQUANT_BC_COFINS   write FQUANT_BC_COFINS;
    property ALIQ_COFINS_QUANT : Variant                 read FALIQ_COFINS_QUANT write FALIQ_COFINS_QUANT;
    property VL_COFINS         : Variant                 read FVL_COFINS         write FVL_COFINS;
    property COD_CTA           : string                  read FCOD_CTA           write FCOD_CTA;
  end;

  // Registro C495 - Lista
  TRegistroC495List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC495;
    procedure SetItem(Index: Integer; const Value: TRegistroC495);
  public
    function New: TRegistroC495;
    property Items[Index: Integer]: TRegistroC495 read GetItem write SetItem;
  end;

  //REGISTRO C499: PROCESSO REFERENCIADO
  TRegistroC499 = class
  private
    fNUM_PROC         : string;              //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso; //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil;9 - Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro C499 - Lista
  TRegistroC499List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC499;
    procedure SetItem(Index: Integer; const Value: TRegistroC499);
  public
    function New: TRegistroC499;
    property Items[Index: Integer]: TRegistroC499 read GetItem write SetItem;
  end;

  //REGISTRO C500: NOTA FISCAL/CONTA DE ENERGIA ELÉTRICA (CÓDIGO 06), NOTA FISCAL/CONTA DE FORNECIMENTO D'ÁGUA CANALIZADA (CÓDIGO 29) E NOTA FISCAL CONSUMO FORNECIMENTO DE GÁS (CÓDIGO 28) – DOCUMENTOS DE ENTRADA/AQUISIÇÃO COM CRÉDITO
  TRegistroC500 = class
  private
    fCOD_PART             : string;               //02	COD_PART	Código do participante do fornecedor (campo 02 do Registro 0150). 	C	060	-
    fCOD_MOD              : string;               //03	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1 	C	002*	-
    fCOD_SIT              : TACBrCodSit;          //04	COD_SIT	Código da situação do documento fiscal, conforme a Tabela 4.1.2 	N	002*	-
    fSER                  : string;               //05	SER	Série do documento fiscal	C	004	-
    fSUB                  : Integer;              //06	SUB	Subsérie do documento fiscal	N	003	-
    fNUM_DOC              : Integer;              //07	NUM_DOC	Número do documento fiscal	N	009	-
    fDT_DOC               : TDateTime;            //08	DT_DOC	Data da emissão do documento fiscal	N	008*	-
    fDT_ENT               : TDateTime;            //09	DT_ENT	Data da entrada	N	008*	-
    fVL_DOC               : Currency;             //10	VL_DOC	Valor total do documento fiscal	N	-	02
    fVL_ICMS              : Currency;             //11	VL_ICMS	Valor acumulado do ICMS	N	-	02
    fCOD_INF              : string;               //12	COD_INF	Código da informação complementar do documento fiscal (campo 02 do Registro 0450)	C	006	-
    fVL_PIS               : Currency;             //13	VL_PIS	Valor do PIS/PASEP	N	-	02
    fVL_COFINS            : Currency;             //14	VL_COFINS	Valor da COFINS	N	-	02

    FRegistroC501         : TRegistroC501List;
    FRegistroC505         : TRegistroC505List;
    FRegistroC509         : TRegistroC509List;
  public
    constructor Create;  virtual;                 /// Create
    destructor  Destroy; override;                /// Destroy

    property COD_PART     : string             read FCOD_PART     write FCOD_PART;
    property COD_MOD      : string             read FCOD_MOD      write FCOD_MOD;
    property COD_SIT      : TACBrCodSit        read FCOD_SIT      write FCOD_SIT;
    property SER          : string             read FSER          write FSER;
    property SUB          : Integer            read FSUB          write FSUB;
    property NUM_DOC      : Integer            read FNUM_DOC      write FNUM_DOC;
    property DT_DOC       : TDateTime          read FDT_DOC       write FDT_DOC;
    property DT_ENT       : TDateTime          read FDT_ENT       write FDT_ENT;
    property VL_DOC       : Currency           read FVL_DOC       write FVL_DOC;
    property VL_ICMS      : Currency           read FVL_ICMS      write FVL_ICMS;
    property COD_INF      : string             read FCOD_INF      write FCOD_INF;
    property VL_PIS       : Currency           read FVL_PIS       write FVL_PIS;
    property VL_COFINS    : Currency           read FVL_COFINS    write FVL_COFINS;

    property RegistroC501 : TRegistroC501List  read FRegistroC501 write FRegistroC501;
    property RegistroC505 : TRegistroC505List  read FRegistroC505 write FRegistroC505;
    property RegistroC509 : TRegistroC509List  read FRegistroC509 write FRegistroC509;
  end;

 // Registro C500 - Lista
  TRegistroC500List = class(TObjectList)
  private
    function  GetItem(Index  : Integer): TRegistroC500;
    procedure SetItem(Index  : Integer; const Value: TRegistroC500);
  public
    function New             : TRegistroC500;
    property Items[Index     : Integer]: TRegistroC500 read GetItem write SetItem;
  end;

  //REGISTRO C501: COMPLEMENTO DA OPERAÇÃO (CÓDIGOS 06, 28 e 29) – PIS/PASEP
  TRegistroC501 = class
  private
    fCST_PIS             : TACBrCstPis;             //02	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fVL_ITEM             : Currency;                //03	VL_ITEM	Valor total dos itens	N	-	02
    fNAT_BC_CRED         : TACBrBaseCalculoCredito; //04	NAT_BC_CRED	Código da Base de Cálculo do Crédito, conforme a Tabela indicada no item 4.3.7.	C	002*	-
    fVL_BC_PIS           : Currency;                //05	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	- 	02
    fALIQ_PIS            : Currency;                //06	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fVL_PIS              : Currency;                //07	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCOD_CTA             : string;                  //08	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_PIS     : TACBrCstPis             read FCST_PIS     write FCST_PIS;
    property VL_ITEM     : Currency                read FVL_ITEM     write FVL_ITEM;
    property NAT_BC_CRED : TACBrBaseCalculoCredito read FNAT_BC_CRED write FNAT_BC_CRED;
    property VL_BC_PIS   : Currency                read FVL_BC_PIS   write FVL_BC_PIS;
    property ALIQ_PIS    : Currency                read FALIQ_PIS    write FALIQ_PIS;
    property VL_PIS      : Currency                read FVL_PIS      write FVL_PIS;
    property COD_CTA     : string                  read FCOD_CTA     write FCOD_CTA;
  end;

  // Registro C501 - Lista
  TRegistroC501List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC501;
    procedure SetItem(Index: Integer; const Value: TRegistroC501);
  public
    function New: TRegistroC501;
    property Items[Index: Integer]: TRegistroC501 read GetItem write SetItem;
  end;

  //REGISTRO C505: COMPLEMENTO DA OPERAÇÃO (CÓDIGOS 06, 28 e 29) – COFINS
  TRegistroC505 = class
  private
    fCST_COFINS           : TACBrSituacaoTribCOFINS;           //02	CST_COFINS	Código da Situação Tributária referente a COFINS	N	002*	-
    fVL_ITEM              : Currency;                          //03	VL_ITEM	Valor total dos itens	N	-	02
    fNAT_BC_CRED          : TACBrBaseCalculoCredito;           //04	NAT_BC_CRED	Código da Base de Cálculo do Crédito, conforme a Tabela indicada no item 4.3.7 	C	002*	-
    fVL_BC_COFINS         : Currency;                          //05	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	 -	02
    fALIQ_COFINS          : Currency;                          //06	ALIQ_COFINS	Alíquota da COFINS  (em percentual)	N	008	04
    fVL_COFINS            : Currency;                          //07	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA              : string;                            //08	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_COFINS   : TACBrSituacaoTribCOFINS  read FCST_COFINS   write FCST_COFINS;
    property VL_ITEM      : Currency                 read FVL_ITEM      write FVL_ITEM;
    property NAT_BC_CRED  : TACBrBaseCalculoCredito  read FNAT_BC_CRED  write FNAT_BC_CRED;
    property VL_BC_COFINS : Currency                 read FVL_BC_COFINS write FVL_BC_COFINS;
    property ALIQ_COFINS  : Currency                 read FALIQ_COFINS  write FALIQ_COFINS;
    property VL_COFINS    : Currency                 read FVL_COFINS    write FVL_COFINS;
    property COD_CTA      : string                   read FCOD_CTA      write FCOD_CTA;
  end;

  // Registro C505 - Lista
  TRegistroC505List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC505;
    procedure SetItem(Index: Integer; const Value: TRegistroC505);
  public
    function New: TRegistroC505;
    property Items[Index: Integer]: TRegistroC505 read GetItem write SetItem;
  end;

  //REGISTRO C509: PROCESSO REFERENCIADO
  TRegistroC509 = class
  private
    fNUM_PROC         : string;              //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso; //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil;9 – Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro C509 - Lista
  TRegistroC509List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC509;
    procedure SetItem(Index: Integer; const Value: TRegistroC509);
  public
    function New: TRegistroC509;
    property Items[Index: Integer]: TRegistroC509 read GetItem write SetItem;
  end;

  //REGISTRO C600: CONSOLIDAÇÃO DIÁRIA DE NOTAS FISCAIS/CONTAS EMITIDAS DE ENERGIA ELÉTRICA (CÓDIGO 06), NOTA FISCAL/CONTA DE FORNECIMENTO D'ÁGUA CANALIZADA (CÓDIGO 29) E NOTA FISCAL/CONTA DE FORNECIMENTO DE GÁS (CÓDIGO 28) (EMPRESAS OBRIGADAS OU NÃO OBRIGADAS AO CONVENIO ICMS 115/03) – DOCUMENTOS DE SAÍDA
  TRegistroC600 = class
  private
    fCOD_MOD               : string;                     //02	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1 	C	002*	-
    fCOD_MUN               : Integer;                    //03	COD_MUN	Código do município dos pontos de consumo, conforme a tabela IBGE	N	007*	-
    fSER                   : string;                     //04	SER	Série do documento fiscal	C	004	-
    fSUB                   : Integer;                    //05	SUB	Subsérie do documento fiscal	N	003	-
    fCOD_CONS              : Integer;                    //06	COD_CONS	Código de classe de consumo de energia elétrica, conforme a Tabela 4.4.5, ou Código de Consumo de Fornecimento D´água – Tabela 4.4.2 ou Código da classe de consumo de gás canalizado   conforme Tabela 4.4.3.	N	002*	-
    fQTD_CONS              : Integer;                    //07	QTD_CONS	Quantidade de documentos consolidados neste registro	N	-	-
    fQTD_CANC              : Integer;                    //08	QTD_CANC	Quantidade de documentos cancelados	N	-	-
    fDT_DOC                : TDateTime;                  //09	DT_DOC	Data dos documentos consolidados	N	008*	-
    fVL_DOC                : Currency;                   //10	VL_DOC	Valor total dos documentos	N	-	02
    fVL_DESC               : Currency;                   //11	VL_DESC	Valor acumulado dos descontos	N	-	02
    fCONS                  : Integer;                    //12	CONS	Consumo total acumulado, em kWh (Código 06)	N	-	-
    fVL_FORN               : Currency;                   //13	VL_FORN	Valor acumulado do fornecimento	N	-	02
    fVL_SERV_NT            : Currency;                   //14	VL_SERV_NT	Valor acumulado dos serviços não-tributados pelo ICMS	N	-	02
    fVL_TERC               : Currency;                   //15	VL_TERC	Valores cobrados em nome de terceiros	N	-	02
    fVL_DA                 : Currency;                   //16	VL_DA	Valor acumulado das despesas acessórias	N	-	02
    fVL_BC_ICMS            : Currency;                   //17	VL_BC_ICMS	Valor acumulado da base de cálculo do ICMS	N	-	02
    fVL_ICMS               : Currency;                   //18	VL_ICMS	Valor acumulado do ICMS	N	-	02
    fVL_BC_ICMS_ST         : Currency;                   //19	VL_BC_ICMS_ST	Valor acumulado da base de cálculo do ICMS substituição tributária	N	-	02
    fVL_ICMS_ST            : Currency;                   //20	VL_ICMS_ST	Valor acumulado do ICMS retido por substituição tributária	N	-	02
    fVL_PIS                : Currency;                   //21	VL_PIS	Valor acumulado do PIS/PASEP	N	-	02
    fVL_COFINS             : Currency;                   //22	VL_COFINS	Valor acumulado da COFINS	N	-	02

    FRegistroC601          : TRegistroC601List;
    FRegistroC605          : TRegistroC605List;
    FRegistroC609          : TRegistroC609List;
  public
    constructor Create; virtual;                         /// Create
    destructor Destroy; override;                        /// Destroy

    property COD_MOD       : string            read FCOD_MOD       write FCOD_MOD;
    property COD_MUN       : Integer           read FCOD_MUN       write FCOD_MUN;
    property SER           : string            read FSER           write FSER;
    property SUB           : Integer           read FSUB           write FSUB;
    property COD_CONS      : Integer           read FCOD_CONS      write FCOD_CONS;
    property QTD_CONS      : Integer           read FQTD_CONS      write FQTD_CONS;
    property QTD_CANC      : Integer           read FQTD_CANC      write FQTD_CANC;
    property DT_DOC        : TDateTime         read FDT_DOC        write FDT_DOC;
    property VL_DOC        : Currency          read FVL_DOC        write FVL_DOC;
    property VL_DESC       : Currency          read FVL_DESC       write FVL_DESC;
    property CONS          : Integer           read FCONS          write FCONS;
    property VL_FORN       : Currency          read FVL_FORN       write FVL_FORN;
    property VL_SERV_NT    : Currency          read FVL_SERV_NT    write FVL_SERV_NT;
    property VL_TERC       : Currency          read FVL_TERC       write FVL_TERC;
    property VL_DA         : Currency          read FVL_DA         write FVL_DA;
    property VL_BC_ICMS    : Currency          read FVL_BC_ICMS    write FVL_BC_ICMS;
    property VL_ICMS       : Currency          read FVL_ICMS       write FVL_ICMS;
    property VL_BC_ICMS_ST : Currency          read FVL_BC_ICMS_ST write FVL_BC_ICMS_ST;
    property VL_ICMS_ST    : Currency          read FVL_ICMS_ST    write FVL_ICMS_ST;
    property VL_PIS        : Currency          read FVL_PIS        write FVL_PIS;
    property VL_COFINS     : Currency          read FVL_COFINS     write FVL_COFINS;

    property RegistroC601  : TRegistroC601List read FRegistroC601  write FRegistroC601;
    property RegistroC605  : TRegistroC605List read FRegistroC605  write FRegistroC605;
    property RegistroC609  : TRegistroC609List read FRegistroC609  write FRegistroC609;
  end;

  // Registro C600 - Lista
  TRegistroC600List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC600;
    procedure SetItem(Index: Integer; const Value: TRegistroC600);
  public
    function New: TRegistroC600;
    property Items[Index: Integer]: TRegistroC600 read GetItem write SetItem;
  end;

  //REGISTRO C601: COMPLEMENTO DA CONSOLIDAÇÃO DIÁRIA (CÓDIGOS 06, 28 e 29) – DOCUMENTOS DE SAÍDAS - PIS/PASEP
  TRegistroC601 = class
  private
    fCST_PIS           : TACBrCstPis;          //02	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fVL_ITEM           : Currency;             //03	VL_ITEM	Valor total dos itens	N	-	02
    fVL_BC_PIS         : Currency;             //04	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	 -	02
    fALIQ_PIS          : Currency;             //05	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fVL_PIS            : Currency;             //06	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCOD_CTA           : string;               //07	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_PIS   : TACBrCstPis read FCST_PIS   write FCST_PIS;
    property VL_ITEM   : Currency    read FVL_ITEM   write FVL_ITEM;
    property VL_BC_PIS : Currency    read FVL_BC_PIS write FVL_BC_PIS;
    property ALIQ_PIS  : Currency    read FALIQ_PIS  write FALIQ_PIS;
    property VL_PIS    : Currency    read FVL_PIS    write FVL_PIS;
    property COD_CTA   : string      read FCOD_CTA   write FCOD_CTA;
  end;

  // Registro C601 - Lista
  TRegistroC601List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC601;
    procedure SetItem(Index: Integer; const Value: TRegistroC601);
  public
    function New: TRegistroC601;
    property Items[Index: Integer]: TRegistroC601 read GetItem write SetItem;
  end;

  //REGISTRO C605: COMPLEMENTO DA CONSOLIDAÇÃO DIÁRIA (CÓDIGOS 06, 28 e 29) – DOCUMENTOS DE SAÍDAS - COFINS
  TRegistroC605 = class
  private
    fCST_COFINS           : TACBrSituacaoTribCOFINS; //02	CST_COFINS	Código da Situação Tributária referente a COFINS	N	002*	-
    fVL_ITEM              : Currency;                //03	VL_ITEM	Valor total dos itens	N	-	02
    fVL_BC_COFINS         : Currency;                //04	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	 	02
    fALIQ_COFINS          : Currency;                //05	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008	04
    fVL_COFINS            : Currency;                //06	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA              : string;                  //07	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_COFINS   : TACBrSituacaoTribCOFINS  read FCST_COFINS   write FCST_COFINS;
    property VL_ITEM      : Currency                 read FVL_ITEM      write FVL_ITEM;
    property VL_BC_COFINS : Currency                 read FVL_BC_COFINS write FVL_BC_COFINS;
    property ALIQ_COFINS  : Currency                 read FALIQ_COFINS  write FALIQ_COFINS;
    property VL_COFINS    : Currency                 read FVL_COFINS    write FVL_COFINS;
    property COD_CTA      : string                   read FCOD_CTA      write FCOD_CTA;
  end;

  // Registro C605 - Lista
  TRegistroC605List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC605;
    procedure SetItem(Index: Integer; const Value: TRegistroC605);
  public
    function New: TRegistroC605;
    property Items[Index: Integer]: TRegistroC605 read GetItem write SetItem;
  end;

  //REGISTRO C609: PROCESSO REFERENCIADO
  TRegistroC609 = class
  private
    fNUM_PROC         : string;              //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso; //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil;9 – Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro C609 - Lista
  TRegistroC609List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC609;
    procedure SetItem(Index: Integer; const Value: TRegistroC609);
  public
    function New: TRegistroC609;
    property Items[Index: Integer]: TRegistroC609 read GetItem write SetItem;
  end;

  //REGISTRO C800: CUPOM FISCAL ELETRÔNICO (CÓDIGO 59)
  TRegistroC800 = class
  private
    FCOD_MOD              : string;
    FCOD_SIT              : TACBrCodSit;
    FNUM_CFE              : Integer;
    FDT_DOC               : TDateTime;
    FVL_PIS               : Variant;
    FVL_COFINS            : Variant;
    FVL_CFE               : Variant;
    FCNPJ_CPF             : String;
    FNR_SAT               : String;
    FCHV_CFE              : String;
    FVL_DESC              : Variant;
    FVL_MERC              : Variant;
    FVL_OUT_DA            : Variant;
    FVL_PIS_ST            : Variant;
    FVL_ICMS              : Variant;
    FVL_COFINS_ST         : Variant;

    FRegistroC820         : TRegistroC820List;
    FRegistroC830         : TRegistroC830List;
    FRegistroC810         : TRegistroC810List;
  public
    constructor Create; virtual;                         /// Create
    destructor Destroy; override;                        /// Destroy

    property COD_MOD      : String      read FCOD_MOD      write FCOD_MOD;
    property COD_SIT      : TACBrCodSit read FCOD_SIT      write FCOD_SIT;
    property NUM_CFE      : Integer     read FNUM_CFE      write FNUM_CFE;
    property DT_DOC       : TDateTime   read FDT_DOC       write FDT_DOC;
    property VL_CFE       : Variant     read FVL_CFE       write FVL_CFE;
    property VL_PIS       : Variant     read FVL_PIS       write FVL_PIS;
    property VL_COFINS    : Variant     read FVL_COFINS    write FVL_COFINS;
    property CNPJ_CPF     : String      read FCNPJ_CPF     write FCNPJ_CPF;
    property NR_SAT       : String      read FNR_SAT       write FNR_SAT;
    property CHV_CFE      : String      read FCHV_CFE      write FCHV_CFE;
    property VL_DESC      : Variant     read FVL_DESC      write FVL_DESC;
    property VL_MERC      : Variant     read FVL_MERC      write FVL_MERC;
    property VL_OUT_DA    : Variant     read FVL_OUT_DA    write FVL_OUT_DA;
    property VL_ICMS      : Variant     read FVL_ICMS      write FVL_ICMS;
    property VL_PIS_ST    : Variant     read FVL_PIS_ST    write FVL_PIS_ST;
    property VL_COFINS_ST : Variant     read FVL_COFINS_ST write FVL_COFINS_ST;

    property RegistroC810  : TRegistroC810List read FRegistroC810  write FRegistroC810;
    property RegistroC820  : TRegistroC820List read FRegistroC820  write FRegistroC820;
    property RegistroC830  : TRegistroC830List read FRegistroC830  write FRegistroC830;
  end;

  // Registro C800 - Lista
  TRegistroC800List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC800;
    procedure SetItem(Index: Integer; const Value: TRegistroC800);
  public
    function New: TRegistroC800;
    property Items[Index: Integer]: TRegistroC800 read GetItem write SetItem;
  end;

  //REGISTRO C810: DETALHAMENTO DO CUPOM FISCAL ELETRÔNICO (CÓDIGO 59) – PIS/PASEP E COFINS

  TRegistroC810 = class
  private
    fCFOP                 : string;
    fVL_ITEM              : Currency;
    fCOD_ITEM             : string;
    fCST_PIS              : TACBrSituacaoTribPIS;
    fVL_BC_PIS            : Variant;
    fALIQ_PIS             : Variant;
    fVL_PIS               : Variant;
    fCST_COFINS           : TACBrSituacaoTribCOFINS;
    fVL_BC_COFINS         : Variant;
    fALIQ_COFINS          : Variant;
    fVL_COFINS            : Variant;
    fCOD_CTA              : string;
  public
    property CFOP         : string                  read FCFOP         write FCFOP;
    property VL_ITEM      : Currency                read FVL_ITEM      write FVL_ITEM;
    property COD_ITEM     : string                  read FCOD_ITEM     write FCOD_ITEM;
    property CST_PIS      : TACBrSituacaoTribPIS    read FCST_PIS      write FCST_PIS;
    property VL_BC_PIS    : Variant                 read FVL_BC_PIS    write FVL_BC_PIS;
    property ALIQ_PIS     : Variant                 read FALIQ_PIS     write FALIQ_PIS;
    property VL_PIS       : Variant                 read FVL_PIS       write FVL_PIS;
    property CST_COFINS   : TACBrSituacaoTribCOFINS read FCST_COFINS   write FCST_COFINS;
    property VL_BC_COFINS : Variant                 read FVL_BC_COFINS write FVL_BC_COFINS;
    property ALIQ_COFINS  : Variant                 read FALIQ_COFINS  write FALIQ_COFINS;
    property VL_COFINS    : Variant                 read FVL_COFINS    write FVL_COFINS;
    property COD_CTA      : string                  read FCOD_CTA      write FCOD_CTA;
  end;

  // Registro C810 - Lista
  TRegistroC810List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC810;
    procedure SetItem(Index: Integer; const Value: TRegistroC810);
  public
    function New: TRegistroC810;
    property Items[Index: Integer]: TRegistroC810 read GetItem write SetItem;
  end;

  //REGISTRO C820: REGISTRO ANALÍTICO DO DOCUMENTO (CÓDIGO 65)

  TRegistroC820 = class
  private
    fCFOP                         : string;
    fVL_ITEM                      : Currency;
    fCOD_ITEM                     : string;
    fCST_PIS                      : TACBrCstPis;
    fQUANT_BC_PIS                 : Variant;
    fALIQ_PIS_QUANT               : Variant;
    fVL_PIS                       : Variant;
    fCST_COFINS                   : TACBrSituacaoTribCOFINS;
    fQUANT_BC_COFINS              : Variant;
    fALIQ_COFINS_QUANT            : Variant;
    fVL_COFINS                    : Variant;
    fCOD_CTA                      : string;
  public
    property CFOP                 : string                  read fCFOP              write fCFOP;
    property VL_ITEM              : Currency                read FVL_ITEM           write FVL_ITEM;
    property COD_ITEM             : string                  read FCOD_ITEM          write FCOD_ITEM;
    property CST_PIS              : TACBrCstPis             read fCST_PIS           write fCST_PIS;
    property QUANT_BC_PIS         : Variant                 read fQUANT_BC_PIS      write fQUANT_BC_PIS;
    property ALIQ_PIS_QUANT       : Variant                 read fALIQ_PIS_QUANT    write fALIQ_PIS_QUANT;
    property VL_PIS               : Variant                 read fVL_PIS            write fVL_PIS;
    property CST_COFINS           : TACBrSituacaoTribCOFINS read fCST_COFINS        write fCST_COFINS;
    property QUANT_BC_COFINS      : Variant                 read fQUANT_BC_COFINS   write fQUANT_BC_COFINS;
    property ALIQ_COFINS_QUANT    : Variant                 read fALIQ_COFINS_QUANT write fALIQ_COFINS_QUANT;
    property VL_COFINS            : Variant                 read fVL_COFINS         write fVL_COFINS;
    property COD_CTA              : string                  read fCOD_CTA           write fCOD_CTA;
  end;

  // Registro C820 - Lista
  TRegistroC820List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC820;
    procedure SetItem(Index: Integer; const Value: TRegistroC820);
  public
    function New: TRegistroC820;
    property Items[Index: Integer]: TRegistroC820 read GetItem write SetItem;
  end;

  //REGISTRO C830: PROCESSO REFERENCIADO

  TRegistroC830 = class
  private
    fNUM_PROC         : string;
    fIND_PROC         : TACBrOrigemProcesso;
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro C830 - Lista
  TRegistroC830List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC830;
    procedure SetItem(Index: Integer; const Value: TRegistroC830);
  public
    function New: TRegistroC830;
    property Items[Index: Integer]: TRegistroC830 read GetItem write SetItem;
  end;

  //REGISTRO C860: IDENTIFICAÇÃO DO EQUIPAMENTO SAT-CF-E

  TRegistroC860 = class
  private
    fCOD_MOD: String;
    fDOC_FIM: Integer;
    fDOC_INI: Integer;
    fDT_DOC: TDateTime;
    fNR_SAT: Integer;
    FRegistroC870: TRegistroC870List;
    FRegistroC880: TRegistroC880List;
    FRegistroC890: TRegistroC890List;
  public
    constructor Create; virtual;                         /// Create
    destructor Destroy; override;                        /// Destroy

    property COD_MOD : String read fCOD_MOD write fCOD_MOD;
    property NR_SAT : Integer read fNR_SAT write fNR_SAT;
    property DT_DOC : TDateTime read fDT_DOC write fDT_DOC;
    property DOC_INI : Integer read fDOC_INI write fDOC_INI;
    property DOC_FIM : Integer read fDOC_FIM write fDOC_FIM;

    property RegistroC870  : TRegistroC870List read FRegistroC870  write FRegistroC870;
    property RegistroC880  : TRegistroC880List read FRegistroC880  write FRegistroC880;
    property RegistroC890  : TRegistroC890List read FRegistroC890  write FRegistroC890;
  end;

  // Registro C860 - Lista

  TRegistroC860List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC860;
    procedure SetItem(Index: Integer; const Value: TRegistroC860);
  public
    function New: TRegistroC860;
    property Items[Index: Integer]: TRegistroC860 read GetItem write SetItem;
  end;

  //REGISTRO C870: RESUMO DIÁRIO DE DOCUMENTOS EMITIDOS POR EQUIPAMENTO
  //SAT-CF-E (CÓDIGO 59) – PIS/PASEP E COFINS

  TRegistroC870 = class
  private
    fCOD_ITEM: string;
    fCFOP: string;
    fVL_ITEM: Variant;
    fVL_DESC: Variant;
    fALIQ_COFINS: Variant;
    fALIQ_PIS: Variant;
    fCOD_CTA: string;
    fCST_COFINS: TACBrCstCofins;
    fCST_PIS: TACBrCstPis;
    fVL_BC_COFINS: Variant;
    fVL_BC_PIS: Variant;
    fVL_COFINS: Variant;
    fVL_PIS: Variant;
  public
    property COD_ITEM : String read fCOD_ITEM write fCOD_ITEM;
    property CFOP : String read fCFOP write fCFOP;
    property VL_ITEM : Variant read fVL_ITEM write fVL_ITEM;
    property VL_DESC : Variant read fVL_DESC write fVL_DESC;
    property CST_PIS : TACBrCstPis read fCST_PIS write fCST_PIS;
    property VL_BC_PIS : Variant read fVL_BC_PIS write fVL_BC_PIS;
    property ALIQ_PIS : Variant read fALIQ_PIS write fALIQ_PIS;
    property VL_PIS : Variant read fVL_PIS write fVL_PIS;
    property CST_COFINS : TACBrCstCofins read fCST_COFINS write fCST_COFINS;
    property VL_BC_COFINS : Variant read fVL_BC_COFINS write fVL_BC_COFINS;
    property ALIQ_COFINS : Variant read fALIQ_COFINS write fALIQ_COFINS;
    property VL_COFINS : Variant read fVL_COFINS write fVL_COFINS;
    property COD_CTA : string read fCOD_CTA write fCOD_CTA;
  end;

  // Registro C870 - Lista

  TRegistroC870List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC870;
    procedure SetItem(Index: Integer; const Value: TRegistroC870);
  public
    function New: TRegistroC870;
    property Items[Index: Integer]: TRegistroC870 read GetItem write SetItem;
  end;

  // REGISTRO C880: RESUMO DIÁRIO DE DOCUMENTOS EMITIDOS POR EQUIPAMENTO
  // SAT-CF-E (CÓDIGO 59) – PIS/PASEP E COFINS APURADO POR UNIDADE DE MEDIDA DE
  // PRODUTO

  TRegistroC880 = class
  private
    fCFOP: string;
    fVL_ITEM: Variant;
    fCOD_ITEM: string;
    fALIQ_COFINS_QUANT: Variant;
    fALIQ_PIS_QUANT: Variant;
    fCOD_CTA: String;
    fCST_COFINS: TACBrCstCofins;
    fCST_PIS: TACBrCstPis;
    fQUANT_BC_COFINS: Variant;
    fQUANT_BC_PIS: Variant;
    fVL_COFINS: Variant;
    fVL_PIS: Variant;
  public
    property COD_ITEM : string read fCOD_ITEM write fCOD_ITEM;
    property CFOP : string read fCFOP write fCFOP;
    property VL_ITEM : Variant read fVL_ITEM write fVL_ITEM;
    property CST_PIS : TACBrCstPis read fCST_PIS write fCST_PIS;
    property QUANT_BC_PIS : Variant read fQUANT_BC_PIS write fQUANT_BC_PIS;
    property ALIQ_PIS_QUANT : Variant read fALIQ_PIS_QUANT write fALIQ_PIS_QUANT;
    property VL_PIS : Variant read fVL_PIS write fVL_PIS;
    property CST_COFINS : TACBrCstCofins read fCST_COFINS write fCST_COFINS;
    property QUANT_BC_COFINS : Variant read fQUANT_BC_COFINS write fQUANT_BC_COFINS;
    property ALIQ_COFINS_QUANT : Variant read fALIQ_COFINS_QUANT write fALIQ_COFINS_QUANT;
    property VL_COFINS : Variant read fVL_COFINS write fVL_COFINS;
    property COD_CTA : String read fCOD_CTA write fCOD_CTA;
  end;

  // Registro C880 - Lista

  TRegistroC880List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC880;
    procedure SetItem(Index: Integer; const Value: TRegistroC880);
  public
    function New: TRegistroC880;
    property Items[Index: Integer]: TRegistroC880 read GetItem write SetItem;
  end;

  // REGISTRO C890: PROCESSO REFERENCIADO

  TRegistroC890 = class
  private
    fNUM_PROC         : string;
    fIND_PROC         : TACBrOrigemProcesso;
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro C890 - Lista

  TRegistroC890List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroC890;
    procedure SetItem(Index: Integer; const Value: TRegistroC890);
  public
    function New: TRegistroC890;
    property Items[Index: Integer]: TRegistroC890 read GetItem write SetItem;
  end;

  //REGISTRO C990: ENCERRAMENTO DO BLOCO C
  TRegistroC990 = class
  private
    fQTD_LIN_C         : Integer;      //02	QTD_LIN_C	Quantidade total de linhas do Bloco C	N	-	-
  public
    property QTD_LIN_C : Integer read FQTD_LIN_C write FQTD_LIN_C;
  end;

implementation

{ TRegistroC175List }

function TRegistroC175List.GetItem(Index: Integer): TRegistroC175;
begin
  Result := TRegistroC175(Inherited Items[Index]);
end;

procedure TRegistroC175List.SetItem(Index: Integer; const Value: TRegistroC175);
begin
  Put(Index, Value);
end;

function TRegistroC175List.New: TRegistroC175;
begin
  Result := TRegistroC175.Create;
  Add(Result);
end;

{ TRegistroC001 }

constructor TRegistroC001.Create;
begin
  inherited Create;
  RegistroC010 := TRegistroC010List.Create;
end;

destructor TRegistroC001.Destroy;
begin
  RegistroC010.Free;
  inherited;
end;

{TRegistroC010}

function TRegistroC010List.GetItem(Index: Integer): TRegistroC010;
begin
  Result := TRegistroC010(Inherited Items[Index]);
end;

function TRegistroC010List.New: TRegistroC010;
begin
  Result := TRegistroC010.Create;
  Add(Result);
end;

procedure TRegistroC010List.SetItem(Index: Integer; const Value: TRegistroC010);
begin
  Put(Index, Value);
end;

{ TRegistroC010 }

constructor TRegistroC010.Create;
begin
  FRegistroC100 := TRegistroC100List.Create;
  FRegistroC180 := TRegistroC180List.Create;
  FRegistroC190 := TRegistroC190List.Create;
  FRegistroC380 := TRegistroC380List.Create;
  FRegistroC395 := TRegistroC395List.Create;
  FRegistroC400 := TRegistroC400List.Create;
  FRegistroC490 := TRegistroC490List.Create;
  FRegistroC500 := TRegistroC500List.Create;
  FRegistroC600 := TRegistroC600List.Create;
  FRegistroC800 := TRegistroC800List.Create;
  FRegistroC860 := TRegistroC860List.Create;
end;

destructor TRegistroC010.Destroy;
begin
  FRegistroC100.Free;
  FRegistroC180.Free;
  FRegistroC190.Free;
  FRegistroC380.Free;
  FRegistroC395.Free;
  FRegistroC400.Free;
  FRegistroC490.Free;
  FRegistroC500.Free;
  FRegistroC600.Free;
  FRegistroC800.Free;
  FRegistroC860.Free;
  inherited;
end;

{TRegistroC100}

function TRegistroC100List.GetItem(Index: Integer): TRegistroC100;
begin
  Result := TRegistroC100(Inherited Items[Index]);
end;

function TRegistroC100List.New: TRegistroC100;
begin
  Result := TRegistroC100.Create;
  Add(Result);
end;

procedure TRegistroC100List.SetItem(Index: Integer; const Value: TRegistroC100);
begin
  Put(Index, Value);
end;

{ TRegistroC100 }

constructor TRegistroC100.Create;
begin
   FRegistroC110 := TRegistroC110List.Create;
   FRegistroC111 := TRegistroC111List.Create;
   FRegistroC120 := TRegistroC120List.Create;
   FRegistroC170 := TRegistroC170List.Create;
   FRegistroC175 := TRegistroC175List.Create;
end;

destructor TRegistroC100.Destroy;
begin
  FRegistroC110.Free;
  FRegistroC111.Free;
  FRegistroC120.Free;
  FRegistroC170.Free;
  FRegistroC175.Free;
  inherited;
end;

{TRegistroC110}

function TRegistroC110List.GetItem(Index: Integer): TRegistroC110;
begin
  Result := TRegistroC110(Inherited Items[Index]);
end;

function TRegistroC110List.New: TRegistroC110;
begin
  Result := TRegistroC110.Create;
  Add(Result);
end;

procedure TRegistroC110List.SetItem(Index: Integer; const Value: TRegistroC110);
begin
  Put(Index, Value);
end;


{TRegistroC111}

function TRegistroC111List.GetItem(Index: Integer): TRegistroC111;
begin
  Result := TRegistroC111(Inherited Items[Index]);
end;

function TRegistroC111List.New: TRegistroC111;
begin
  Result := TRegistroC111.Create;
  Add(Result);
end;

procedure TRegistroC111List.SetItem(Index: Integer; const Value: TRegistroC111);
begin
  Put(Index, Value);
end;

{TRegistroC120}

function TRegistroC120List.GetItem(Index: Integer): TRegistroC120;
begin
  Result := TRegistroC120(Inherited Items[Index]);
end;

function TRegistroC120List.New: TRegistroC120;
begin
  Result := TRegistroC120.Create;
  Add(Result);
end;

procedure TRegistroC120List.SetItem(Index: Integer; const Value: TRegistroC120);
begin
  Put(Index, Value);
end;

{TRegistroC170}

function TRegistroC170List.GetItem(Index: Integer): TRegistroC170;
begin
  Result := TRegistroC170(Inherited Items[Index]);
end;

function TRegistroC170List.New: TRegistroC170;
begin
  Result := TRegistroC170.Create;
  Add(Result);
end;

procedure TRegistroC170List.SetItem(Index: Integer; const Value: TRegistroC170);
begin
  Put(Index, Value);
end;

{TRegistroC180}

function TRegistroC180List.GetItem(Index: Integer): TRegistroC180;
begin
  Result := TRegistroC180(Inherited Items[Index]);
end;

function TRegistroC180List.New: TRegistroC180;
begin
  Result := TRegistroC180.Create;
  Add(Result);
end;

procedure TRegistroC180List.SetItem(Index: Integer; const Value: TRegistroC180);
begin
  Put(Index, Value);
end;

{ TRegistroC180 }

constructor TRegistroC180.Create;
begin
  FRegistroC181 := TRegistroC181List.Create;
  FRegistroC185 := TRegistroC185List.Create;
  FRegistroC188 := TRegistroC188List.Create;
end;

destructor TRegistroC180.Destroy;
begin
  FRegistroC181.Free;
  FRegistroC185.Free;
  FRegistroC188.Free;
  inherited;
end;

{TRegistroC181}

function TRegistroC181List.GetItem(Index: Integer): TRegistroC181;
begin
  Result := TRegistroC181(Inherited Items[Index]);
end;

function TRegistroC181List.New: TRegistroC181;
begin
  Result := TRegistroC181.Create;
  Add(Result);
end;

procedure TRegistroC181List.SetItem(Index: Integer; const Value: TRegistroC181);
begin
  Put(Index, Value);
end;

{TRegistroC185}

function TRegistroC185List.GetItem(Index: Integer): TRegistroC185;
begin
  Result := TRegistroC185(Inherited Items[Index]);
end;

function TRegistroC185List.New: TRegistroC185;
begin
  Result := TRegistroC185.Create;
  Add(Result);
end;

procedure TRegistroC185List.SetItem(Index: Integer; const Value: TRegistroC185);
begin
  Put(Index, Value);
end;

{TRegistroC188}

function TRegistroC188List.GetItem(Index: Integer): TRegistroC188;
begin
  Result := TRegistroC188(Inherited Items[Index]);
end;

function TRegistroC188List.New: TRegistroC188;
begin
  Result := TRegistroC188.Create;
  Add(Result);
end;

procedure TRegistroC188List.SetItem(Index: Integer; const Value: TRegistroC188);
begin
  Put(Index, Value);
end;

{TRegistroC190}

function TRegistroC190List.GetItem(Index: Integer): TRegistroC190;
begin
  Result := TRegistroC190(Inherited Items[Index]);
end;

function TRegistroC190List.New: TRegistroC190;
begin
  Result := TRegistroC190.Create;
  Add(Result);
end;

procedure TRegistroC190List.SetItem(Index: Integer; const Value: TRegistroC190);
begin
  Put(Index, Value);
end;

{ TRegistroC190 }

constructor TRegistroC190.Create;
begin
  FRegistroC191 := TRegistroC191List.Create;  /// BLOCO C - Lista de RegistroC190 (FILHO)
  FRegistroC195 := TRegistroC195List.Create;  /// BLOCO C - Lista de RegistroC195 (FILHO)
  FRegistroC198 := TRegistroC198List.Create;  /// BLOCO C - Lista de RegistroC195 (FILHO)
  FRegistroC199 := TRegistroC199List.Create;  /// BLOCO C - Lista de RegistroC195 (FILHO)
end;

destructor TRegistroC190.Destroy;
begin
  FRegistroC191.Free;
  FRegistroC195.Free;
  FRegistroC198.Free;
  FRegistroC199.Free;
  inherited;
end;

{TRegistroC191}

function TRegistroC191List.GetItem(Index: Integer): TRegistroC191;
begin
  Result := TRegistroC191(Inherited Items[Index]);
end;

function TRegistroC191List.New: TRegistroC191;
begin
  Result := TRegistroC191.Create;
  Add(Result);
end;

procedure TRegistroC191List.SetItem(Index: Integer; const Value: TRegistroC191);
begin
  Put(Index, Value);
end;

{TRegistroC195}

function TRegistroC195List.GetItem(Index: Integer): TRegistroC195;
begin
  Result := TRegistroC195(Inherited Items[Index]);
end;

function TRegistroC195List.New: TRegistroC195;
begin
  Result := TRegistroC195.Create;
  Add(Result);
end;

procedure TRegistroC195List.SetItem(Index: Integer; const Value: TRegistroC195);
begin
  Put(Index, Value);
end;

{TRegistroC198}

function TRegistroC198List.GetItem(Index: Integer): TRegistroC198;
begin
  Result := TRegistroC198(Inherited Items[Index]);
end;

function TRegistroC198List.New: TRegistroC198;
begin
  Result := TRegistroC198.Create;
  Add(Result);
end;

procedure TRegistroC198List.SetItem(Index: Integer; const Value: TRegistroC198);
begin
  Put(Index, Value);
end;

{TRegistroC199}

function TRegistroC199List.GetItem(Index: Integer): TRegistroC199;
begin
  Result := TRegistroC199(Inherited Items[Index]);
end;

function TRegistroC199List.New: TRegistroC199;
begin
  Result := TRegistroC199.Create;
  Add(Result);
end;

procedure TRegistroC199List.SetItem(Index: Integer; const Value: TRegistroC199);
begin
  Put(Index, Value);
end;

{TRegistroC380}

function TRegistroC380List.GetItem(Index: Integer): TRegistroC380;
begin
  Result := TRegistroC380(Inherited Items[Index]);
end;

function TRegistroC380List.New: TRegistroC380;
begin
  Result := TRegistroC380.Create;
  Add(Result);
end;

procedure TRegistroC380List.SetItem(Index: Integer; const Value: TRegistroC380);
begin
  Put(Index, Value);
end;

{ TRegistroC380 }

constructor TRegistroC380.Create;
begin
  FRegistroC381 := TRegistroC381List.Create;
  FRegistroC385 := TRegistroC385List.Create;
end;

destructor TRegistroC380.Destroy;
begin
  FRegistroC381.Free;
  FRegistroC385.Free;
  inherited;
end;

{TRegistroC381}

function TRegistroC381List.GetItem(Index: Integer): TRegistroC381;
begin
  Result := TRegistroC381(Inherited Items[Index]);
end;

function TRegistroC381List.New: TRegistroC381;
begin
  Result := TRegistroC381.Create;
  Add(Result);
end;

procedure TRegistroC381List.SetItem(Index: Integer; const Value: TRegistroC381);
begin
  Put(Index, Value);
end;

{TRegistroC385}

function TRegistroC385List.GetItem(Index: Integer): TRegistroC385;
begin
  Result := TRegistroC385(Inherited Items[Index]);
end;

function TRegistroC385List.New: TRegistroC385;
begin
  Result := TRegistroC385.Create;
  Add(Result);
end;

procedure TRegistroC385List.SetItem(Index: Integer; const Value: TRegistroC385);
begin
  Put(Index, Value);
end;

{TRegistroC395}

function TRegistroC395List.GetItem(Index: Integer): TRegistroC395;
begin
  Result := TRegistroC395(Inherited Items[Index]);
end;

function TRegistroC395List.New: TRegistroC395;
begin
  Result := TRegistroC395.Create;
  Add(Result);
end;

procedure TRegistroC395List.SetItem(Index: Integer; const Value: TRegistroC395);
begin
  Put(Index, Value);
end;

{ TRegistroC395 }

constructor TRegistroC395.Create;
begin
  FRegistroC396 := TRegistroC396List.Create;
end;

destructor TRegistroC395.Destroy;
begin
  FRegistroC396.Free;
  inherited;
end;

{TRegistroC396}

function TRegistroC396List.GetItem(Index: Integer): TRegistroC396;
begin
  Result := TRegistroC396(Inherited Items[Index]);
end;

function TRegistroC396List.New: TRegistroC396;
begin
  Result := TRegistroC396.Create;
  Add(Result);
end;

procedure TRegistroC396List.SetItem(Index: Integer; const Value: TRegistroC396);
begin
  Put(Index, Value);
end;

{TRegistroC400}

function TRegistroC400List.GetItem(Index: Integer): TRegistroC400;
begin
  Result := TRegistroC400(Inherited Items[Index]);
end;

function TRegistroC400List.New: TRegistroC400;
begin
  Result := TRegistroC400.Create;
  Add(Result);
end;

procedure TRegistroC400List.SetItem(Index: Integer; const Value: TRegistroC400);
begin
  Put(Index, Value);
end;

{ TRegistroC400 }

constructor TRegistroC400.Create;
begin
  FRegistroC405 := TRegistroC405List.Create;
  FRegistroC489 := TRegistroC489List.Create;
end;

destructor TRegistroC400.Destroy;
begin
  FRegistroC405.Free;
  FRegistroC489.Free;
  inherited;
end;

{TRegistroC405}

function TRegistroC405List.GetItem(Index: Integer): TRegistroC405;
begin
  Result := TRegistroC405(Inherited Items[Index]);
end;

function TRegistroC405List.New: TRegistroC405;
begin
  Result := TRegistroC405.Create;
  Add(Result);
end;

procedure TRegistroC405List.SetItem(Index: Integer; const Value: TRegistroC405);
begin
  Put(Index, Value);
end;

{ TRegistroC405 }

constructor TRegistroC405.Create;
begin
  FRegistroC481 := TRegistroC481List.Create;
  FRegistroC485 := TRegistroC485List.Create;
end;

destructor TRegistroC405.Destroy;
begin
  FRegistroC481.Free;
  FRegistroC485.Free;
  inherited;
end;

{TRegistroC481}

function TRegistroC481List.GetItem(Index: Integer): TRegistroC481;
begin
  Result := TRegistroC481(Inherited Items[Index]);
end;

function TRegistroC481List.New: TRegistroC481;
begin
  Result := TRegistroC481.Create;
  Add(Result);
end;

procedure TRegistroC481List.SetItem(Index: Integer; const Value: TRegistroC481);
begin
  Put(Index, Value);
end;

{TRegistroC485}

function TRegistroC485List.GetItem(Index: Integer): TRegistroC485;
begin
  Result := TRegistroC485(Inherited Items[Index]);
end;

function TRegistroC485List.New: TRegistroC485;
begin
  Result := TRegistroC485.Create;
  Add(Result);
end;

procedure TRegistroC485List.SetItem(Index: Integer; const Value: TRegistroC485);
begin
  Put(Index, Value);
end;

{TRegistroC489}

function TRegistroC489List.GetItem(Index: Integer): TRegistroC489;
begin
  Result := TRegistroC489(Inherited Items[Index]);
end;

function TRegistroC489List.New: TRegistroC489;
begin
  Result := TRegistroC489.Create;
  Add(Result);
end;

procedure TRegistroC489List.SetItem(Index: Integer; const Value: TRegistroC489);
begin
  Put(Index, Value);
end;

{TRegistroC490}

function TRegistroC490List.GetItem(Index: Integer): TRegistroC490;
begin
  Result := TRegistroC490(Inherited Items[Index]);
end;

function TRegistroC490List.New: TRegistroC490;
begin
  Result := TRegistroC490.Create;
  Add(Result);
end;

procedure TRegistroC490List.SetItem(Index: Integer; const Value: TRegistroC490);
begin
  Put(Index, Value);
end;

{ TRegistroC490 }

constructor TRegistroC490.Create;
begin
  FRegistroC491 := TRegistroC491List.Create;
  FRegistroC495 := TRegistroC495List.Create;
  FRegistroC499 := TRegistroC499List.Create;
end;

destructor TRegistroC490.Destroy;
begin
  FRegistroC491.Free;
  FRegistroC495.Free;
  FRegistroC499.Free;
  inherited;
end;

{TRegistroC491}

function TRegistroC491List.GetItem(Index: Integer): TRegistroC491;
begin
  Result := TRegistroC491(Inherited Items[Index]);
end;

function TRegistroC491List.New: TRegistroC491;
begin
  Result := TRegistroC491.Create;
  Add(Result);
end;

procedure TRegistroC491List.SetItem(Index: Integer; const Value: TRegistroC491);
begin
  Put(Index, Value);
end;

{TRegistroC495}

function TRegistroC495List.GetItem(Index: Integer): TRegistroC495;
begin
  Result := TRegistroC495(Inherited Items[Index]);
end;

function TRegistroC495List.New: TRegistroC495;
begin
  Result := TRegistroC495.Create;
  Add(Result);
end;

procedure TRegistroC495List.SetItem(Index: Integer; const Value: TRegistroC495);
begin
  Put(Index, Value);
end;

{TRegistroC499}

function TRegistroC499List.GetItem(Index: Integer): TRegistroC499;
begin
  Result := TRegistroC499(Inherited Items[Index]);
end;

function TRegistroC499List.New: TRegistroC499;
begin
  Result := TRegistroC499.Create;
  Add(Result);
end;

procedure TRegistroC499List.SetItem(Index: Integer; const Value: TRegistroC499);
begin
  Put(Index, Value);
end;

{TRegistroC500}

function TRegistroC500List.GetItem(Index: Integer): TRegistroC500;
begin
  Result := TRegistroC500(Inherited Items[Index]);
end;

function TRegistroC500List.New: TRegistroC500;
begin
  Result := TRegistroC500.Create;
  Add(Result);
end;

procedure TRegistroC500List.SetItem(Index: Integer; const Value: TRegistroC500);
begin
  Put(Index, Value);
end;

{ TRegistroC500 }

constructor TRegistroC500.Create;
begin
  FRegistroC501 := TRegistroC501List.Create;
  FRegistroC505 := TRegistroC505List.Create;
  FRegistroC509 := TRegistroC509List.Create;
end;

destructor TRegistroC500.Destroy;
begin
  FRegistroC501.Free;
  FRegistroC505.Free;
  FRegistroC509.Free;
  inherited;
end;

{TRegistroC501}

function TRegistroC501List.GetItem(Index: Integer): TRegistroC501;
begin
  Result := TRegistroC501(Inherited Items[Index]);
end;

function TRegistroC501List.New: TRegistroC501;
begin
  Result := TRegistroC501.Create;
  Add(Result);
end;

procedure TRegistroC501List.SetItem(Index: Integer; const Value: TRegistroC501);
begin
  Put(Index, Value);
end;

{TRegistroC505}

function TRegistroC505List.GetItem(Index: Integer): TRegistroC505;
begin
  Result := TRegistroC505(Inherited Items[Index]);
end;

function TRegistroC505List.New: TRegistroC505;
begin
  Result := TRegistroC505.Create;
  Add(Result);
end;

procedure TRegistroC505List.SetItem(Index: Integer; const Value: TRegistroC505);
begin
  Put(Index, Value);
end;

{TRegistroC509}

function TRegistroC509List.GetItem(Index: Integer): TRegistroC509;
begin
  Result := TRegistroC509(Inherited Items[Index]);
end;

function TRegistroC509List.New: TRegistroC509;
begin
  Result := TRegistroC509.Create;
  Add(Result);
end;

procedure TRegistroC509List.SetItem(Index: Integer; const Value: TRegistroC509);
begin
  Put(Index, Value);
end;

{TRegistroC600}

function TRegistroC600List.GetItem(Index: Integer): TRegistroC600;
begin
  Result := TRegistroC600(Inherited Items[Index]);
end;

function TRegistroC600List.New: TRegistroC600;
begin
  Result := TRegistroC600.Create;
  Add(Result);
end;

procedure TRegistroC600List.SetItem(Index: Integer; const Value: TRegistroC600);
begin
  Put(Index, Value);
end;

{ TRegistroC600 }

constructor TRegistroC600.Create;
begin
  FRegistroC601 := TRegistroC601List.Create;
  FRegistroC605 := TRegistroC605List.Create;
  FRegistroC609 := TRegistroC609List.Create;
end;

destructor TRegistroC600.Destroy;
begin
  FRegistroC601.Free;
  FRegistroC605.Free;
  FRegistroC609.Free;
  inherited;
end;

{TRegistroC601}

function TRegistroC601List.GetItem(Index: Integer): TRegistroC601;
begin
  Result := TRegistroC601(Inherited Items[Index]);
end;

function TRegistroC601List.New: TRegistroC601;
begin
  Result := TRegistroC601.Create;
  Add(Result);
end;

procedure TRegistroC601List.SetItem(Index: Integer; const Value: TRegistroC601);
begin
  Put(Index, Value);
end;

{TRegistroC605}

function TRegistroC605List.GetItem(Index: Integer): TRegistroC605;
begin
  Result := TRegistroC605(Inherited Items[Index]);
end;

function TRegistroC605List.New: TRegistroC605;
begin
  Result := TRegistroC605.Create;
  Add(Result);
end;

procedure TRegistroC605List.SetItem(Index: Integer; const Value: TRegistroC605);
begin
  Put(Index, Value);
end;

{TRegistroC609}

function TRegistroC609List.GetItem(Index: Integer): TRegistroC609;
begin
  Result := TRegistroC609(Inherited Items[Index]);
end;

function TRegistroC609List.New: TRegistroC609;
begin
  Result := TRegistroC609.Create;
  Add(Result);
end;

procedure TRegistroC609List.SetItem(Index: Integer; const Value: TRegistroC609);
begin
  Put(Index, Value);
end;

{TRegistroC800}

function TRegistroC800List.GetItem(Index: Integer): TRegistroC800;
begin
  Result := TRegistroC800(Inherited Items[Index]);
end;

function TRegistroC800List.New: TRegistroC800;
begin
  Result := TRegistroC800.Create;
  Add(Result);
end;

procedure TRegistroC800List.SetItem(Index: Integer; const Value: TRegistroC800);
begin
  Put(Index, Value);
end;

{ TRegistroC800 }

constructor TRegistroC800.Create;
begin
  FRegistroC810 := TRegistroC810List.Create;
  FRegistroC820 := TRegistroC820List.Create;
  FRegistroC830 := TRegistroC830List.Create;
end;

destructor TRegistroC800.Destroy;
begin
  FRegistroC810.Free;
  FRegistroC820.Free;
  FRegistroC830.Free;
  inherited;
end;

{ TRegistroC810List }

function TRegistroC810List.GetItem(Index: Integer): TRegistroC810;
begin
  Result := TRegistroC810(Inherited Items[Index]);
end;

function TRegistroC810List.New: TRegistroC810;
begin
  Result := TRegistroC810.Create;
  Add(Result);
end;

procedure TRegistroC810List.SetItem(Index: Integer; const Value: TRegistroC810);
begin
  Put(Index, Value);
end;

{ TRegistroC820List }

function TRegistroC820List.GetItem(Index: Integer): TRegistroC820;
begin
  Result := TRegistroC820(Inherited Items[Index]);
end;

function TRegistroC820List.New: TRegistroC820;
begin
	Result := TRegistroC820.Create;
	Add(Result);
end;

procedure TRegistroC820List.SetItem(Index: Integer; const Value: TRegistroC820);
begin
	Put(Index, Value);
end;

{ TRegistroC830List }

function TRegistroC830List.GetItem(Index: Integer): TRegistroC830;
begin
	Result := TRegistroC830(Inherited Items[Index]);
end;

function TRegistroC830List.New: TRegistroC830;
begin
	Result := TRegistroC830.Create;
	Add(Result);
end;

procedure TRegistroC830List.SetItem(Index: Integer; const Value: TRegistroC830);
begin
	Put(Index, Value);
end;

{ TRegistroC860List }

function TRegistroC860List.GetItem(Index: Integer): TRegistroC860;
begin
	Result := TRegistroC860(Inherited Items[Index]);
end;

function TRegistroC860List.New: TRegistroC860;
begin
	Result := TRegistroC860.Create;
	Add(Result);
end;

procedure TRegistroC860List.SetItem(Index: Integer; const Value: TRegistroC860);
begin
	Put(Index, Value);
end;

{ TRegistroC870List }

function TRegistroC870List.GetItem(Index: Integer): TRegistroC870;
begin
	Result := TRegistroC870(Inherited Items[Index]);
end;

function TRegistroC870List.New: TRegistroC870;
begin
	Result := TRegistroC870.Create;
	Add(Result);
end;

procedure TRegistroC870List.SetItem(Index: Integer; const Value: TRegistroC870);
begin
	Put(Index, Value);
end;

{ TRegistroC880List }

function TRegistroC880List.GetItem(Index: Integer): TRegistroC880;
begin
	Result := TRegistroC880(Inherited Items[Index]);
end;

function TRegistroC880List.New: TRegistroC880;
begin
	Result := TRegistroC880.Create;
	Add(Result);
end;

procedure TRegistroC880List.SetItem(Index: Integer; const Value: TRegistroC880);
begin
	Put(Index, Value);
end;

{ TRegistroC890List }

function TRegistroC890List.GetItem(Index: Integer): TRegistroC890;
begin
	Result := TRegistroC890(Inherited Items[Index]);
end;

function TRegistroC890List.New: TRegistroC890;
begin
	Result := TRegistroC890.Create;
	Add(Result);
end;

procedure TRegistroC890List.SetItem(Index: Integer; const Value: TRegistroC890);
begin
	Put(Index, Value);
end;

{ TRegistroC860 }

constructor TRegistroC860.Create;
begin
 FRegistroC870 := TRegistroC870List.Create;
 FRegistroC880 := TRegistroC880List.Create;
 FRegistroC890 := TRegistroC890List.Create;
end;

destructor TRegistroC860.Destroy;
begin

  inherited;
end;

end.
