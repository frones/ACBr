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

unit ACBrEPCBloco_1;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEPCBlocos;

type
  TRegistro1010List = class;
  TRegistro1020List = class;
  TRegistro1050List = class;
  TRegistro1100List = class;
  TRegistro1101List = class;
  TRegistro1102 = class;
  TRegistro1200List = class;
  TRegistro1210List = class;
  TRegistro1220List = class;
  TRegistro1300List = class;
  TRegistro1500List = class;
  TRegistro1501List = class;
  TRegistro1502 = class;
  TRegistro1600List = class;
  TRegistro1610List = class;
  TRegistro1620List = class;
  TRegistro1700List = class;
  TRegistro1800List = class;
  TRegistro1809List = class;
  TRegistro1900List = class;

  //REGISTRO 1001: ABERTURA DO BLOCO 1
  TRegistro1001 = class(TOpenBlocos)
  private
    FRegistro1010: TRegistro1010List; // NIVEL 2
    FRegistro1020: TRegistro1020List; // NIVEL 2
    FRegistro1050: TRegistro1050List; // NIVEL 2
    FRegistro1100: TRegistro1100List; // NIVEL 2
    FRegistro1200: TRegistro1200List; // NIVEL 2
    FRegistro1300: TRegistro1300List; // NIVEL 2
    FRegistro1500: TRegistro1500List; // NIVEL 2
    FRegistro1600: TRegistro1600List; // NIVEL 2
    FRegistro1700: TRegistro1700List; // NIVEL 2
    FRegistro1800: TRegistro1800List; // NIVEL 2
    FRegistro1900: TRegistro1900List; // NIVEL 2
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property Registro1010: TRegistro1010List read FRegistro1010 write FRegistro1010;
    property Registro1020: TRegistro1020List read FRegistro1020 write FRegistro1020;
    property Registro1050: TRegistro1050List read FRegistro1050 write FRegistro1050;
    property Registro1100: TRegistro1100List read FRegistro1100 write FRegistro1100;
    property Registro1200: TRegistro1200List read FRegistro1200 write FRegistro1200;
    property Registro1300: TRegistro1300List read FRegistro1300 write FRegistro1300;
    property Registro1500: TRegistro1500List read FRegistro1500 write FRegistro1500;
    property Registro1600: TRegistro1600List read FRegistro1600 write FRegistro1600;
    property Registro1700: TRegistro1700List read FRegistro1700 write FRegistro1700;
    property Registro1800: TRegistro1800List read FRegistro1800 write FRegistro1800;
    property Registro1900: TRegistro1900List read FRegistro1900 write FRegistro1900;
  end;

  //REGISTRO 1010: PROCESSO REFERENCIADO – AÇÃO JUDICIAL
  TRegistro1010 = class
  private
    fNUM_PROC: string;	          //02	NUM_PROC	Identificação do Numero do Processo Judicial	C	020	-
    fID_SEC_JUD: string;	        //03	ID_SEC_JUD	Identificação da Seção Judiciária	C	-	-
    fID_VARA: string;	            //04	ID_VARA	Identificação da Vara	C	002	-
    fIND_NAT_ACAO: Integer;       //05	IND_NAT_ACAO	Indicador da Natureza da Ação Judicial, impetrada na Justiça Federal:01 – Decisão Judicial Transitada em Julgado, a favor da Pessoa Jurídica.02 – Decisão Judicial Não Transitada em Julgado, a favor da Pessoa Jurídica.03 – Decisão Judicial oriunda de Liminar em Mandado de Segurança.04 – Decisão Judicial oriunda de Liminar em Medida Cautelar.05 – Decisão Judicial oriunda de Antecipação de Tutela.06 - Decisão Judicial Vinculada a Depósito Administrativo ou Judicial em Montante Integral.07 – Medida Judicial em que a Pessoa Jurídica não é o autor.08 – Súmula Vinculante aprovada pelo STF.99 - Outros.	C	002*	-
    fDESC_DEC_JUD: string;        //06	DESC_DEC_JUD	Descrição Resumida dos Efeitos Tributários abrangidos pela Decisão Judicial proferida.	C	100	-
    fDT_SENT_JUD: TDateTime;	    //07	DT_SENT_JUD	Data da Sentença/Decisão Judicial	N	008*	-
  public
    property NUM_PROC: string read FNUM_PROC write FNUM_PROC;
    property ID_SEC_JUD: string read FID_SEC_JUD write FID_SEC_JUD;
    property ID_VARA: string read FID_VARA write FID_VARA;
    property IND_NAT_ACAO: Integer read FIND_NAT_ACAO write FIND_NAT_ACAO;
    property DESC_DEC_JUD: string read FDESC_DEC_JUD write FDESC_DEC_JUD;
    property DT_SENT_JUD: TDateTime read FDT_SENT_JUD write FDT_SENT_JUD;
  end;

  // Registro 1010 - Lista
  TRegistro1010List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1010;
    procedure SetItem(Index: Integer; const Value: TRegistro1010);
  public
    function New: TRegistro1010;
    property Items[Index: Integer]: TRegistro1010 read GetItem write SetItem;
  end;

  //REGISTRO 1020: PROCESSO REFERENCIADO – PROCESSO ADMINISTRATIVO
  TRegistro1020 = class
  private
    fNUM_PROC: string;	          //02	NUM_PROC	Identificação do Processo Administrativo ou da Decisão Administrativa	C	020	-
    fIND_NAT_ACAO: Integer;       //03	IND_NAT_ACAO	Indicador da Natureza da Ação, decorrente de Processo Administrativo na Secretaria da Receita Federal do Brasil:01 – Processo Administrativo de Consulta;02 – Despacho Decisório;03 – Ato Declaratório Executivo;04 – Ato Declaratório Interpretativo;05 – Decisão Administrativa de DRJ ou do CARF;06 – Auto de Infração;99 – Outros	C	002*	-
    fDT_DEC_ADM: TDateTime;	      //04	DT_DEC_ADM	Data do Despacho/Decisão Administrativa	N	008*	-
  public
    property NUM_PROC: string read FNUM_PROC write FNUM_PROC;
    property IND_NAT_ACAO: Integer read FIND_NAT_ACAO write FIND_NAT_ACAO;
    property DT_DEC_ADM: TDateTime read FDT_DEC_ADM write FDT_DEC_ADM;
  end;

  // Registro 1020 - Lista
  TRegistro1020List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1020;
    procedure SetItem(Index: Integer; const Value: TRegistro1020);
  public
    function New: TRegistro1020;
    property Items[Index: Integer]: TRegistro1020 read GetItem write SetItem;
  end;


 //REGISTRO 1050:  Detalhamento de Ajustes de Base de Cálculo – Valores Extra Apuração
  TRegistro1050 = class
  private
    FVL_AJ_CST01            : currency;
    FVL_AJ_CST06            : currency;
    FVL_AJ_CST07            : currency;
    FCNPJ                   : String;
    FVL_AJ_CST04            : currency;
    FVL_AJ_CST05            : currency;
    FNUM_REC                : String;
    FDT_REF                 : TDateTime;
    FVL_AJ_TOT              : currency;
    FVL_AJ_CST99            : currency;
    FIND_AJ_BC              : TACBrTabCodAjBaseCalcContrib;
    FINFO_COMPL             : String;
    FVL_AJ_CST08            : currency;
    FVL_AJ_CST09            : currency;
    FIND_APROP              : TACBrIndicadorApropAjuste;
    FVL_AJ_CST49            : currency;
    FVL_AJ_CST02            : currency;
    FVL_AJ_CST03            : currency;
  public
    property DT_REF         : TDateTime                      read FDT_REF          write FDT_REF;
    property IND_AJ_BC      : TACBrTabCodAjBaseCalcContrib   read FIND_AJ_BC       write FIND_AJ_BC;
    property CNPJ           : String                         read FCNPJ            write FCNPJ;
    property VL_AJ_TOT      : currency                       read FVL_AJ_TOT       write FVL_AJ_TOT;
    property VL_AJ_CST01    : currency                       read FVL_AJ_CST01     write FVL_AJ_CST01;
    property VL_AJ_CST02    : currency                       read FVL_AJ_CST02     write FVL_AJ_CST02;
    property VL_AJ_CST03    : currency                       read FVL_AJ_CST03     write FVL_AJ_CST03;
    property VL_AJ_CST04    : currency                       read FVL_AJ_CST04     write FVL_AJ_CST04;
    property VL_AJ_CST05    : currency                       read FVL_AJ_CST05     write FVL_AJ_CST05;
    property VL_AJ_CST06    : currency                       read FVL_AJ_CST06     write FVL_AJ_CST06;
    property VL_AJ_CST07    : currency                       read FVL_AJ_CST07     write FVL_AJ_CST07;
    property VL_AJ_CST08    : currency                       read FVL_AJ_CST08     write FVL_AJ_CST08;
    property VL_AJ_CST09    : currency                       read FVL_AJ_CST09     write FVL_AJ_CST09;
    property VL_AJ_CST49    : currency                       read FVL_AJ_CST49     write FVL_AJ_CST49;
    property VL_AJ_CST99    : currency                       read FVL_AJ_CST99     write FVL_AJ_CST99;
    property IND_APROP      : TACBrIndicadorApropAjuste      read FIND_APROP       write FIND_APROP;
    property NUM_REC        : String                         read FNUM_REC         write FNUM_REC;
    property INFO_COMPL     : String                         read FINFO_COMPL      write FINFO_COMPL;
  end;


  // Registro 1050 - Lista
  TRegistro1050List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1050;
    procedure SetItem(Index: Integer; const Value: TRegistro1050);
  public
    function New: TRegistro1050;
    property Items[Index: Integer]: TRegistro1050 read GetItem write SetItem;
  end;


  //REGISTRO 1100: CONTROLE DE CRÉDITOS FISCAIS – PIS/PASEP
  TRegistro1100 = class
  private
    fPER_APU_CRED: Integer;	             //02	PER_APU_CRED	Período de Apuração do Crédito (MM/AAAA)	N	006	-
    fORIG_CRED: Integer;	               //03	ORIG_CRED	Indicador da origem do crédito:01 – Crédito decorrente de operações próprias;02 – Crédito transferido por pessoa jurídica sucedida.	N	002*	-
    fCNPJ_SUC: string;	                 //04	CNPJ_SUC	CNPJ da pessoa jurídica cedente do crédito (se ORIG_CRED = 02).	N	014*	-
    fCOD_CRED: Integer;	                 //05	COD_CRED	Código do Tipo do Crédito, conforme Tabela 4.3.6.	N	003*	-
    fVL_CRED_APU: Currency;	             //06	VL_CRED_APU	Valor do Crédito apurado na Escrituração Fiscal Digital ou em demonstrativo DACON de períodos anteriores.  	N	-	02
    fVL_CRED_EXT_APU: Currency;	         //07	VL_CRED_EXT_APU	Valor de Crédito Extemporâneo Apurado (Registro 1101), referente a Período Anterior, Informado no Campo 02 – PER_APU_CRED	N	-	02
    fVL_TOT_CRED_APU: Currency;	         //08	VL_TOT_CRED_APU	Valor Total do Crédito Apurado (06 + 07)	N	-	02
    fVL_CRED_DESC_PA_ANT: Currency;	     //09	VL_CRED_DESC_PA_ANT	Valor do Crédito utilizado mediante Desconto, em Período(s)  Anterior(es).	N	-	02
    fVL_CRED_PER_PA_ANT: Currency;	     //10	VL_CRED_PER_PA_ANT	Valor do Crédito utilizado mediante Pedido de Ressarcimento, em Período(s) Anterior(es).	N	-	02
    fVL_CRED_DCOMP_PA_ANT: Currency;     //11	VL_CRED_DCOMP_PA_ANT	Valor do Crédito utilizado mediante Declaração de Compensação Intermediária (Crédito de Exportação), em Período(s) Anterior(es).	N	-	02
    fSD_CRED_DISP_EFD: Currency;	       //12	SD_CRED_DISP_EFD	Saldo do Crédito Disponível para Utilização neste Período de Escrituração (08 – 09 – 10 - 11).	N	-	02
    fVL_CRED_DESC_EFD: Currency;	       //13	VL_CRED_DESC_EFD	Valor do Crédito descontado neste período de escrituração.	N	-	02
    fVL_CRED_PER_EFD: Currency;	         //14	VL_CRED_PER_EFD	Valor do Crédito objeto de Pedido de Ressarcimento (PER) neste período de escrituração.	N	-	02
    fVL_CRED_DCOMP_EFD: Currency;	       //15	VL_CRED_DCOMP_EFD	Valor do Crédito utilizado mediante Declaração de Compensação Intermediária neste período de escrituração.	N	-	02
    fVL_CRED_TRANS: Currency;	           //16	VL_CRED_TRANS	Valor do crédito transferido em evento de cisão, fusão ou incorporação.	N	-	02
    fVL_CRED_OUT: Currency;	             //17	VL_CRED_OUT	Valor do crédito utilizado por outras formas.	N	-	02
    fSLD_CRED_FIM: Currency;	           //18	SLD_CRED_FIM	Saldo de créditos a utilizar em período de apuração futuro (12 – 13 – 14 – 15 – 16 - 17).	N	-	02

    FRegistro1101: TRegistro1101List; // NIVEL 3
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property PER_APU_CRED: Integer read FPER_APU_CRED write FPER_APU_CRED;
    property ORIG_CRED: Integer read FORIG_CRED write FORIG_CRED;
    property CNPJ_SUC: string read FCNPJ_SUC write FCNPJ_SUC;
    property COD_CRED: Integer read FCOD_CRED write FCOD_CRED;
    property VL_CRED_APU: Currency read FVL_CRED_APU write FVL_CRED_APU;
    property VL_CRED_EXT_APU: Currency read FVL_CRED_EXT_APU write FVL_CRED_EXT_APU;
    property VL_TOT_CRED_APU: Currency read FVL_TOT_CRED_APU write FVL_TOT_CRED_APU;
    property VL_CRED_DESC_PA_ANT: Currency read FVL_CRED_DESC_PA_ANT write FVL_CRED_DESC_PA_ANT;
    property VL_CRED_PER_PA_ANT: Currency read FVL_CRED_PER_PA_ANT write FVL_CRED_PER_PA_ANT;
    property VL_CRED_DCOMP_PA_ANT: Currency read FVL_CRED_DCOMP_PA_ANT write FVL_CRED_DCOMP_PA_ANT;
    property SD_CRED_DISP_EFD: Currency read FSD_CRED_DISP_EFD write FSD_CRED_DISP_EFD;
    property VL_CRED_DESC_EFD: Currency read FVL_CRED_DESC_EFD write FVL_CRED_DESC_EFD;
    property VL_CRED_PER_EFD: Currency read FVL_CRED_PER_EFD write FVL_CRED_PER_EFD;
    property VL_CRED_DCOMP_EFD: Currency read FVL_CRED_DCOMP_EFD write FVL_CRED_DCOMP_EFD;
    property VL_CRED_TRANS: Currency read FVL_CRED_TRANS write FVL_CRED_TRANS;
    property VL_CRED_OUT: Currency read FVL_CRED_OUT write FVL_CRED_OUT;
    property SLD_CRED_FIM: Currency read FSLD_CRED_FIM write FSLD_CRED_FIM;

    property Registro1101: TRegistro1101List read FRegistro1101 write FRegistro1101;
  end;

  // Registro 1100 - Lista
  TRegistro1100List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1100;
    procedure SetItem(Index: Integer; const Value: TRegistro1100);
  public
    function New: TRegistro1100;
    property Items[Index: Integer]: TRegistro1100 read GetItem write SetItem;
  end;

  //REGISTRO 1101: APURAÇÃO DE CRÉDITO EXTEMPORÂNEO - DOCUMENTOS E OPERAÇÕES DE PERÍODOS ANTERIORES – PIS/PASEP
  TRegistro1101 = class
  private
    fCOD_PART: string;	            //02	COD_PART	Código do participante (Campo 02 do Registro 0150)	C	060	-
    fCOD_ITEM: string;	            //03	COD_ITEM	Código do item (campo 02 do Registro 0200)	C	060	-
    fCOD_MOD: string;	              //04	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1.	C	002*	-
    fSER: string;	                  //05	SER	Série do documento fiscal	C	004	-
    fSUB_SER: string;	              //06	SUB_SER	Subsérie do documento fiscal	C	003	-
    fNUM_DOC: Integer;	            //07	NUM_DOC	Número do documento fiscal	N	009	-
    fDT_OPER: TDateTime;	          //08	DT_OPER	Data da Operação (ddmmaaaa)	N	008*	-
    fCHV_NFE: String;	            //09	CHV_NFE	Chave da Nota Fiscal Eletrônica	N	044*	-
    fVL_OPER: Currency;	            //10	VL_OPER	Valor da Operação	N	-	02
    fCFOP: Integer;	                //11	CFOP	Código fiscal de operação e prestação	N	004*	-
    fNAT_BC_CRED: string;	          //12	NAT_BC_CRED	Código da Base de Cálculo do Crédito, conforme a Tabela indicada no item 4.3.7.	C	002*	-
    fIND_ORIG_CRED: Integer;	      //13	IND_ORIG_CRED	Indicador da origem do crédito:0 – Operação no Mercado Interno;1 – Operação de Importação	C	001*	-
    fCST_PIS: Integer;	            //14	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP, conforme a Tabela indicada no item 4.3.3.	N	002*	-
    fVL_BC_PIS: Currency;	          //15	VL_BC_PIS	Base de Cálculo do Crédito de PIS/PASEP (em valor ou em quantidade).	N	-	03
    fALIQ_PIS: Currency;	          //16	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual ou em reais).	N	-	04
    fVL_PIS: Currency;	            //17	VL_PIS	Valor do Crédito de PIS/PASEP.	N	-	02
    fCOD_CTA: string;	              //18	COD_CTA	Código da conta analítica contábil debitada/creditada.	C	060	-
    fCOD_CCUS: string;	            //19	COD_CCUS	Código do Centro de Custos.	C	060	-
    fDESC_COMPL: string;	          //20	DESC_COMPL	Descrição complementar do Documento/Operação.	C	-	-
    fPER_ESCRIT: Integer;	          //21	PER_ESCRIT	Mês/Ano da Escrituração em que foi registrado o documento/operação (Crédito pelo método da Apropriação Direta).	N	006*	-
    fCNPJ: string;	                //22	CNPJ	CNPJ do estabelecimento gerador do crédito extemporâneo (Campo 04  do Registro 0140)	N	014*	-

    FRegistro1102: TRegistro1102; // NIVEL 4
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_PART: string read FCOD_PART write FCOD_PART;
    property COD_ITEM: string read FCOD_ITEM write FCOD_ITEM;
    property COD_MOD: string read FCOD_MOD write FCOD_MOD;
    property SER: string read FSER write FSER;
    property SUB_SER: string read FSUB_SER write FSUB_SER;
    property NUM_DOC: Integer read FNUM_DOC write FNUM_DOC;
    property DT_OPER: TDateTime read FDT_OPER write FDT_OPER;
    property CHV_NFE: String read FCHV_NFE write FCHV_NFE;
    property VL_OPER: Currency read FVL_OPER write FVL_OPER;
    property CFOP: Integer read FCFOP write FCFOP;
    property NAT_BC_CRED: string read FNAT_BC_CRED write FNAT_BC_CRED;
    property IND_ORIG_CRED: Integer read FIND_ORIG_CRED write FIND_ORIG_CRED;
    property CST_PIS: Integer read FCST_PIS write FCST_PIS;
    property VL_BC_PIS: Currency read FVL_BC_PIS write FVL_BC_PIS;
    property ALIQ_PIS: Currency read FALIQ_PIS write FALIQ_PIS;
    property VL_PIS: Currency read FVL_PIS write FVL_PIS;
    property COD_CTA: string read FCOD_CTA write FCOD_CTA;
    property COD_CCUS: string read FCOD_CCUS write FCOD_CCUS;
    property DESC_COMPL: string read FDESC_COMPL write FDESC_COMPL;
    property PER_ESCRIT: Integer read FPER_ESCRIT write FPER_ESCRIT;
    property CNPJ: string read FCNPJ write FCNPJ;

    property Registro1102: TRegistro1102 read FRegistro1102 write FRegistro1102;
  end;

  // Registro 1101 - Lista
  TRegistro1101List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1101;
    procedure SetItem(Index: Integer; const Value: TRegistro1101);
  public
    function New: TRegistro1101;
    property Items[Index: Integer]: TRegistro1101 read GetItem write SetItem;
  end;

  //REGISTRO 1102: DETALHAMENTO DO CRÉDITO EXTEMPORANEO VINCULADO A MAIS DE UM TIPO DE RECEITA – PIS/PASEP
  TRegistro1102 = class
  private
    fVL_CRED_PIS_TRIB_MI: Variant;         //02	VL_CRED_PIS_TRIB_MI	Parcela do Crédito de PIS/PASEP, vinculada a Receita Tributada no Mercado Interno
    fVL_CRED_PIS_NT_MI: Variant;	          //03	VL_CRED_PIS_NT_MI	Parcela do Crédito de PIS/PASEP, vinculada a Receita Não Tributada no Mercado Interno
    fVL_CRED_PIS_EXP: Variant;	           //04	VL_CRED_PIS_ EXP	Parcela do Crédito de PIS/PASEP, vinculada a Receita de Exportação
  public
    property VL_CRED_PIS_TRIB_MI: Variant read FVL_CRED_PIS_TRIB_MI write FVL_CRED_PIS_TRIB_MI;
    property VL_CRED_PIS_NT_MI: Variant read FVL_CRED_PIS_NT_MI write FVL_CRED_PIS_NT_MI;
    property VL_CRED_PIS_EXP: Variant read FVL_CRED_PIS_EXP write FVL_CRED_PIS_EXP;
  end;

  //REGISTRO 1200: CONTRIBUIÇÃO SOCIAL EXTEMPORÂNEA – PIS/PASEP
  TRegistro1200 = class
  private
    fPER_APUR_ANT: Integer;            //02	PER_APUR_ANT	Período de Apuração da Contribuição Social Extemporânea (MMAAAA).	N	006*	-
    fNAT_CONT_REC: string;             //03	NAT_CONT_REC	Natureza da Contribuição a Recolher, conforme Tabela 4.3.5.	C	002	-
    fVL_CONT_APUR: Currency;           //04	VL_CONT_APUR	Valor da Contribuição Apurada.	N	-	02
    fVL_CRED_PIS_DESC: Currency;       //05	VL_CRED_PIS_DESC	Valor do Crédito de PIS/PASEP a Descontar, da Contribuição Social Extemporânea.	N	-	02
    fVL_CONT_DEV: Currency;	           //06	VL_CONT_DEV	Valor da Contribuição Social Extemporânea Devida.	N	-	02
    fVL_OUT_DED: Currency;	           //07	VL_OUT_DED	Valor de Outras Deduções.	N	-	02
    fVL_CONT_EXT: Currency;	           //08	VL_CONT_EXT	Valor da Contribuição Social Extemporânea a pagar.	N	-	02
    fVL_MUL: Currency;	               //09	VL_MUL	Valor da Multa.	N	-	02
    fVL_JUR: Currency;	               //10	VL_JUR	Valor dos Juros.	N	-	02
    fDT_RECOL: TDateTime;	             //11	DT_RECOL	Data do Recolhimento.	N	008*	-

    FRegistro1210: TRegistro1210List; // NIVEL 3
    FRegistro1220: TRegistro1220List; // NIVEL 3
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property PER_APUR_ANT: Integer read FPER_APUR_ANT write FPER_APUR_ANT;
    property NAT_CONT_REC: string read FNAT_CONT_REC write FNAT_CONT_REC;
    property VL_CONT_APUR: Currency read FVL_CONT_APUR write FVL_CONT_APUR;
    property VL_CRED_PIS_DESC: Currency read FVL_CRED_PIS_DESC write FVL_CRED_PIS_DESC;
    property VL_CONT_DEV: Currency read FVL_CONT_DEV write FVL_CONT_DEV;
    property VL_OUT_DED: Currency read FVL_OUT_DED write FVL_OUT_DED;
    property VL_CONT_EXT: Currency read FVL_CONT_EXT write FVL_CONT_EXT;
    property VL_MUL: Currency read FVL_MUL write FVL_MUL;
    property VL_JUR: Currency read FVL_JUR write FVL_JUR;
    property DT_RECOL: TDateTime read FDT_RECOL write FDT_RECOL;

    property Registro1210: TRegistro1210List read FRegistro1210 write FRegistro1210;
    property Registro1220: TRegistro1220List read FRegistro1220 write FRegistro1220;
  end;

  // Registro 1200 - Lista
  TRegistro1200List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1200;
    procedure SetItem(Index: Integer; const Value: TRegistro1200);
  public
    function New: TRegistro1200;
    property Items[Index: Integer]: TRegistro1200 read GetItem write SetItem;
  end;

  //REGISTRO 1210: DETALHAMENTO DA CONTRIBUIÇÃO SOCIAL EXTEMPORÂNEA – PIS/PASEP
  TRegistro1210 = class
  private
    fCNPJ: string;	              //02	CNPJ	Número de inscrição do estabelecimento no CNPJ (Campo 04 do Registro 0140).	N	014*	-
    fCST_PIS: TACBrCstPis;	          //03	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP, conforme a Tabela indicada no item 4.3.3.	N	002*	-
    fCOD_PART: string;	          //04	COD_PART	Código do participante (Campo 02 do Registro 0150)	C	060	-
    fDT_OPER: TDateTime;	        //05	DT_OPER	Data da Operação (ddmmaaaa)	N	008*	-
    fVL_OPER: Currency;	          //06	VL_OPER	Valor da Operação	N	-	02
    fVL_BC_PIS: Currency;	        //07	VL_BC_PIS	Base de cálculo do PIS/PASEP (em valor ou em quantidade)	N	-	03
    fALIQ_PIS: Currency;	        //08	ALIQ_PIS	Alíquota da PIS (em percentual ou em reais)	N	-	04
    fVL_PIS: Currency;	          //09	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCOD_CTA: string;	            //10	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
    fDESC_COMPL: string;          //11	DESC_COMPL	Descrição complementar do Documento/Operação	C	-	-
  public
    property CNPJ: string read FCNPJ write FCNPJ;
    property CST_PIS: TACBrCstPis read FCST_PIS write FCST_PIS;
    property COD_PART: string read FCOD_PART write FCOD_PART;
    property DT_OPER: TDateTime read FDT_OPER write FDT_OPER;
    property VL_OPER: Currency read FVL_OPER write FVL_OPER;
    property VL_BC_PIS: Currency read FVL_BC_PIS write FVL_BC_PIS;
    property ALIQ_PIS: Currency read FALIQ_PIS write FALIQ_PIS;
    property VL_PIS: Currency read FVL_PIS write FVL_PIS;
    property COD_CTA: string read FCOD_CTA write FCOD_CTA;
    property DESC_COMPL: string read FDESC_COMPL write FDESC_COMPL;
  end;

  // Registro 1210 - Lista
  TRegistro1210List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1210;
    procedure SetItem(Index: Integer; const Value: TRegistro1210);
  public
    function New: TRegistro1210;
    property Items[Index: Integer]: TRegistro1210 read GetItem write SetItem;
  end;

  //REGISTRO 1220: DEMONSTRAÇÃO DO CRÉDITO A DESCONTAR DA CONTRIBUIÇÃO EXTEMPORÂNEA – PIS/PASEP
  TRegistro1220 = class
  private
    fPER_APU_CRED: Integer;        //02	PER_APU_CRED	Período de Apuração do Crédito (MM/AAAA)	N	006	-
    fORIG_CRED: Integer;	         //03	ORIG_CRED	Indicador da origem do crédito:01 – Crédito decorrente de operações próprias;02 – Crédito transferido por pessoa jurídica sucedida.	N	002*	-
    fCOD_CRED: Integer;	           //04	COD_CRED	Código do Tipo do Crédito, conforme Tabela 4.3.6.	N	003*	-
    fVL_CRED: Currency;	           //05	VL_CRED	Valor do Crédito a Descontar	N	-	002
  public
    property PER_APU_CRED: Integer read FPER_APU_CRED write FPER_APU_CRED;
    property ORIG_CRED: Integer read FORIG_CRED write FORIG_CRED;
    property COD_CRED: Integer read FCOD_CRED write FCOD_CRED;
    property VL_CRED: Currency read FVL_CRED write FVL_CRED;
  end;

  // Registro 1220 - Lista
  TRegistro1220List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1220;
    procedure SetItem(Index: Integer; const Value: TRegistro1220);
  public
    function New: TRegistro1220;
    property Items[Index: Integer]: TRegistro1220 read GetItem write SetItem;
  end;

  //REGISTRO 1300: CONTROLE DOS VALORES RETIDOS NA FONTE – PIS/PASEP
  TRegistro1300 = class
  private
    fIND_NAT_RET: Integer;	         //02	IND_NAT_RET	Indicador de Natureza da Retenção na Fonte:01 - Retenção por Órgãos, Autarquias e Fundações Federais;02 - Retenção por outras Entidades da Administração Pública Federal;03 - Retenção por Pessoas Jurídicas de Direito Privado;04 - Recolhimento por Sociedade Cooperativa;05 - Retenção por Fabricante de Máquinas e Veículos;99 - Outras Retenções	N	002*	-
    fPR_REC_RET: Integer;	           //03	PR_REC_RET	Período do Recebimento e da Retenção (MM/AAAA)	N	006	-
    fVL_RET_APU: Currency;	         //04	VL_RET_APU	Valor Total da Retenção	N	-	02
    fVL_RET_DED: Currency;	         //05	VL_RET_DED	Valor da Retenção deduzida da Contribuição Devida	N	-	02
    fVL_RET_PER: Currency;	         //06	VL_RET_PER	Valor da Retenção utilizada mediante Pedido de Restituição.	N	-	02
    fVL_RET_DCOMP: Currency;         //07	VL_RET_DCOMP	Valor da Retenção utilizada mediante Declaração de Compensação.	N	-	02
    fSLD_RET: Currency;              //08	SLD_RET	Saldo de Retenção a utilizar em períodos de apuração futuros (04 – 05 - 06 - 07).	N	-	02
  public
    property IND_NAT_RET: Integer read FIND_NAT_RET write FIND_NAT_RET;
    property PR_REC_RET: Integer read FPR_REC_RET write FPR_REC_RET;
    property VL_RET_APU: Currency read FVL_RET_APU write FVL_RET_APU;
    property VL_RET_DED: Currency read FVL_RET_DED write FVL_RET_DED;
    property VL_RET_PER: Currency read FVL_RET_PER write FVL_RET_PER;
    property VL_RET_DCOMP: Currency read FVL_RET_DCOMP write FVL_RET_DCOMP;
    property SLD_RET: Currency read FSLD_RET write FSLD_RET;
  end;

  // Registro 1300 - Lista
  TRegistro1300List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1300;
    procedure SetItem(Index: Integer; const Value: TRegistro1300);
  public
    function New: TRegistro1300;
    property Items[Index: Integer]: TRegistro1300 read GetItem write SetItem;
  end;

  //REGISTRO 1500: CONTROLE DE CRÉDITOS FISCAIS – COFINS
  TRegistro1500 = class
  private
    fPER_APU_CRED: Integer;	            //02	PER_APU_CRED	Período de Apuração do Crédito (MM/AAAA)	N	006	-
    fORIG_CRED: Integer;	              //03	ORIG_CRED	Indicador da origem do crédito:01 – Crédito decorrente de operações próprias;02 – Crédito transferido por pessoa jurídica sucedida.	N	002*	-
    fCNPJ_SUC: string;	                //04	CNPJ_SUC	CNPJ da pessoa jurídica cedente do crédito (se ORIG_CRED = 02).	N	014*	-
    fCOD_CRED: Integer;	                //05	COD_CRED	Código do Tipo do Crédito, conforme Tabela 4.3.6.	N	003*	-
    fVL_CRED_APU: Currency;	            //06	VL_CRED_APU	Valor do Crédito apurado na Escrituração Fiscal Digital ou em demonstrativo DACON de períodos anteriores.	N	-	02
    fVL_CRED_EXT_APU: Currency;	        //07	VL_CRED_EXT_APU	Valor de Crédito Extemporâneo Apurado (Registro 1501), referente a Período Anterior, Informado no Campo 02 – PER_APU_CRED	N	-	02
    fVL_TOT_CRED_APU: Currency;	        //08	VL_TOT_CRED_APU	Valor Total do Crédito Apurado (06 + 07)	N	-	02
    fVL_CRED_DESC_PA_ANT: Currency;	    //09	VL_CRED_DESC_PA_ANT	Valor do Crédito utilizado mediante Desconto, em Período(s)  Anterior(es) 	N	-	02
    fVL_CRED_PER_PA_ANT: Currency;	    //10	VL_CRED_PER_PA_ANT	Valor do Crédito utilizado mediante Pedido de Ressarcimento, em Período(s) Anterior(es).	N	-	02
    fVL_CRED_DCOMP_PA_ANT: Currency;    //11	VL_CRED_DCOMP_PA_ANT	Valor do Crédito utilizado mediante Declaração de Compensação Intermediária (Crédito de Exportação), em Período(s) Anterior(es)	N	-	02
    fSD_CRED_DISP_EFD: Currency;	      //12	SD_CRED_DISP_EFD	Saldo do Crédito Disponível para Utilização neste Período de Escrituração (08-09-10-11)	N	-	02
    fVL_CRED_DESC_EFD: Currency;	      //13	VL_CRED_DESC_EFD	Valor do Crédito descontado neste período de escrituração	N	-	02
    fVL_CRED_PER_EFD: Currency;	        //14	VL_CRED_PER_EFD	Valor do Crédito objeto de Pedido de Ressarcimento (PER) neste período de escrituração	N	-	02
    fVL_CRED_DCOMP_EFD: Currency;	      //15	VL_CRED_DCOMP_EFD	Valor do Crédito utilizado mediante Declaração de Compensação Intermediária neste período de escrituração	N	-	02
    fVL_CRED_TRANS: Currency;	          //16	VL_CRED_TRANS	Valor do crédito transferido em evento de cisão, fusão ou incorporação	N	-	02
    fVL_CRED_OUT: Currency;	            //17	VL_CRED_OUT	Valor do crédito utilizado por outras formas	N	-	02
    fSLD_CRED_FIM: Currency;	          //18	SLD_CRED_FIM	Saldo de créditos a utilizar em período de apuração futuro (12-13-14-15-16-17).	N	-	02

    FRegistro1501: TRegistro1501List; // NIVEL 3
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property PER_APU_CRED: Integer read FPER_APU_CRED write FPER_APU_CRED;
    property ORIG_CRED: Integer read FORIG_CRED write FORIG_CRED;
    property CNPJ_SUC: string read FCNPJ_SUC write FCNPJ_SUC;
    property COD_CRED: Integer read FCOD_CRED write FCOD_CRED;
    property VL_CRED_APU: Currency read FVL_CRED_APU write FVL_CRED_APU;
    property VL_CRED_EXT_APU: Currency read FVL_CRED_EXT_APU write FVL_CRED_EXT_APU;
    property VL_TOT_CRED_APU: Currency read FVL_TOT_CRED_APU write FVL_TOT_CRED_APU;
    property VL_CRED_DESC_PA_ANT: Currency read FVL_CRED_DESC_PA_ANT write FVL_CRED_DESC_PA_ANT;
    property VL_CRED_PER_PA_ANT: Currency read FVL_CRED_PER_PA_ANT write FVL_CRED_PER_PA_ANT;
    property VL_CRED_DCOMP_PA_ANT: Currency read FVL_CRED_DCOMP_PA_ANT write FVL_CRED_DCOMP_PA_ANT;
    property SD_CRED_DISP_EFD: Currency read FSD_CRED_DISP_EFD write FSD_CRED_DISP_EFD;
    property VL_CRED_DESC_EFD: Currency read FVL_CRED_DESC_EFD write FVL_CRED_DESC_EFD;
    property VL_CRED_PER_EFD: Currency read FVL_CRED_PER_EFD write FVL_CRED_PER_EFD;
    property VL_CRED_DCOMP_EFD: Currency read FVL_CRED_DCOMP_EFD write FVL_CRED_DCOMP_EFD;
    property VL_CRED_TRANS: Currency read FVL_CRED_TRANS write FVL_CRED_TRANS;
    property VL_CRED_OUT: Currency read FVL_CRED_OUT write FVL_CRED_OUT;
    property SLD_CRED_FIM: Currency read FSLD_CRED_FIM write FSLD_CRED_FIM;

    property Registro1501: TRegistro1501List read FRegistro1501 write FRegistro1501;
  end;

  // Registro 1500 - Lista
  TRegistro1500List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1500;
    procedure SetItem(Index: Integer; const Value: TRegistro1500);
  public
    function New: TRegistro1500;
    property Items[Index: Integer]: TRegistro1500 read GetItem write SetItem;
  end;

  //REGISTRO 1501: APURAÇÃO DE CRÉDITO EXTEMPORÂNEO - DOCUMENTOS E OPERAÇÕES DE PERÍODOS ANTERIORES – COFINS
  TRegistro1501 = class
  private
    fCOD_PART: string;	            //02	COD_PART	Código do participante (Campo 02 do Registro 0150)	C	060	-
    fCOD_ITEM: string;	            //03	COD_ITEM	Código do item (campo 02 do Registro 0200)	C	060	-
    fCOD_MOD: string;	              //04	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1.	C	002*	-
    fSER:string;	                  //05	SER	Série do documento fiscal	C	004	-
    fSUB_SER: string;	              //06	SUB_SER	Subsérie do documento fiscal	C	003	-
    fNUM_DOC: Integer;	            //07	NUM_DOC	Número do documento fiscal	N	009	-
    fDT_OPER: TDateTime;	          //08	DT_OPER	Data da Operação (ddmmaaaa)	N	008*	-
    fCHV_NFE: String;	            //09	CHV_NFE	Chave da Nota Fiscal Eletrônica	N	044*	-
    fVL_OPER: Currency;	            //10	VL_OPER	Valor da Operação	N	-	02
    fCFOP: Integer;	                //11	CFOP	Código fiscal de operação e prestação	N	004*	-
    fNAT_BC_CRED: string;	          //12	NAT_BC_CRED	Código da Base de Cálculo do Crédito, conforme a Tabela indicada no item 4.3.7.	C	002*	-
    fIND_ORIG_CRED: Integer;        //13	IND_ORIG_CRED	Indicador da origem do crédito:0 – Operação no Mercado Interno;1 – Operação de Importação	C	001*	-
    fCST_COFINS: Integer;	          //14	CST_COFINS	Código da Situação Tributária referente ao COFINS, conforme a Tabela indicada no item 4.3.4.	N	002*	-
    fVL_BC_COFINS: Currency;        //15	VL_BC_COFINS	Base de Cálculo do Crédito de COFINS (em valor ou em quantidade)	N	-	03
    fALIQ_COFINS: Currency;	        //16	ALIQ_COFINS	Alíquota do COFINS (em percentual ou em reais)	N	-	04
    fVL_COFINS: Currency;	          //17	VL_COFINS	Valor do Crédito de COFINS	N	-	02
    fCOD_CTA: string;	              //18	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
    fCOD_CCUS: string;	            //19	COD_CCUS	Código do Centro de Custos	C	060	-
    fDESC_COMPL: string;	          //20	DESC_COMPL	Descrição complementar do Documento/Operação	C	-	-
    fPER_ESCRIT: Integer;	          //21	PER_ESCRIT	Mês/Ano da Escrituração em que foi registrado o documento/operação (Crédito pelo método da Apropriação Direta).	N	006*	-
    fCNPJ: string;	                //22	CNPJ	CNPJ do estabelecimento gerador do crédito extemporâneo (Campo 04  do Registro 0140)	N	014*	-

    FRegistro1502: TRegistro1502; // NIVEL 4
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_PART: string read FCOD_PART write FCOD_PART;
    property COD_ITEM: string read FCOD_ITEM write FCOD_ITEM;
    property COD_MOD: string read FCOD_MOD write FCOD_MOD;
    property SER:string read FSER write FSER;
    property SUB_SER: string read FSUB_SER write FSUB_SER;
    property NUM_DOC: Integer read FNUM_DOC write FNUM_DOC;
    property DT_OPER: TDateTime read FDT_OPER write FDT_OPER;
    property CHV_NFE: String read FCHV_NFE write FCHV_NFE;
    property VL_OPER: Currency read FVL_OPER write FVL_OPER;
    property CFOP: Integer read FCFOP write FCFOP;
    property NAT_BC_CRED: string read FNAT_BC_CRED write FNAT_BC_CRED;
    property IND_ORIG_CRED: Integer read FIND_ORIG_CRED write FIND_ORIG_CRED;
    property CST_COFINS: Integer read FCST_COFINS write FCST_COFINS;
    property VL_BC_COFINS: Currency read FVL_BC_COFINS write FVL_BC_COFINS;
    property ALIQ_COFINS: Currency read FALIQ_COFINS write FALIQ_COFINS;
    property VL_COFINS: Currency read FVL_COFINS write FVL_COFINS;
    property COD_CTA: string read FCOD_CTA write FCOD_CTA;
    property COD_CCUS: string read FCOD_CCUS write FCOD_CCUS;
    property DESC_COMPL: string read FDESC_COMPL write FDESC_COMPL;
    property PER_ESCRIT: Integer read FPER_ESCRIT write FPER_ESCRIT;
    property CNPJ: string read FCNPJ write FCNPJ;

    property Registro1502: TRegistro1502 read FRegistro1502 write FRegistro1502;
  end;

  // Registro 1501 - Lista
  TRegistro1501List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1501;
    procedure SetItem(Index: Integer; const Value: TRegistro1501);
  public
    function New: TRegistro1501;
    property Items[Index: Integer]: TRegistro1501 read GetItem write SetItem;
  end;

  //REGISTRO 1502: DETALHAMENTO DO CRÉDITO EXTEMPORÂNEO VINCULADO A MAIS DE UM TIPO DE RECEITA – COFINS
  TRegistro1502 = class
  private
    fVL_CRED_COFINS_TRIB_MI: Variant;      //02	VL_CRED_COFINS_TRIB_MI	Parcela do Crédito de COFINS, vinculada a Receita Tributada no Mercado Interno	N	-	02
    fVL_CRED_COFINS_NT_MI: Variant;	      //03	VL_CRED_COFINS_NT_MI	Parcela do Crédito de COFINS, vinculada a Receita Não Tributada no Mercado Interno	N	-	02
    fVL_CRED_COFINS_EXP: Variant;	        //03? repetiu o numero do campo no manual	VL_CRED_COFINS_ EXP	Parcela do Crédito de COFINS, vinculada a Receita de Exportação	N	-	02
  public
    property VL_CRED_COFINS_TRIB_MI: Variant read FVL_CRED_COFINS_TRIB_MI write FVL_CRED_COFINS_TRIB_MI;
    property VL_CRED_COFINS_NT_MI: Variant read FVL_CRED_COFINS_NT_MI write FVL_CRED_COFINS_NT_MI;
    property VL_CRED_COFINS_EXP: Variant read FVL_CRED_COFINS_EXP write FVL_CRED_COFINS_EXP;
  end;

  //REGISTRO 1600: CONTRIBUIÇÃO SOCIAL EXTEMPORÂNEA – COFINS
  TRegistro1600 = class
  private
    fPER_APUR_ANT: Integer;	                //02	PER_APUR_ANT	Período de Apuração da Contribuição Social Extemporânea (MMAAAA)	N	006*	-
    fNAT_CONT_REC: string;	                //03	NAT_CONT_REC	Natureza da Contribuição a Recolher, conforme Tabela 4.3.5.	C	002	-
    fVL_CONT_APUR: Currency;	              //04	VL_CONT_APUR	Valor da Contribuição Apurada	N	-	02
    fVL_CRED_COFINS_DESC: Currency;         //05	VL_CRED_COFINS_DESC	Valor do Crédito de COFINS a Descontar, da Contribuição Social Extemporânea.	N	-	02
    fVL_CONT_DEV: Currency;	                //06	VL_CONT_DEV	Valor da Contribuição Social Extemporânea Devida.	N	-	02
    fVL_OUT_DED: Currency;	                //07	VL_OUT_DED	Valor de Outras Deduções.	N	-	02
    fVL_CONT_EXT: Currency;	                //08	VL_CONT_EXT	Valor da Contribuição Social Extemporânea a pagar.	N	-	02
    fVL_MUL: Currency;	                    //09	VL_MUL	Valor da Multa.	N	-	02
    fVL_JUR: Currency;	                    //10	VL_JUR	Valor dos Juros.	N	-	02
    fDT_RECOL: TDateTime;	                  //11	DT_RECOL	Data do Recolhimento.	N	008*	-

    FRegistro1610: TRegistro1610List; // NIVEL 3
    FRegistro1620: TRegistro1620List; // NIVEL 3
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property PER_APUR_ANT: Integer read FPER_APUR_ANT write FPER_APUR_ANT;
    property NAT_CONT_REC: string read FNAT_CONT_REC write FNAT_CONT_REC;
    property VL_CONT_APUR: Currency read FVL_CONT_APUR write FVL_CONT_APUR;
    property VL_CRED_COFINS_DESC: Currency read FVL_CRED_COFINS_DESC write FVL_CRED_COFINS_DESC;
    property VL_CONT_DEV: Currency read FVL_CONT_DEV write FVL_CONT_DEV;
    property VL_OUT_DED: Currency read FVL_OUT_DED write FVL_OUT_DED;
    property VL_CONT_EXT: Currency read FVL_CONT_EXT write FVL_CONT_EXT;
    property VL_MUL: Currency read FVL_MUL write FVL_MUL;
    property VL_JUR: Currency read FVL_JUR write FVL_JUR;
    property DT_RECOL: TDateTime read FDT_RECOL write FDT_RECOL;

    property Registro1610: TRegistro1610List read FRegistro1610 write FRegistro1610;
    property Registro1620: TRegistro1620List read FRegistro1620 write FRegistro1620;
  end;

  // Registro 1600 - Lista
  TRegistro1600List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1600;
    procedure SetItem(Index: Integer; const Value: TRegistro1600);
  public
    function New: TRegistro1600;
    property Items[Index: Integer]: TRegistro1600 read GetItem write SetItem;
  end;

  //REGISTRO 1610: DETALHAMENTO DA CONTRIBUIÇÃO SOCIAL EXTEMPORÂNEA – COFINS
  TRegistro1610 = class
  private
    fCNPJ: string;	                //02	CNPJ	Número de inscrição do estabelecimento no CNPJ (Campo 04 do Registro 0140).	N	014*	-
    fCST_COFINS: TACBrCstCofins;	    //03	CST_COFINS	Código da Situação Tributária referente a COFINS, conforme a Tabela indicada no item 4.3.4.	N	002*	-
    fCOD_PART: string;	             //04	COD_PART	Código do participante (Campo 02 do Registro 0150)	C	060	-
    fDT_OPER: TDateTime;	          //05	DT_OPER	Data da Operação (ddmmaaaa)	N	008*	-
    fVL_OPER: Currency;	             //06	VL_OPER	Valor da Operação	N	-	02
    fVL_BC_COFINS: Currency;         //07	VL_BC_COFINS	Base de cálculo da COFINS (em valor ou em quantidade)	N	-	03
    fALIQ_COFINS: Currency;	       //08	ALIQ_COFINS	Alíquota da COFINS (em percentual ou em reais)	N	-	04
    fVL_COFINS: Currency;	          //09	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA: string;	             //10	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
    fDESC_COMPL: string;	          //11	DESC_COMPL	Descrição complementar do Documento/Operação	C	-	-
  public
    property CNPJ: string read FCNPJ write FCNPJ;
    property CST_COFINS: TACBrCstCofins read FCST_COFINS write FCST_COFINS;
    property COD_PART: string read FCOD_PART write FCOD_PART;
    property DT_OPER: TDateTime read FDT_OPER write FDT_OPER;
    property VL_OPER: Currency read FVL_OPER write FVL_OPER;
    property VL_BC_COFINS: Currency read FVL_BC_COFINS write FVL_BC_COFINS;
    property ALIQ_COFINS: Currency read FALIQ_COFINS write FALIQ_COFINS;
    property VL_COFINS: Currency read FVL_COFINS write FVL_COFINS;
    property COD_CTA: string read FCOD_CTA write FCOD_CTA;
    property DESC_COMPL: string read FDESC_COMPL write FDESC_COMPL;
  end;

  // Registro 1610 - Lista
  TRegistro1610List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1610;
    procedure SetItem(Index: Integer; const Value: TRegistro1610);
  public
    function New: TRegistro1610;
    property Items[Index: Integer]: TRegistro1610 read GetItem write SetItem;
  end;

  //REGISTRO 1620: DEMONSTRAÇÃO DO CRÉDITO A DESCONTAR DA CONTRIBUIÇÃO EXTEMPORÂNEA – COFINS
  TRegistro1620 = class
  private
    fPER_APU_CRED: Integer;         //02	PER_APU_CRED	Período de Apuração do Crédito (MM/AAAA)	N	006	-
    fORIG_CRED: Integer;	          //03	ORIG_CRED	Indicador da origem do crédito:01 – Crédito decorrente de operações próprias;02 – Crédito transferido por pessoa jurídica sucedida.	N	002*	-
    fCOD_CRED: Integer;	            //04	COD_CRED	Código do Tipo do Crédito, conforme Tabela 4.3.6.	N	003*	-
    fVL_CRED: Currency;	            //05	VL_CRED	Valor do Crédito a Descontar	N	-	002
  public
    property PER_APU_CRED: Integer read FPER_APU_CRED write FPER_APU_CRED;
    property ORIG_CRED: Integer read FORIG_CRED write FORIG_CRED;
    property COD_CRED: Integer read FCOD_CRED write FCOD_CRED;
    property VL_CRED: Currency read FVL_CRED write FVL_CRED;
  end;

  // Registro 1620 - Lista
  TRegistro1620List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1620;
    procedure SetItem(Index: Integer; const Value: TRegistro1620);
  public
    function New: TRegistro1620;
    property Items[Index: Integer]: TRegistro1620 read GetItem write SetItem;
  end;

  //REGISTRO 1700: CONTROLE DOS VALORES RETIDOS NA FONTE – COFINS
  TRegistro1700 = class
  private
    fIND_NAT_RET: Integer;	         //02	IND_NAT_RET	Indicador de Natureza da Retenção na Fonte:01 - Retenção por Órgãos, Autarquias e Fundações Federais;02 - Retenção por outras Entidades da Administração Pública Federal;03 - Retenção por Pessoas Jurídicas de Direito Privado;04 - Recolhimento por Sociedade Cooperativa;05 - Retenção por Fabricante de Máquinas e Veículos;99 - Outras Retenções	N	002*	-
    fPR_REC_RET: Integer;	           //03	PR_REC_RET	Período do Recebimento e da Retenção (MM/AAAA)	N	006*	-
    fVL_RET_APU: Currency;	         //04	VL_RET_APU	Valor Total da Retenção	N	-	02
    fVL_RET_DED: Currency;		       //05	VL_RET_DED	Valor da Retenção deduzida da Contribuição Devida	N	-	02
    fVL_RET_PER: Currency;		       //06	VL_RET_PER	Valor da Retenção utilizada mediante Pedido de Restituição.	N	-	02
    fVL_RET_DCOMP: Currency;	       //07	VL_RET_DCOMP	Valor da Retenção utilizada mediante Declaração de Compensação.	N	-	02
    fSLD_RET: Currency;		           //08	SLD_RET	Saldo de Retenção a utilizar em períodos de apuração futuros (04 - 05 - 06 - 07).	N	-	02
  public
    property IND_NAT_RET: Integer read FIND_NAT_RET write FIND_NAT_RET;
    property PR_REC_RET: Integer read FPR_REC_RET write FPR_REC_RET;
    property VL_RET_APU: Currency read FVL_RET_APU write FVL_RET_APU;
    property VL_RET_DED: Currency read FVL_RET_DED write FVL_RET_DED;
    property VL_RET_PER: Currency read FVL_RET_PER write FVL_RET_PER;
    property VL_RET_DCOMP: Currency read FVL_RET_DCOMP write FVL_RET_DCOMP;
    property SLD_RET: Currency read FSLD_RET write FSLD_RET;
  end;

  // Registro 1700 - Lista
  TRegistro1700List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1700;
    procedure SetItem(Index: Integer; const Value: TRegistro1700);
  public
    function New: TRegistro1700;
    property Items[Index: Integer]: TRegistro1700 read GetItem write SetItem;
  end;

  //REGISTRO 1800: INCORPORAÇÃO IMOBILIÁRIA - RET
  TRegistro1800 = class
  private
    fINC_IMOB: string;	            //02	INC_IMOB	Empreendimento objeto de Incorporação Imobiliária, optante pelo RET.	C	090	-
    fREC_RECEB_RET: Currency;       //03	REC_RECEB_RET	Receitas recebidas pela incorporadora na venda das unidades imobiliárias que compõem a incorporação.	N	-	02
    fREC_FIN_RET: Currency;	        //04	REC_FIN_RET	Receitas Financeiras e Variações Monetárias decorrentes das vendas submetidas ao RET.	N	-	02
    fBC_RET: Currency;	            //05	BC_RET	Base de Cálculo do Recolhimento Unificado	N	-	02
    fALIQ_RET: Currency;	          //06	ALIQ_RET	Alíquota do Recolhimento Unificado.	N	006	02
    fVL_REC_UNI: Currency;	        //07	VL_REC_UNI	Valor do Recolhimento Unificado.	N	-	02
    fDT_REC_UNI: TDateTime;	        //08	DT_REC_UNI	Data do recolhimento unificado	N	008*	-
    fCOD_REC: string;	              //09	COD_REC	Código da Receita	C	004	-

    FRegistro1809: TRegistro1809List; // NIVEL 3
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property INC_IMOB: string read FINC_IMOB write FINC_IMOB;
    property REC_RECEB_RET: Currency read FREC_RECEB_RET write FREC_RECEB_RET;
    property REC_FIN_RET: Currency read FREC_FIN_RET write FREC_FIN_RET;
    property BC_RET: Currency read FBC_RET write FBC_RET;
    property ALIQ_RET: Currency read FALIQ_RET write FALIQ_RET;
    property VL_REC_UNI: Currency read FVL_REC_UNI write FVL_REC_UNI;
    property DT_REC_UNI: TDateTime read FDT_REC_UNI write FDT_REC_UNI;
    property COD_REC: string read FCOD_REC write FCOD_REC;

    property Registro1809: TRegistro1809List read FRegistro1809 write FRegistro1809;
  end;

  // Registro 1800 - Lista
  TRegistro1800List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1800;
    procedure SetItem(Index: Integer; const Value: TRegistro1800);
  public
    function New: TRegistro1800;
    property Items[Index: Integer]: TRegistro1800 read GetItem write SetItem;
  end;

  //REGISTRO 1809: PROCESSO REFERENCIADO
  TRegistro1809 = class
  private
    fNUM_PROC: string;         //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC: TACBrOrigemProcesso;        //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil;9 – Outros.	C	001*	-
  public
    property NUM_PROC: string read FNUM_PROC write FNUM_PROC;
    property IND_PROC: TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro 1809 - Lista
  TRegistro1809List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1809;
    procedure SetItem(Index: Integer; const Value: TRegistro1809);
  public
    function New: TRegistro1809;
    property Items[Index: Integer]: TRegistro1809 read GetItem write SetItem;
  end;


  (*Por: Edilson Alves de Oliveira
    REGISTRO 1900 - CONSOLIDAÇÃO DOS DOCUMENTOS EMITIDOS NO PERÍODO POR 
                    PESSOA  JURÍDICA  SUBMETIDA  AO  REGIME  DE  TRIBUTAÇÃO
                    COM  BASE  NO LUCRO PRESUMIDO – REGIME DE CAIXA OU DE COMPETÊNCIA*)

  TRegistro1900 = class
  private
    FCNPJ       : string;
    FCOD_MOD    : string;
    FSER        : string;
    FSUB_SER    : string;
    FCOD_SIT    : TACBrCodSitF;
    FVL_TOT_REC : Currency;
    FQUANT_DOC  : Integer;
    FCST_PIS    : TACBrCstPis;
    FCST_COFINS : TACBrCstCofins;
    FCFOP       : Integer;
    FINF_COMPL  : string;
    FCOD_CTA    : string;

  public

    property CNPJ      : string          read FCNPJ        write FCNPJ       ;
    property COD_MOD   : string          read FCOD_MOD     write FCOD_MOD    ;
    property SER       : string          read FSER         write FSER        ;
    property SUB_SER   : string          read FSUB_SER     write FSUB_SER    ;
    property COD_SIT   : TACBrCodSitF    read FCOD_SIT     write FCOD_SIT    ;
    property VL_TOT_REC: Currency        read FVL_TOT_REC  write FVL_TOT_REC ;
    property QUANT_DOC : Integer         read FQUANT_DOC   write FQUANT_DOC  ;
    property CST_PIS   : TACBrCstPis     read FCST_PIS     write FCST_PIS    ;
    property CST_COFINS: TACBrCstCofins  read FCST_COFINS  write FCST_COFINS ;
    property CFOP      : Integer         read FCFOP        write FCFOP       ;
    property INF_COMPL : string          read FINF_COMPL   write FINF_COMPL  ;
    property COD_CTA   : string          read FCOD_CTA     write FCOD_CTA    ;

  end;

  // Registro 1900 - Lista
  TRegistro1900List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro1900;
    procedure SetItem(Index: Integer; const Value: TRegistro1900);
  public
    function New: TRegistro1900;
    property Items[Index: Integer]: TRegistro1900 read GetItem write SetItem;
  end;

  //REGISTRO 1990: ENCERRAMENTO DO BLOCO 1
  TRegistro1990 = class
  private
    fQTD_LIN_1: Integer;      //02	QTD_LIN_1	Quantidade total de linhas do Bloco 1	N	-	-
  public
    property QTD_LIN_1: Integer read FQTD_LIN_1 write FQTD_LIN_1;
  end;

implementation

{TRegistro1001}

constructor TRegistro1001.Create;
begin
  inherited Create;
  FRegistro1010 := TRegistro1010List.Create;
  FRegistro1020 := TRegistro1020List.Create;
  FRegistro1050 := TRegistro1050List.Create;
  FRegistro1100 := TRegistro1100List.Create;
  FRegistro1200 := TRegistro1200List.Create;
  FRegistro1300 := TRegistro1300List.Create;
  FRegistro1500 := TRegistro1500List.Create;
  FRegistro1600 := TRegistro1600List.Create;
  FRegistro1700 := TRegistro1700List.Create;
  FRegistro1800 := TRegistro1800List.Create;
  FRegistro1900 := TRegistro1900List.Create;
end;

destructor TRegistro1001.Destroy;
begin
  FRegistro1010.Free;
  FRegistro1020.Free;
  FRegistro1050.Free;
  FRegistro1100.Free;
  FRegistro1200.Free;
  FRegistro1300.Free;
  FRegistro1500.Free;
  FRegistro1600.Free;
  FRegistro1700.Free;
  FRegistro1800.Free;
  FRegistro1900.Free;
  inherited;
end;

{TRegistro1010}

function TRegistro1010List.GetItem(Index: Integer): TRegistro1010;
begin
  Result := TRegistro1010(Inherited Items[Index]);
end;

function TRegistro1010List.New: TRegistro1010;
begin
  Result := TRegistro1010.Create;
  Add(Result);
end;

procedure TRegistro1010List.SetItem(Index: Integer; const Value: TRegistro1010);
begin
  Put(Index, Value);
end;

{TRegistro1020}

function TRegistro1020List.GetItem(Index: Integer): TRegistro1020;
begin
  Result := TRegistro1020(Inherited Items[Index]);
end;

function TRegistro1020List.New: TRegistro1020;
begin
  Result := TRegistro1020.Create;
  Add(Result);
end;

procedure TRegistro1020List.SetItem(Index: Integer; const Value: TRegistro1020);
begin
  Put(Index, Value);
end;



{TRegistro1050}

function TRegistro1050List.GetItem(Index: Integer): TRegistro1050;
begin
  Result := TRegistro1050(Inherited Items[Index]);
end;

function TRegistro1050List.New: TRegistro1050;
begin
  Result := TRegistro1050.Create;
  Add(Result);
end;

procedure TRegistro1050List.SetItem(Index: Integer; const Value: TRegistro1050);
begin
  Put(Index, Value);
end;


{TRegistro1100}

function TRegistro1100List.GetItem(Index: Integer): TRegistro1100;
begin
  Result := TRegistro1100(Inherited Items[Index]);
end;

function TRegistro1100List.New: TRegistro1100;
begin
  Result := TRegistro1100.Create;
  Add(Result);
end;

procedure TRegistro1100List.SetItem(Index: Integer; const Value: TRegistro1100);
begin
  Put(Index, Value);
end;

{ TRegistro1100 }

constructor TRegistro1100.Create;
begin
  FRegistro1101 := TRegistro1101List.Create;
end;

destructor TRegistro1100.Destroy;
begin
  FRegistro1101.Free;
  inherited;
end;

{TRegistro1101}

function TRegistro1101List.GetItem(Index: Integer): TRegistro1101;
begin
  Result := TRegistro1101(Inherited Items[Index]);
end;

function TRegistro1101List.New: TRegistro1101;
begin
  Result := TRegistro1101.Create;
  Add(Result);
end;

procedure TRegistro1101List.SetItem(Index: Integer; const Value: TRegistro1101);
begin
  Put(Index, Value);
end;

{ TRegistro1101 }

constructor TRegistro1101.Create;
begin
  FRegistro1102 := TRegistro1102.Create;
end;

destructor TRegistro1101.Destroy;
begin
  FRegistro1102.Free;
  inherited;
end;

{TRegistro1200}

function TRegistro1200List.GetItem(Index: Integer): TRegistro1200;
begin
  Result := TRegistro1200(Inherited Items[Index]);
end;

function TRegistro1200List.New: TRegistro1200;
begin
  Result := TRegistro1200.Create;
  Add(Result);
end;

procedure TRegistro1200List.SetItem(Index: Integer; const Value: TRegistro1200);
begin
  Put(Index, Value);
end;

{ TRegistro1200 }

constructor TRegistro1200.Create;
begin
  FRegistro1210 := TRegistro1210List.Create;
  FRegistro1220 := TRegistro1220List.Create;
end;

destructor TRegistro1200.Destroy;
begin
  FRegistro1210.Free;
  FRegistro1220.Free;
  inherited;
end;

{TRegistro1210}

function TRegistro1210List.GetItem(Index: Integer): TRegistro1210;
begin
  Result := TRegistro1210(Inherited Items[Index]);
end;

function TRegistro1210List.New: TRegistro1210;
begin
  Result := TRegistro1210.Create;
  Add(Result);
end;

procedure TRegistro1210List.SetItem(Index: Integer; const Value: TRegistro1210);
begin
  Put(Index, Value);
end;

{TRegistro1220}

function TRegistro1220List.GetItem(Index: Integer): TRegistro1220;
begin
  Result := TRegistro1220(Inherited Items[Index]);
end;

function TRegistro1220List.New: TRegistro1220;
begin
  Result := TRegistro1220.Create;
  Add(Result);
end;

procedure TRegistro1220List.SetItem(Index: Integer; const Value: TRegistro1220);
begin
  Put(Index, Value);
end;

{TRegistro1300}

function TRegistro1300List.GetItem(Index: Integer): TRegistro1300;
begin
  Result := TRegistro1300(Inherited Items[Index]);
end;

function TRegistro1300List.New: TRegistro1300;
begin
  Result := TRegistro1300.Create;
  Add(Result);
end;

procedure TRegistro1300List.SetItem(Index: Integer; const Value: TRegistro1300);
begin
  Put(Index, Value);
end;

{TRegistro1500}

function TRegistro1500List.GetItem(Index: Integer): TRegistro1500;
begin
  Result := TRegistro1500(Inherited Items[Index]);
end;

function TRegistro1500List.New: TRegistro1500;
begin
  Result := TRegistro1500.Create;
  Add(Result);
end;

procedure TRegistro1500List.SetItem(Index: Integer; const Value: TRegistro1500);
begin
  Put(Index, Value);
end;

{ TRegistro1500 }

constructor TRegistro1500.Create;
begin
  FRegistro1501 := TRegistro1501List.Create;
end;

destructor TRegistro1500.Destroy;
begin
  FRegistro1501.Free;
  inherited;
end;

{TRegistro1501}

function TRegistro1501List.GetItem(Index: Integer): TRegistro1501;
begin
  Result := TRegistro1501(Inherited Items[Index]);
end;

function TRegistro1501List.New: TRegistro1501;
begin
  Result := TRegistro1501.Create;
  Add(Result);
end;

procedure TRegistro1501List.SetItem(Index: Integer; const Value: TRegistro1501);
begin
  Put(Index, Value);
end;

{ TRegistro1501 }

constructor TRegistro1501.Create;
begin
  FRegistro1502 := TRegistro1502.Create;
end;

destructor TRegistro1501.Destroy;
begin
  FRegistro1502.Free;
  inherited;
end;

{TRegistro1600}

function TRegistro1600List.GetItem(Index: Integer): TRegistro1600;
begin
  Result := TRegistro1600(Inherited Items[Index]);
end;

function TRegistro1600List.New: TRegistro1600;
begin
  Result := TRegistro1600.Create;
  Add(Result);
end;

procedure TRegistro1600List.SetItem(Index: Integer; const Value: TRegistro1600);
begin
  Put(Index, Value);
end;

{ TRegistro1600 }

constructor TRegistro1600.Create;
begin
  FRegistro1610 := TRegistro1610List.Create;
  FRegistro1620 := TRegistro1620List.Create;
end;

destructor TRegistro1600.Destroy;
begin
  FRegistro1610.Free;
  FRegistro1620.Free;
  inherited;
end;

{TRegistro1610}

function TRegistro1610List.GetItem(Index: Integer): TRegistro1610;
begin
  Result := TRegistro1610(Inherited Items[Index]);
end;

function TRegistro1610List.New: TRegistro1610;
begin
  Result := TRegistro1610.Create;
  Add(Result);
end;

procedure TRegistro1610List.SetItem(Index: Integer; const Value: TRegistro1610);
begin
  Put(Index, Value);
end;

{TRegistro1620}

function TRegistro1620List.GetItem(Index: Integer): TRegistro1620;
begin
  Result := TRegistro1620(Inherited Items[Index]);
end;

function TRegistro1620List.New: TRegistro1620;
begin
  Result := TRegistro1620.Create;
  Add(Result);
end;

procedure TRegistro1620List.SetItem(Index: Integer; const Value: TRegistro1620);
begin
  Put(Index, Value);
end;

{TRegistro1700}

function TRegistro1700List.GetItem(Index: Integer): TRegistro1700;
begin
  Result := TRegistro1700(Inherited Items[Index]);
end;

function TRegistro1700List.New: TRegistro1700;
begin
  Result := TRegistro1700.Create;
  Add(Result);
end;

procedure TRegistro1700List.SetItem(Index: Integer; const Value: TRegistro1700);
begin
  Put(Index, Value);
end;

{TRegistro1800}

function TRegistro1800List.GetItem(Index: Integer): TRegistro1800;
begin
  Result := TRegistro1800(Inherited Items[Index]);
end;

function TRegistro1800List.New: TRegistro1800;
begin
  Result := TRegistro1800.Create;
  Add(Result);
end;

procedure TRegistro1800List.SetItem(Index: Integer; const Value: TRegistro1800);
begin
  Put(Index, Value);
end;

{ TRegistro1800 }

constructor TRegistro1800.Create;
begin
  FRegistro1809 := TRegistro1809List.Create;
end;

destructor TRegistro1800.Destroy;
begin
  FRegistro1809.Free;
  inherited;
end;

{TRegistro1809}

function TRegistro1809List.GetItem(Index: Integer): TRegistro1809;
begin
  Result := TRegistro1809(Inherited Items[Index]);
end;

function TRegistro1809List.New: TRegistro1809;
begin
  Result := TRegistro1809.Create;
  Add(Result);
end;

procedure TRegistro1809List.SetItem(Index: Integer; const Value: TRegistro1809);
begin
  Put(Index, Value);
end;

{ TRegistro1900List }

function TRegistro1900List.GetItem(Index: Integer): TRegistro1900;
begin
  Result := TRegistro1900(Inherited Items[Index]);
end;

function TRegistro1900List.New: TRegistro1900;
begin
  Result := TRegistro1900.Create;
  Add(Result);
end;

procedure TRegistro1900List.SetItem(Index: Integer;
  const Value: TRegistro1900);
begin
  Put(Index, Value);
end;

end.
