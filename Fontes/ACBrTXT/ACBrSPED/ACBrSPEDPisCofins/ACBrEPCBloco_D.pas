{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro e Claudio Roberto de Souza      }
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

unit ACBrEPCBloco_D;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEPCBlocos;

type
   TRegistroD010List = class;
   TRegistroD100List = class;
   TRegistroD101List = class;
   TRegistroD105List = class;
   TRegistroD111List = class;
   TRegistroD200List = class;
   TRegistroD201List = class;
   TRegistroD205List = class;
   TRegistroD209List = class;
   TRegistroD300List = class;
   TRegistroD309List = class;
   TRegistroD350List = class;
   TRegistroD359List = class;
   TRegistroD500List = class;
   TRegistroD501List = class;
   TRegistroD505List = class;
   TRegistroD509List = class;
   TRegistroD600List = class;
   TRegistroD601List = class;
   TRegistroD605List = class;
   TRegistroD609List = class;

  //REGISTRO D001: ABERTURA DO BLOCO D
  TRegistroD001 = class(TOpenBlocos)
  private
    FRegistroD010: TRegistroD010List;
  public
    constructor Create;  virtual; /// Create
    destructor  Destroy; override; /// Destroy

    property RegistroD010: TRegistroD010List read FRegistroD010 write FRegistroD010;
  end;

  //REGISTRO D010: IDENTIFICAÇÃO DO ESTABELECIMENTO
  TRegistroD010 = class
  private
    fCNPJ: string;        //02	CNPJ	Número de inscrição do estabelecimento no CNPJ.	N	014*	-

    FRegistroD100: TRegistroD100List;
    FRegistroD200: TRegistroD200List;
    FRegistroD300: TRegistroD300List;
    FRegistroD350: TRegistroD350List;
    FRegistroD500: TRegistroD500List;
    FRegistroD600: TRegistroD600List;
  public
    constructor Create; virtual;   // Create
    destructor  Destroy; override; // Destroy

    property CNPJ         : string            read FCNPJ         write FCNPJ;

    property RegistroD100 : TRegistroD100List read FRegistroD100 write FRegistroD100;
    property RegistroD200 : TRegistroD200List read FRegistroD200 write FRegistroD200;
    property RegistroD300 : TRegistroD300List read FRegistroD300 write FRegistroD300;
    property RegistroD350 : TRegistroD350List read FRegistroD350 write FRegistroD350;
    property RegistroD500 : TRegistroD500List read FRegistroD500 write FRegistroD500;
    property RegistroD600 : TRegistroD600List read FRegistroD600 write FRegistroD600;
  end;

  // Registro D010 - Lista
  TRegistroD010List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD010;
    procedure SetItem(Index: Integer; const Value: TRegistroD010);
  public
    function New: TRegistroD010;
    property Items[Index: Integer]: TRegistroD010 read GetItem write SetItem;
  end;

  //REGISTRO D100: AQUISIÇÃO DE SERVIÇOS DE TRANSPORTE - NOTA FISCAL DE SERVIÇO DE TRANSPORTE (CÓDIGO 07) E CONHECIMENTOS DE TRANSPORTE RODOVIÁRIO DE CARGAS (CÓDIGO 08), CONHECIMENTO DE TRANSPORTE DE CARGAS AVULSO (CÓDIGO 8B), AQUAVIÁRIO DE CARGAS (CÓDIGO 09), AÉREO (CÓDIGO 10), FERROVIÁRIO DE CARGAS (CÓDIGO 11), MULTIMODAL DE CARGAS (CÓDIGO 26), NOTA FISCAL DE TRANSPORTE FERROVIÁRIO DE CARGA (CÓDIGO 27) E CONHECIMENTO DE TRANSPORTE ELETRÔNICO – CT-e (CÓDIGO 57)
  TRegistroD100 = class
  private
    fIND_OPER     : string;                            //02	IND_OPER	Indicador do tipo de operação:0- Aquisição;	C	001*	-
    fIND_EMIT     : TACBrIndicadorEmitenteDF;          //03	IND_EMIT	Indicador do emitente do documento fiscal:0- Emissão Própria;1- Emissão por Terceiros	C	001*	-
    fCOD_PART     : string;                            //04	COD_PART	Código do participante (campo 02 do Registro 0150).	C	060	-
    fCOD_MOD      : string;                            //05	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1 	C	002*	-
    fCOD_SIT      : TACBrSituacaoDF;                   //06	COD_SIT	Código da situação do documento fiscal, conforme a Tabela 4.1.2	N	002*	-
    fSER          : string;                            //07	SER	Série do documento fiscal	C	004	-
    fSUB          : string;                            //08	SUB	Subsérie do documento fiscal	C	003	-
    fNUM_DOC      : string;                            //09	NUM_DOC	Número do documento fiscal	N	009	-
    fCHV_CTE      : string;                            //10	CHV_CTE	Chave do Conhecimento de Transporte Eletrônico	N	044*	-
    fDT_DOC       : TDateTime;                         //11	DT_DOC	Data de referência/emissão dos documentos fiscais	N	008*	-
    fDT_A_P       : TDateTime;                         //12	DT_A_P	Data da aquisição ou da prestação do serviço	N	008*	-
    fTP_CT_e      : string;                            //13	TP_CT-e	Tipo de Conhecimento de Transporte Eletrônico conforme definido no Manual de Integração do CT-e	N	001*	-
    fCHV_CTE_REF  : string;                            //14	CHV_CTE_REF	Chave do CT-e de referência cujos valores foram complementados (opção “1” do campo anterior) ou cujo débito foi anulado (opção “2” do campo anterior). 	N	044*	-
    fVL_DOC       : Currency;                          //15	VL_DOC	Valor total do documento fiscal	N	-	02
    fVL_DESC      : Currency;                          //16	VL_DESC	Valor total do desconto	N	-	02
    fIND_FRT      : TACBrTipoFrete;                    //17	IND_FRT	Indicador do tipo do frete:0- Por conta de terceiros;1- Por conta do emitente;2- Por conta do destinatário;9- Sem cobrança de frete.	C	001*	-
    fVL_SERV      : Currency;                          //18	VL_SERV	Valor total da prestação de serviço	N	-	02
    fVL_BC_ICMS   : Currency;                          //19	VL_BC_ICMS	Valor da base de cálculo do ICMS	N	-	02
    fVL_ICMS      : Currency;                          //20	VL_ICMS	Valor do ICMS	N	-	02
    fVL_NT        : Currency;                          //21	VL_NT	Valor não-tributado do ICMS	N	-	02
    fCOD_INF      : string;                            //22	COD_INF	Código da informação complementar do documento fiscal (campo 02 do Registro 0450)	C	006	-
    fCOD_CTA      : string;                            //23	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-

    FRegistroD101 : TRegistroD101List;
    FRegistroD105 : TRegistroD105List;
    FRegistroD111 : TRegistroD111List;
  public
    constructor Create;  virtual;  // Create
    destructor  Destroy; override; // Destroy

    property IND_OPER     : string                   read FIND_OPER     write FIND_OPER;
    property IND_EMIT     : TACBrIndicadorEmitenteDF read FIND_EMIT     write FIND_EMIT;
    property COD_PART     : string                   read FCOD_PART     write FCOD_PART;
    property COD_MOD      : string                   read FCOD_MOD      write FCOD_MOD;
    property COD_SIT      : TACBrSituacaoDF          read FCOD_SIT      write FCOD_SIT;
    property SER          : string                   read FSER          write FSER;
    property SUB          : string                   read FSUB          write FSUB;
    property NUM_DOC      : string                   read FNUM_DOC      write FNUM_DOC;
    property CHV_CTE      : string                   read FCHV_CTE      write FCHV_CTE;
    property DT_DOC       : TDateTime                read FDT_DOC       write FDT_DOC;
    property DT_A_P       : TDateTime                read FDT_A_P       write FDT_A_P;
    property TP_CT_e      : string                   read FTP_CT_e      write FTP_CT_e;
    property CHV_CTE_REF  : string                   read FCHV_CTE_REF  write FCHV_CTE_REF;
    property VL_DOC       : Currency                 read FVL_DOC       write FVL_DOC;
    property VL_DESC      : Currency                 read FVL_DESC      write FVL_DESC;
    property IND_FRT      : TACBrTipoFrete           read FIND_FRT      write FIND_FRT;
    property VL_SERV      : Currency                 read FVL_SERV      write FVL_SERV;
    property VL_BC_ICMS   : Currency                 read FVL_BC_ICMS   write FVL_BC_ICMS;
    property VL_ICMS      : Currency                 read FVL_ICMS      write FVL_ICMS;
    property VL_NT        : Currency                 read FVL_NT        write FVL_NT;
    property COD_INF      : string                   read FCOD_INF      write FCOD_INF;
    property COD_CTA      : string                   read FCOD_CTA      write FCOD_CTA;

    property RegistroD101 : TRegistroD101List        read FRegistroD101 write FRegistroD101;
    property RegistroD105 : TRegistroD105List        read FRegistroD105 write FRegistroD105;
    property RegistroD111 : TRegistroD111List        read FRegistroD111 write FRegistroD111;
  end;

  // Registro D100 - Lista
  TRegistroD100List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD100;
    procedure SetItem(Index: Integer; const Value: TRegistroD100);
  public
    function New: TRegistroD100;
    property Items[Index: Integer]: TRegistroD100 read GetItem write SetItem;
  end;

  //REGISTRO D101: COMPLEMENTO DO DOCUMENTO DE TRANSPORTE (Códigos 07, 08, 8B, 09, 10, 11, 26, 27 e 57) – PIS/PASEP
  TRegistroD101 = class
  private
    fIND_NAT_FRT         : TACBrNaturezaFrtContratado;     //02	IND_NAT_FRT	Indicador da Natureza do Frete Contratado, referente a:0 – Operações de vendas, com ônus suportado pelo estabelecimento vendedor;1 – Operações de vendas, com ônus suportado pelo adquirente;2 – Operações de compras (bens para revenda, matérias-prima e outros produtos, geradores de crédito);3 – Operações de compras (bens para revenda, matérias-prima e outros produtos, não geradores de crédito);4 – Transferência de produtos acabados entre estabelecimentos da pessoa jurídica;5 – Transferência de produtos em elaboração entre estabelecimentos da pessoa jurídica;9 – Outras.	C	001*	-
    fVL_ITEM             : Currency;                       //03	VL_ITEM	Valor total dos itens	N	-	02
    fCST_PIS             : TACBrCstPis;                    //04	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fNAT_BC_CRED         : TACBrBaseCalculoCredito;        //05	NAT_BC_CRED	Código da Base de Cálculo do Crédito, conforme a Tabela indicada no item 4.3.7.	C	002*	-
    fVL_BC_PIS           : Currency;                       //06	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	 -	02
    fALIQ_PIS            : Currency;                       //07	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fVL_PIS              : Currency;                       //08	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCOD_CTA             : string;                         //09	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property IND_NAT_FRT : TACBrNaturezaFrtContratado read FIND_NAT_FRT write FIND_NAT_FRT;
    property VL_ITEM     : Currency                   read FVL_ITEM     write FVL_ITEM;
    property CST_PIS     : TACBrCstPis                read FCST_PIS     write FCST_PIS;
    property NAT_BC_CRED : TACBrBaseCalculoCredito    read FNAT_BC_CRED write FNAT_BC_CRED;
    property VL_BC_PIS   : Currency                   read FVL_BC_PIS   write FVL_BC_PIS;
    property ALIQ_PIS    : Currency                   read FALIQ_PIS    write FALIQ_PIS;
    property VL_PIS      : Currency                   read FVL_PIS      write FVL_PIS;
    property COD_CTA     : string                     read FCOD_CTA     write FCOD_CTA;
  end;

  // Registro D101 - Lista
  TRegistroD101List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD101;
    procedure SetItem(Index: Integer; const Value: TRegistroD101);
  public
    function New: TRegistroD101;
    property Items[Index: Integer]: TRegistroD101 read GetItem write SetItem;
  end;

  //REGISTRO D105: COMPLEMENTO DO DOCUMENTO DE TRANSPORTE (Códigos 07, 08, 8B, 09, 10, 11, 26, 27 e 57) – COFINS
  TRegistroD105 = class
  private
    fIND_NAT_FRT          : TACBrNaturezaFrtContratado;           //02	IND_NAT_FRT	Indicador da Natureza do Frete Contratado, referente a:0 – Operações de vendas, com ônus suportado pelo estabelecimento vendedor;1 – Operações de vendas, com ônus suportado pelo adquirente;2 – Operações de compras (bens para revenda, matérias-prima e outros produtos, geradores de crédito);3 – Operações de compras (bens para revenda, matérias-prima e outros produtos, não geradores de crédito);4 – Transferência de produtos acabados entre estabelecimentos da pessoa jurídica;5 – Transferência de produtos em elaboração entre estabelecimentos da pessoa jurídica;9 – Outras.	C	001*	-
    fVL_ITEM              : Currency;                             //03	VL_ITEM	Valor total dos itens	N	-	02
    fCST_COFINS           : TACBrSituacaoTribCOFINS;              //04	CST_COFINS	Código da Situação Tributária referente a COFINS	N	002*	-
    fNAT_BC_CRED          : TACBrBaseCalculoCredito;              //05	NAT_BC_CRED	Código da base de Cálculo do Crédito, conforme a Tabela indicada no item 4.3.7 	C	002*	-
    fVL_BC_COFINS         : Currency;                             //06	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	 -	02
    fALIQ_COFINS          : Currency;                             //07	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008	04
    fVL_COFINS            : Currency;                             //08	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA              : string;                               //09	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property IND_NAT_FRT  : TACBrNaturezaFrtContratado read FIND_NAT_FRT  write FIND_NAT_FRT;
    property VL_ITEM      : Currency                   read FVL_ITEM      write FVL_ITEM;
    property CST_COFINS   : TACBrSituacaoTribCOFINS    read FCST_COFINS   write FCST_COFINS;
    property NAT_BC_CRED  : TACBrBaseCalculoCredito    read FNAT_BC_CRED  write FNAT_BC_CRED;
    property VL_BC_COFINS : Currency                   read FVL_BC_COFINS write FVL_BC_COFINS;
    property ALIQ_COFINS  : Currency                   read FALIQ_COFINS  write FALIQ_COFINS;
    property VL_COFINS    : Currency                   read FVL_COFINS    write FVL_COFINS;
    property COD_CTA      : string                     read FCOD_CTA      write FCOD_CTA;
  end;

  // Registro D105 - Lista
  TRegistroD105List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD105;
    procedure SetItem(Index: Integer; const Value: TRegistroD105);
  public
    function New: TRegistroD105;
    property Items[Index: Integer]: TRegistroD105 read GetItem write SetItem;
  end;

  //REGISTRO D111: PROCESSO REFERENCIADO
  TRegistroD111 = class
  private
    fNUM_PROC         : string;               //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso;  //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil;9 – Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro D111 - Lista
  TRegistroD111List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD111;
    procedure SetItem(Index: Integer; const Value: TRegistroD111);
  public
    function New: TRegistroD111;
    property Items[Index: Integer]: TRegistroD111 read GetItem write SetItem;
  end;

  //REGISTRO D200: RESUMO DA ESCRITURAÇÃO DIÁRIA – PRESTAÇÃO DE SERVIÇOS DE TRANSPORTE - NOTA FISCAL DE SERVIÇO DE TRANSPORTE (CÓDIGO 07) E CONHECIMENTOS DE TRANSPORTE RODOVIÁRIO DE CARGAS (CÓDIGO 08), CONHECIMENTO DE TRANSPORTE DE CARGAS AVULSO (CÓDIGO 8B), AQUAVIÁRIO DE CARGAS (CÓDIGO 09), AÉREO (CÓDIGO 10), FERROVIÁRIO DE CARGAS (CÓDIGO 11), MULTIMODAL DE CARGAS (CÓDIGO 26), NOTA FISCAL DE TRANSPORTE FERROVIÁRIO DE CARGA (CÓDIGO 27) E CONHECIMENTO DE TRANSPORTE ELETRÔNICO – CT-e (CÓDIGO 57)
  TRegistroD200 = class
  private
    fCOD_MOD              : string;            //02	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1 	C	002*	-
    fCOD_SIT              : TACBrSituacaoDF;   //03	COD_SIT	Código da situação do documento fiscal, conforme a Tabela 4.1.2	N	002*	-
    fSER                  : string;            //04	SER	Série do documento fiscal	C	004	-
    fSUB                  : string;            //05	SUB	Subsérie do documento fiscal	C	003	-
    fNUM_DOC_INI          : Integer;           //06	NUM_DOC_INI	Número do documento fiscal inicial emitido no período (mesmo modelo, série e subsérie).	N	009	-
    fNUM_DOC_FIN          : Integer;           //07	NUM_DOC_FIN	Número do documento fiscal final emitido no período (mesmo modelo, série e subsérie).	N	009	-
    fCFOP                 : Integer;           //08	CFOP	Código Fiscal de Operação e Prestação conforme tabela indicada no item 4.2.2	N	004*	-
    fDT_REF               : TDateTime;         //09	DT_REF	Data do dia de referência do resumo diário	N	008*	-
    fVL_DOC               : Currency;          //10	VL_DOC	Valor total dos documentos fiscais	N	-	02
    fVL_DESC              : Currency;          //11	VL_DESC	Valor total dos descontos	N	-	02

    FRegistroD201         : TRegistroD201List;
    FRegistroD205         : TRegistroD205List;
    FRegistroD209         : TRegistroD209List;
  public
    constructor Create;  virtual;              // Create
    destructor  Destroy; override;             // Destroy

    property COD_MOD      : string            read FCOD_MOD      write FCOD_MOD;
    property COD_SIT      : TACBrSituacaoDF   read FCOD_SIT      write FCOD_SIT;
    property SER          : string            read FSER          write FSER;
    property SUB          : string            read FSUB          write FSUB;
    property NUM_DOC_INI  : Integer           read FNUM_DOC_INI  write FNUM_DOC_INI;
    property NUM_DOC_FIN  : Integer           read FNUM_DOC_FIN  write FNUM_DOC_FIN;
    property CFOP         : Integer           read FCFOP         write FCFOP;
    property DT_REF       : TDateTime         read FDT_REF       write FDT_REF;
    property VL_DOC       : Currency          read FVL_DOC       write FVL_DOC;
    property VL_DESC      : Currency          read FVL_DESC      write FVL_DESC;

    property RegistroD201 : TRegistroD201List read FRegistroD201 write FRegistroD201;
    property RegistroD205 : TRegistroD205List read FRegistroD205 write FRegistroD205;
    property RegistroD209 : TRegistroD209List read FRegistroD209 write FRegistroD209;
  end;

  // Registro D200 - Lista
  TRegistroD200List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD200;
    procedure SetItem(Index: Integer; const Value: TRegistroD200);
  public
    function New: TRegistroD200;
    property Items[Index: Integer]: TRegistroD200 read GetItem write SetItem;
  end;

  //REGISTRO D201: TOTALIZAÇÃO DO RESUMO DIÁRIO – PIS/PASEP
  TRegistroD201 = class
  private
    fCST_PIS           : TACBrCstPis;          //02	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fVL_ITEM           : Currency;             //03	VL_ITEM	Valor total dos itens	N	-	02
    fVL_BC_PIS         : Currency;             //04	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	-	02
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

  // Registro D201 - Lista
  TRegistroD201List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD201;
    procedure SetItem(Index: Integer; const Value: TRegistroD201);
  public
    function New: TRegistroD201;
    property Items[Index: Integer]: TRegistroD201 read GetItem write SetItem;
  end;

  //REGISTRO D205: TOTALIZAÇÃO DO RESUMO DIÁRIO – COFINS
  TRegistroD205 = class
  private
    fCST_COFINS           : TACBrSituacaoTribCOFINS;              //02	CST_COFINS	Código da Situação Tributária referente a COFINS.	N	002*	-
    fVL_ITEM              : Currency;                             //03	VL_ITEM	Valor total dos itens	N	-	02
    fVL_BC_COFINS         : Currency;                             //04	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	-	02
    fALIQ_COFINS          : Currency;                             //05	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008	04
    fVL_COFINS            : Currency;                             //06	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA              : string;                               //07	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_COFINS   : TACBrSituacaoTribCOFINS   read FCST_COFINS   write FCST_COFINS;
    property VL_ITEM      : Currency                  read FVL_ITEM      write FVL_ITEM;
    property VL_BC_COFINS : Currency                  read FVL_BC_COFINS write FVL_BC_COFINS;
    property ALIQ_COFINS  : Currency                  read FALIQ_COFINS  write FALIQ_COFINS;
    property VL_COFINS    : Currency                  read FVL_COFINS    write FVL_COFINS;
    property COD_CTA      : string                    read FCOD_CTA      write FCOD_CTA;
  end;

  // Registro D205 - Lista
  TRegistroD205List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD205;
    procedure SetItem(Index: Integer; const Value: TRegistroD205);
  public
    function New: TRegistroD205;
    property Items[Index: Integer]: TRegistroD205 read GetItem write SetItem;
  end;

  //REGISTRO D209: PROCESSO REFERENCIADO
  TRegistroD209 = class
  private
    fNUM_PROC         : string;              //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso; //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil;9 – Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro D209 - Lista
  TRegistroD209List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD209;
    procedure SetItem(Index: Integer; const Value: TRegistroD209);
  public
    function New: TRegistroD209;
    property Items[Index: Integer]: TRegistroD209 read GetItem write SetItem;
  end;

  //REGISTRO D300: RESUMO DA ESCRITURAÇÃO DIÁRIA - BILHETES CONSOLIDADOS DE PASSAGEM RODOVIÁRIO (CÓDIGO 13), DE PASSAGEM AQUAVIÁRIO (CÓDIGO 14), DE PASSAGEM E NOTA DE BAGAGEM (CÓDIGO 15) E DE PASSAGEM FERROVIÁRIO (CÓDIGO 16)
  TRegistroD300 = class
  private
    fCOD_MOD              : string;                  //02	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1.	C	002*	-
    fSER                  : string;                  //03	SER	Série do documento fiscal	C	004	-
    fSUB                  : Integer;                 //04	SUB	Subsérie do documento fiscal	N	003	-
    fNUM_DOC_INI          : Integer;                 //05	NUM_DOC_INI	Número do primeiro documento fiscal emitido no período (mesmo modelo, série e subsérie)	N	006	-
    fNUM_DOC_FIN          : Integer;                 //06	NUM_DOC_FIN	Número do último documento fiscal emitido no período (mesmo modelo, série e subsérie)	N	006	-
    fCFOP                 : Integer;                 //07	CFOP	Código Fiscal de Operação e Prestação conforme tabela indicada no item 4.2.2	N	004*	-
    fDT_REF               : TDateTime;               //08	DT_REF	Data do dia de referência do resumo diário	N	008*	-
    fVL_DOC               : Currency;                //09	VL_DOC	Valor total dos documentos fiscais emitidos	N	-	02
    fVL_DESC              : Currency;                //10	VL_DESC	Valor total dos descontos	N	-	02
    fCST_PIS              : TACBrCstPis;             //11	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fVL_BC_PIS            : Currency;                //12	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	-	02
    fALIQ_PIS             : Currency;                //13	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fVL_PIS               : Currency;                //14	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCST_COFINS           : TACBrSituacaoTribCOFINS; //15	CST_COFINS	Código da Situação Tributária referente a COFINS	N	002*	-
    fVL_BC_COFINS         : Currency;                //16	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	-	02
    fALIQ_COFINS          : Currency;                //17	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008	04
    fVL_COFINS            : Currency;                //18	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA              : string;                  //19	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-

    FRegistroD309         : TRegistroD309List;
  public
    constructor Create; virtual;                     // Create
    destructor Destroy; override;                    // Destroy

    property COD_MOD      : string                  read FCOD_MOD      write FCOD_MOD;
    property SER          : string                  read FSER          write FSER;
    property SUB          : Integer                 read FSUB          write FSUB;
    property NUM_DOC_INI  : Integer                 read FNUM_DOC_INI  write FNUM_DOC_INI;
    property NUM_DOC_FIN  : Integer                 read FNUM_DOC_FIN  write FNUM_DOC_FIN;
    property CFOP         : Integer                 read FCFOP         write FCFOP;
    property DT_REF       : TDateTime               read FDT_REF       write FDT_REF;
    property VL_DOC       : Currency                read FVL_DOC       write FVL_DOC;
    property VL_DESC      : Currency                read FVL_DESC      write FVL_DESC;
    property CST_PIS      : TACBrCstPis             read FCST_PIS      write FCST_PIS;
    property VL_BC_PIS    : Currency                read FVL_BC_PIS    write FVL_BC_PIS;
    property ALIQ_PIS     : Currency                read FALIQ_PIS     write FALIQ_PIS;
    property VL_PIS       : Currency                read FVL_PIS       write FVL_PIS;
    property CST_COFINS   : TACBrSituacaoTribCOFINS read FCST_COFINS   write FCST_COFINS;
    property VL_BC_COFINS : Currency                read FVL_BC_COFINS write FVL_BC_COFINS;
    property ALIQ_COFINS  : Currency                read FALIQ_COFINS  write FALIQ_COFINS;
    property VL_COFINS    : Currency                read FVL_COFINS    write FVL_COFINS;
    property COD_CTA      : string                  read FCOD_CTA      write FCOD_CTA;

    property RegistroD309 : TRegistroD309List       read FRegistroD309 write FRegistroD309;
  end;

  // Registro D300 - Lista
  TRegistroD300List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD300;
    procedure SetItem(Index: Integer; const Value: TRegistroD300);
  public
    function New: TRegistroD300;
    property Items[Index: Integer]: TRegistroD300 read GetItem write SetItem;
  end;

  //REGISTRO D309: PROCESSO REFERENCIADO
  TRegistroD309 = class
  private
    fNUM_PROC         : string;              //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso; //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil ;9 – Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro D309 - Lista
  TRegistroD309List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD309;
    procedure SetItem(Index: Integer; const Value: TRegistroD309);
  public
    function New: TRegistroD309;
    property Items[Index: Integer]: TRegistroD309 read GetItem write SetItem;
  end;

  //REGISTRO D350: RESUMO DIÁRIO DE CUPOM FISCAL EMITIDO POR ECF - (CÓDIGOS 2E, 13, 14, 15 e 16)
  TRegistroD350 = class
  private
    fCOD_MOD                   : string;                  //02	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1	C	002*	-
    fECF_MOD                   : string;                  //03	ECF_MOD	Modelo do equipamento	C	020	-
    fECF_FAB                   : string;                  //04	ECF_FAB	Número de série de fabricação do ECF	C	020	-
    fDT_DOC                    : TDateTime;               //05	DT_DOC	Data do movimento a que se refere a Redução Z	N	008*	-
    fCRO                       : Integer;                 //06	CRO	Posição do Contador de Reinício de Operação	N	003	-
    fCRZ                       : Integer;                 //07	CRZ	Posição do Contador de Redução Z	N	006	-
    fNUM_COO_FIN               : Integer;                 //08	NUM_COO_FIN	Número do Contador de Ordem de Operação do último documento emitido no dia. (Número do COO na Redução Z)	N	006	-
    fGT_FIN                    : Currency;                //09	GT_FIN	Valor do Grande Total final	N	-	02
    fVL_BRT                    : Currency;                //10	VL_BRT	Valor da venda bruta	N	-	02
    fCST_PIS                   : TACBrCstPis;             //11	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fVL_BC_PIS                 : Variant;                 //12	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	- 	02
    fALIQ_PIS                  : Variant;                 //13	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fQUANT_BC_PIS              : Variant;                 //14	QUANT_BC_PIS	Quantidade – Base de cálculo PIS/PASEP	N	- 	03
    fALIQ_PIS_QUANT            : Variant;                 //15	ALIQ_PIS_QUANT	Alíquota do PIS/PASEP (em reais)	N	 -	04
    fVL_PIS                    : Variant;                 //16	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCST_COFINS                : TACBrSituacaoTribCOFINS; //17	CST_COFINS	Código da Situação Tributária referente a COFINS	N	002*	-
    fVL_BC_COFINS              : Variant;                 //18	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	 -	02
    fALIQ_COFINS               : Variant;                 //19	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008	04
    fQUANT_BC_COFINS           : Variant;                 //20	QUANT_BC_COFINS	Quantidade – Base de cálculo da COFINS	N	- 	03
    fALIQ_COFINS_QUANT         : Variant;                 //21	ALIQ_COFINS_QUANT	Alíquota da COFINS (em reais)	N	- 	04
    fVL_COFINS                 : Variant;                 //22	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA                   : string;                  //23	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-

    FRegistroD359              : TRegistroD359List;
  public
    constructor Create;  virtual;                         // Create
    destructor  Destroy; override;                        // Destroy

    property COD_MOD           : string                  read FCOD_MOD           write FCOD_MOD;
    property ECF_MOD           : string                  read FECF_MOD           write FECF_MOD;
    property ECF_FAB           : string                  read FECF_FAB           write FECF_FAB;
    property DT_DOC            : TDateTime               read FDT_DOC            write FDT_DOC;
    property CRO               : Integer                 read FCRO               write FCRO;
    property CRZ               : Integer                 read FCRZ               write FCRZ;
    property NUM_COO_FIN       : Integer                 read FNUM_COO_FIN       write FNUM_COO_FIN;
    property GT_FIN            : Currency                read FGT_FIN            write FGT_FIN;
    property VL_BRT            : Currency                read FVL_BRT            write FVL_BRT;
    property CST_PIS           : TACBrCstPis             read FCST_PIS           write FCST_PIS;
    property VL_BC_PIS         : Variant                 read FVL_BC_PIS         write FVL_BC_PIS;
    property ALIQ_PIS          : Variant                 read FALIQ_PIS          write FALIQ_PIS;
    property QUANT_BC_PIS      : Variant                 read FQUANT_BC_PIS      write FQUANT_BC_PIS;
    property ALIQ_PIS_QUANT    : Variant                 read FALIQ_PIS_QUANT    write FALIQ_PIS_QUANT;
    property VL_PIS            : Variant                 read FVL_PIS            write FVL_PIS;
    property CST_COFINS        : TACBrSituacaoTribCOFINS read FCST_COFINS        write FCST_COFINS;
    property VL_BC_COFINS      : Variant                 read FVL_BC_COFINS      write FVL_BC_COFINS;
    property ALIQ_COFINS       : Variant                 read FALIQ_COFINS       write FALIQ_COFINS;
    property QUANT_BC_COFINS   : Variant                 read FQUANT_BC_COFINS   write FQUANT_BC_COFINS;
    property ALIQ_COFINS_QUANT : Variant                 read FALIQ_COFINS_QUANT write FALIQ_COFINS_QUANT;
    property VL_COFINS         : Variant                 read FVL_COFINS         write FVL_COFINS;
    property COD_CTA           : string                  read FCOD_CTA           write FCOD_CTA;

    property RegistroD359      : TRegistroD359List       read FRegistroD359      write FRegistroD359;
  end;

  // Registro D350 - Lista
  TRegistroD350List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD350;
    procedure SetItem(Index: Integer; const Value: TRegistroD350);
  public
    function New: TRegistroD350;
    property Items[Index: Integer]: TRegistroD350 read GetItem write SetItem;
  end;

  //REGISTRO D359: PROCESSO REFERENCIADO
  TRegistroD359 = class
  private
    fNUM_PROC         : string;              //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso; //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil;9 – Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro D359 - Lista
  TRegistroD359List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD359;
    procedure SetItem(Index: Integer; const Value: TRegistroD359);
  public
    function New: TRegistroD359;
    property Items[Index: Integer]: TRegistroD359 read GetItem write SetItem;
  end;

  //REGISTRO D500: NOTA FISCAL DE SERVIÇO DE COMUNICAÇÃO (CÓDIGO 21) E NOTA FISCAL DE SERVIÇO DE TELECOMUNICAÇÃO (CÓDIGO 22) – DOCUMENTOS DE AQUISIÇÃO COM DIREITO A CRÉDITO
  TRegistroD500 = class
  private
    fIND_OPER             : TACBrIndicadorTpOperacao; //02	IND_OPER	Indicador do tipo de operação:0- Aquisição	C	001*	-
    fIND_EMIT             : TACBrIndicadorEmitenteDF; //03	IND_EMIT	Indicador do emitente do documento fiscal:0- Emissão própria;1- Terceiros	C	001*	-
    fCOD_PART             : string;                   //04	COD_PART	Código do participante prestador do serviço (campo 02 do Registro 0150).	C	060	-
    fCOD_MOD              : string;                   //05	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1.	C	002*	-
    fCOD_SIT              : TACBrSituacaoDF;          //06	COD_SIT	Çódigo da situação do documento fiscal, conforme a Tabela 4.1.2.	N	002*	-
    fSER                  : string;                   //07	SER	Série do documento fiscal	C	004	-
    fSUB                  : Integer;                  //08	SUB	Subsérie do documento fiscal	N	003	-
    fNUM_DOC              : Integer;                  //09	NUM_DOC	Número do documento fiscal	N	009	-
    fDT_DOC               : TDateTime;                //10	DT_DOC	Data da emissão do documento fiscal	N	008*	-
    fDT_A_P               : TDateTime;                //11	DT_A_P	Data da entrada (aquisição)	N	008*	-
    fVL_DOC               : Currency;                 //12	VL_DOC	Valor total do documento fiscal	N	-	02
    fVL_DESC              : Currency;                 //13	VL_DESC	Valor total do desconto	N	-	02
    fVL_SERV              : Currency;                 //14	VL_SERV	Valor da prestação de serviços	N	-	02
    fVL_SERV_NT           : Currency;                 //15	VL_SERV_NT	Valor total dos serviços não-tributados pelo ICMS	N	-	02
    fVL_TERC              : Currency;                 //16	VL_TERC	Valores cobrados em nome de terceiros	N	-	02
    fVL_DA                : Currency;                 //17	VL_DA	Valor de outras despesas indicadas no documento fiscal	N	-	02
    fVL_BC_ICMS           : Currency;                 //18	VL_BC_ICMS	Valor da base de cálculo do ICMS	N	-	02
    fVL_ICMS              : Currency;                 //19	VL_ICMS	Valor do ICMS	N	-	02
    fCOD_INF              : string;                   //20	COD_INF	Código da informação complementar (campo 02 do Registro 0450)	C	006	-
    fVL_PIS               : Currency;                 //21	VL_PIS	Valor do PIS/PASEP	N	-	02
    fVL_COFINS            : Currency;                 //22	VL_COFINS	Valor da COFINS	N	-	02

    FRegistroD501         : TRegistroD501List;
    FRegistroD505         : TRegistroD505List;
    FRegistroD509         : TRegistroD509List;
  public
    constructor Create;  virtual;                     // Create
    destructor  Destroy; override;                    // Destroy

    property IND_OPER     : TACBrIndicadorTpOperacao read FIND_OPER     write FIND_OPER;
    property IND_EMIT     : TACBrIndicadorEmitenteDF read FIND_EMIT     write FIND_EMIT;
    property COD_PART     : string                   read FCOD_PART     write FCOD_PART;
    property COD_MOD      : string                   read FCOD_MOD      write FCOD_MOD;
    property COD_SIT      : TACBrSituacaoDF          read FCOD_SIT      write FCOD_SIT;
    property SER          : string                   read FSER          write FSER;
    property SUB          : Integer                  read FSUB          write FSUB;
    property NUM_DOC      : Integer                  read FNUM_DOC      write FNUM_DOC;
    property DT_DOC       : TDateTime                read FDT_DOC       write FDT_DOC;
    property DT_A_P       : TDateTime                read FDT_A_P       write FDT_A_P;
    property VL_DOC       : Currency                 read FVL_DOC       write FVL_DOC;
    property VL_DESC      : Currency                 read FVL_DESC      write FVL_DESC;
    property VL_SERV      : Currency                 read FVL_SERV      write FVL_SERV;
    property VL_SERV_NT   : Currency                 read FVL_SERV_NT   write FVL_SERV_NT;
    property VL_TERC      : Currency                 read FVL_TERC      write FVL_TERC;
    property VL_DA        : Currency                 read FVL_DA        write FVL_DA;
    property VL_BC_ICMS   : Currency                 read FVL_BC_ICMS   write FVL_BC_ICMS;
    property VL_ICMS      : Currency                 read FVL_ICMS      write FVL_ICMS;
    property COD_INF      : string                   read FCOD_INF      write FCOD_INF;
    property VL_PIS       : Currency                 read FVL_PIS       write FVL_PIS;
    property VL_COFINS    : Currency                 read FVL_COFINS    write FVL_COFINS;

    property RegistroD501 : TRegistroD501List        read FRegistroD501 write FRegistroD501;
    property RegistroD505 : TRegistroD505List        read FRegistroD505 write FRegistroD505;
    property RegistroD509 : TRegistroD509List        read FRegistroD509 write FRegistroD509;
  end;

  // Registro D500 - Lista
  TRegistroD500List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD500;
    procedure SetItem(Index: Integer; const Value: TRegistroD500);
  public
    function New: TRegistroD500;
    property Items[Index: Integer]: TRegistroD500 read GetItem write SetItem;
  end;

  //REGISTRO D501: COMPLEMENTO DA OPERAÇÃO (CÓDIGOS 21 e 22) – PIS/PASEP
  TRegistroD501 = class
  private
    fCST_PIS             : TACBrCstPis;                    //02	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fVL_ITEM             : Currency;                       //03	VL_ITEM	Valor Total dos Itens (Serviços)	N	-	02
    fNAT_BC_CRED         : TACBrBaseCalculoCredito;        //04	NAT_BC_CRED	Código da Base de Cálculo do Crédito, conforme a Tabela indicada no item 4.3.7.	C	002*	-
    fVL_BC_PIS           : Currency;                       //05	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	 -	02
    fALIQ_PIS            : Currency;                       //06	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fVL_PIS              : Currency;                       //07	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCOD_CTA             : string;                         //08	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_PIS     : TACBrCstPis             read FCST_PIS     write FCST_PIS;
    property VL_ITEM     : Currency                read FVL_ITEM     write FVL_ITEM;
    property NAT_BC_CRED : TACBrBaseCalculoCredito read FNAT_BC_CRED write FNAT_BC_CRED;
    property VL_BC_PIS   : Currency                read FVL_BC_PIS   write FVL_BC_PIS;
    property ALIQ_PIS    : Currency                read FALIQ_PIS    write FALIQ_PIS;
    property VL_PIS      : Currency                read FVL_PIS      write FVL_PIS;
    property COD_CTA     : string                  read FCOD_CTA     write FCOD_CTA;
  end;

  // Registro D501 - Lista
  TRegistroD501List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD501;
    procedure SetItem(Index: Integer; const Value: TRegistroD501);
  public
    function New: TRegistroD501;
    property Items[Index: Integer]: TRegistroD501 read GetItem write SetItem;
  end;

  //REGISTRO D505: COMPLEMENTO DA OPERAÇÃO (CÓDIGOS 21 e 22) – COFINS
  TRegistroD505 = class
  private
    fCST_COFINS           : TACBrSituacaoTribCOFINS;           //02	CST_COFINS	Código da Situação Tributária referente a COFINS	N	002*	-
    fVL_ITEM              : Currency;                          //03	VL_ITEM	Valor Total dos Itens	N	-	02
    fNAT_BC_CRED          : TACBrBaseCalculoCredito;           //04	NAT_BC_CRED	Código da Base de Cálculo do Crédito, conforme a Tabela indicada no item 4.3.7.	C	002*	-
    fVL_BC_COFINS         : Currency;                          //05	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	 -	02
    fALIQ_COFINS          : Currency;                          //06	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008	04
    fVL_COFINS            : Currency;                          //07	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA              : string;                            //08	COD_CTA	Código da conta analítica contábil debitada/creditada	C	060	-
  public
    property CST_COFINS   : TACBrSituacaoTribCOFINS   read FCST_COFINS   write FCST_COFINS;
    property VL_ITEM      : Currency                  read FVL_ITEM      write FVL_ITEM;
    property NAT_BC_CRED  : TACBrBaseCalculoCredito   read FNAT_BC_CRED  write FNAT_BC_CRED;
    property VL_BC_COFINS : Currency                  read FVL_BC_COFINS write FVL_BC_COFINS;
    property ALIQ_COFINS  : Currency                  read FALIQ_COFINS  write FALIQ_COFINS;
    property VL_COFINS    : Currency                  read FVL_COFINS    write FVL_COFINS;
    property COD_CTA      : string                    read FCOD_CTA      write FCOD_CTA;
  end;

  // Registro D505 - Lista
  TRegistroD505List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD505;
    procedure SetItem(Index: Integer; const Value: TRegistroD505);
  public
    function New: TRegistroD505;
    property Items[Index: Integer]: TRegistroD505 read GetItem write SetItem;
  end;

  //REGISTRO D509: PROCESSO REFERENCIADO
  TRegistroD509 = class
  private
    fNUM_PROC         : string;              //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso; //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil;9 – Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro D509 - Lista
  TRegistroD509List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD509;
    procedure SetItem(Index: Integer; const Value: TRegistroD509);
  public
    function New: TRegistroD509;
    property Items[Index: Integer]: TRegistroD509 read GetItem write SetItem;
  end;

  //REGISTRO D600: CONSOLIDAÇÃO DA PRESTAÇÃO DE SERVIÇOS - NOTAS DE SERVIÇO DE COMUNICAÇÃO (CÓDIGO 21) E DE SERVIÇO DE TELECOMUNICAÇÃO (CÓDIGO 22)
  TRegistroD600 = class
  private
    fCOD_MOD              : string;             //02	COD_MOD	Código do modelo do documento fiscal, conforme a Tabela 4.1.1.	C	002*	-
    fCOD_MUN              : Integer;            //03	COD_MUN	Código do município dos terminais faturados, conforme a tabela IBGE	N	007*	-
    fSER                  : string;             //04	SER	Série do documento fiscal	C	004	-
    fSUB                  : Integer;            //05	SUB	Subsérie do documento fiscal	N	003	-
    fIND_REC              : TACBrIndRec;        //06	IND_REC	Indicador do tipo de receita:0- Receita própria - serviços prestados;1- Receita própria - cobrança de débitos;2- Receita própria - venda de serviço pré-pago – faturamento de períodos anteriores;3- Receita própria - venda de serviço pré-pago – faturamento no período;4- Outras receitas próprias de serviços de comunicação e telecomunicação;5- Receita própria - co-faturamento;6- Receita própria – serviços a faturar em período futuro;7– Outras receitas próprias de natureza não-cumulativa;8 - Outras receitas de terceiros;9 – Outras receitas	N	001*	-
    fQTD_CONS             : Integer;            //07	QTD_CONS	Quantidade de documentos consolidados neste registro	N	-	-
    fDT_DOC_INI           : TDateTime;          //08	DT_DOC_INI	Data Inicial dos documentos consolidados no período	N	008*	-
    fDT_DOC_FIN           : TDateTime;          //09	DT_DOC_FIN	Data Final dos documentos consolidados no período	N	008*	-
    fVL_DOC               : Currency;           //10	VL_DOC	Valor total acumulado dos documentos fiscais	N	-	02
    fVL_DESC              : Currency;           //11	VL_DESC	Valor acumulado dos descontos	N	-	02
    fVL_SERV              : Currency;           //12	VL_SERV	Valor acumulado das prestações de serviços tributados pelo ICMS	N	-	02
    fVL_SERV_NT           : Currency;           //13	VL_SERV_NT	Valor acumulado dos serviços não-tributados pelo ICMS	N	-	02
    fVL_TERC              : Currency;           //14	VL_TERC	Valores cobrados em nome de terceiros	N	-	02
    fVL_DA                : Currency;           //15	VL_DA	Valor acumulado das despesas acessórias	N	-	02
    fVL_BC_ICMS           : Currency;           //16	VL_BC_ICMS	Valor acumulado da base de cálculo do ICMS	N	-	02
    fVL_ICMS              : Currency;           //17	VL_ICMS	Valor acumulado do ICMS	N	-	02
    fVL_PIS               : Currency;           //18	VL_PIS	Valor do PIS/PASEP	N	-	02
    fVL_COFINS            : Currency;           //19	VL_COFINS	Valor da COFINS	N	-	02

    FRegistroD601         : TRegistroD601List;
    FRegistroD605         : TRegistroD605List;
    FRegistroD609         : TRegistroD609List;
  public
    constructor Create; virtual;                // Create
    destructor  Destroy; override;              // Destroy

    property COD_MOD      : string            read FCOD_MOD      write FCOD_MOD;
    property COD_MUN      : Integer           read FCOD_MUN      write FCOD_MUN;
    property SER          : string            read FSER          write FSER;
    property SUB          : Integer           read FSUB          write FSUB;
    property IND_REC      : TACBrIndRec       read FIND_REC      write FIND_REC;
    property QTD_CONS     : Integer           read FQTD_CONS     write FQTD_CONS;
    property DT_DOC_INI   : TDateTime         read FDT_DOC_INI   write FDT_DOC_INI;
    property DT_DOC_FIN   : TDateTime         read FDT_DOC_FIN   write FDT_DOC_FIN;
    property VL_DOC       : Currency          read FVL_DOC       write FVL_DOC;
    property VL_DESC      : Currency          read FVL_DESC      write FVL_DESC;
    property VL_SERV      : Currency          read FVL_SERV      write FVL_SERV;
    property VL_SERV_NT   : Currency          read FVL_SERV_NT   write FVL_SERV_NT;
    property VL_TERC      : Currency          read FVL_TERC      write FVL_TERC;
    property VL_DA        : Currency          read FVL_DA        write FVL_DA;
    property VL_BC_ICMS   : Currency          read FVL_BC_ICMS   write FVL_BC_ICMS;
    property VL_ICMS      : Currency          read FVL_ICMS      write FVL_ICMS;
    property VL_PIS       : Currency          read FVL_PIS       write FVL_PIS;
    property VL_COFINS    : Currency          read FVL_COFINS    write FVL_COFINS;

    property RegistroD601 : TRegistroD601List read FRegistroD601 write FRegistroD601;
    property RegistroD605 : TRegistroD605List read FRegistroD605 write FRegistroD605;
    property RegistroD609 : TRegistroD609List read FRegistroD609 write FRegistroD609;
  end;

                                                // Registro D600 - Lista
  TRegistroD600List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD600;
    procedure SetItem(Index: Integer; const Value: TRegistroD600);
  public
    function New: TRegistroD600;
    property Items[Index: Integer]: TRegistroD600 read GetItem   write SetItem;
  end;

  //REGISTRO D601: COMPLEMENTO DA CONSOLIDAÇÃO DA PRESTAÇÃO DE SERVIÇOS (CÓDIGOS 21 E 22) - PIS/PASEP
  TRegistroD601 = class
  private
    fCOD_CLASS         : Integer;              //02	COD_CLASS	Código de classificação do item do serviço de comunicação ou de telecomunicação, conforme a Tabela 4.4.1	N	004*	-
    fVL_ITEM           : Variant;             //03	VL_ITEM	Valor acumulado do item	N	-	02
    fVL_DESC           : Variant;             //04	VL_DESC	Valor acumulado dos descontos/exclusões da base de cálculo	N	-	02
    fCST_PIS           : TACBrCstPis;          //05	CST_PIS	Código da Situação Tributária referente ao PIS/PASEP	N	002*	-
    fVL_BC_PIS         : Variant;             //06	VL_BC_PIS	Valor da base de cálculo do PIS/PASEP	N	 -	02
    fALIQ_PIS          : Variant;             //07	ALIQ_PIS	Alíquota do PIS/PASEP (em percentual)	N	008	04
    fVL_PIS            : Variant;             //08	VL_PIS	Valor do PIS/PASEP	N	-	02
    fCOD_CTA           : string;               //09	COD_CTA	Código da conta contábil debitada/creditada	C	060	-
  public
    property COD_CLASS : Integer     read FCOD_CLASS write FCOD_CLASS;
    property VL_ITEM   : Variant     read FVL_ITEM   write FVL_ITEM;
    property VL_DESC   : Variant     read FVL_DESC   write FVL_DESC;
    property CST_PIS   : TACBrCstPis read FCST_PIS   write FCST_PIS;
    property VL_BC_PIS : Variant     read FVL_BC_PIS write FVL_BC_PIS;
    property ALIQ_PIS  : Variant     read FALIQ_PIS  write FALIQ_PIS;
    property VL_PIS    : Variant     read FVL_PIS    write FVL_PIS;
    property COD_CTA   : string      read FCOD_CTA   write FCOD_CTA;
  end;

  // Registro D601 - Lista
  TRegistroD601List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD601;
    procedure SetItem(Index: Integer; const Value: TRegistroD601);
  public
    function New: TRegistroD601;
    property Items[Index: Integer]: TRegistroD601 read GetItem write SetItem;
  end;

  //REGISTRO D605: COMPLEMENTO DA CONSOLIDAÇÃO DA PRESTAÇÃO DE SERVIÇOS (CÓDIGOS 21 E 22) - COFINS
  TRegistroD605 = class
  private
    fCOD_CLASS            : Integer;                 //02	COD_CLASS	Código de classificação do item do serviço de comunicação ou de telecomunicação, conforme a Tabela 4.4.1	N	004*	-
    fVL_ITEM              : Variant;                 //03	VL_ITEM	Valor acumulado do item	N	-	02
    fVL_DESC              : Variant;                 //04	VL_DESC	Valor acumulado dos descontos/exclusões da base de cálculo	N	-	02
    fCST_COFINS           : TACBrSituacaoTribCOFINS; //05	CST_COFINS	Código da Situação Tributária referente a COFINS	N	002*	-
    fVL_BC_COFINS         : Variant;                 //06	VL_BC_COFINS	Valor da base de cálculo da COFINS	N	 -	02
    fALIQ_COFINS          : Variant;                 //07	ALIQ_COFINS	Alíquota da COFINS (em percentual)	N	008-	04
    fVL_COFINS            : Variant;                 //08	VL_COFINS	Valor da COFINS	N	-	02
    fCOD_CTA              : string;                  //09	COD_CTA	Código da conta contábil debitada/creditada	C	060	-
  public
    property COD_CLASS    : Integer                 read FCOD_CLASS    write FCOD_CLASS;
    property VL_ITEM      : Variant                 read FVL_ITEM      write FVL_ITEM;
    property VL_DESC      : Variant                 read FVL_DESC      write FVL_DESC;
    property CST_COFINS   : TACBrSituacaoTribCOFINS read FCST_COFINS   write FCST_COFINS;
    property VL_BC_COFINS : Variant                 read FVL_BC_COFINS write FVL_BC_COFINS;
    property ALIQ_COFINS  : Variant                 read FALIQ_COFINS  write FALIQ_COFINS;
    property VL_COFINS    : Variant                 read FVL_COFINS    write FVL_COFINS;
    property COD_CTA      : string                  read FCOD_CTA      write FCOD_CTA;
  end;

  // Registro D605 - Lista
  TRegistroD605List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroD605;
    procedure SetItem(Index: Integer; const Value: TRegistroD605);
  public
    function New: TRegistroD605;
    property Items[Index: Integer]: TRegistroD605 read GetItem write SetItem;
  end;

  //REGISTRO D609: PROCESSO REFERENCIADO
  TRegistroD609 = class
  private
    fNUM_PROC         : string;              //02	NUM_PROC	Identificação do processo ou ato concessório	C	020	-
    fIND_PROC         : TACBrOrigemProcesso; //03	IND_PROC	Indicador da origem do processo:1 - Justiça Federal;3 – Secretaria da Receita Federal do Brasil;9 – Outros.	C	001*	-
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro D609 - Lista
  TRegistroD609List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD609;
    procedure SetItem(Index: Integer; const Value: TRegistroD609);
  public
    function New: TRegistroD609;
    property Items[Index: Integer]: TRegistroD609 read GetItem write SetItem;
  end;

  //REGISTRO D990: ENCERRAMENTO DO BLOCO D
  TRegistroD990 = class
  private
    fQTD_LIN_D         : Integer;         //02	QTD_LIN_D	Quantidade total de linhas do Bloco D	N	-	-
  public
    property QTD_LIN_D : Integer read FQTD_LIN_D write FQTD_LIN_D;
  end;

implementation

{TRegistroD010}

constructor TRegistroD001.Create;
begin
  inherited Create;
  FRegistroD010 := TRegistroD010List.Create;
end;

destructor TRegistroD001.Destroy;
begin
  FRegistroD010.Free;
  inherited;
end;

function TRegistroD010List.GetItem(Index: Integer): TRegistroD010;
begin
  Result := TRegistroD010(Inherited Items[Index]);
end;

function TRegistroD010List.New: TRegistroD010;
begin
  Result := TRegistroD010.Create;
  Add(Result);
end;

procedure TRegistroD010List.SetItem(Index: Integer; const Value: TRegistroD010);
begin
  Put(Index, Value);
end;

{ TRegistroD010 }

constructor TRegistroD010.Create;
begin
  FRegistroD100 := TRegistroD100List.Create;
  FRegistroD200 := TRegistroD200List.Create;
  FRegistroD300 := TRegistroD300List.Create;
  FRegistroD350 := TRegistroD350List.Create;
  FRegistroD500 := TRegistroD500List.Create;
  FRegistroD600 := TRegistroD600List.Create;
end;

destructor TRegistroD010.Destroy;
begin
  FRegistroD100.Free;
  FRegistroD200.Free;
  FRegistroD300.Free;
  FRegistroD350.Free;
  FRegistroD500.Free;
  FRegistroD600.Free;
  inherited;
end;

{TRegistroD100}

function TRegistroD100List.GetItem(Index: Integer): TRegistroD100;
begin
  Result := TRegistroD100(Inherited Items[Index]);
end;

function TRegistroD100List.New: TRegistroD100;
begin
  Result := TRegistroD100.Create;
  Add(Result);
end;

procedure TRegistroD100List.SetItem(Index: Integer; const Value: TRegistroD100);
begin
  Put(Index, Value);
end;

{ TRegistroD100 }

constructor TRegistroD100.Create;
begin
  FRegistroD101 := TRegistroD101List.Create;
  FRegistroD105 := TRegistroD105List.Create;
  FRegistroD111 := TRegistroD111List.Create;
end;

destructor TRegistroD100.Destroy;
begin
  FRegistroD101.Free;
  FRegistroD105.Free;
  FRegistroD111.Free;
  inherited;
end;

{TRegistroD101}

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

{TRegistroD105}

function TRegistroD105List.GetItem(Index: Integer): TRegistroD105;
begin
  Result := TRegistroD105(Inherited Items[Index]);
end;

function TRegistroD105List.New: TRegistroD105;
begin
  Result := TRegistroD105.Create;
  Add(Result);
end;

procedure TRegistroD105List.SetItem(Index: Integer; const Value: TRegistroD105);
begin
  Put(Index, Value);
end;

{TRegistroD111}

function TRegistroD111List.GetItem(Index: Integer): TRegistroD111;
begin
  Result := TRegistroD111(Inherited Items[Index]);
end;

function TRegistroD111List.New: TRegistroD111;
begin
  Result := TRegistroD111.Create;
  Add(Result);
end;

procedure TRegistroD111List.SetItem(Index: Integer; const Value: TRegistroD111);
begin
  Put(Index, Value);
end;

{TRegistroD200}

function TRegistroD200List.GetItem(Index: Integer): TRegistroD200;
begin
  Result := TRegistroD200(Inherited Items[Index]);
end;

function TRegistroD200List.New: TRegistroD200;
begin
  Result := TRegistroD200.Create;
  Add(Result);
end;

procedure TRegistroD200List.SetItem(Index: Integer; const Value: TRegistroD200);
begin
  Put(Index, Value);
end;

{ TRegistroD200 }

constructor TRegistroD200.Create;
begin
  FRegistroD201 := TRegistroD201List.Create;
  FRegistroD205 := TRegistroD205List.Create;
  FRegistroD209 := TRegistroD209List.Create;
end;

destructor TRegistroD200.Destroy;
begin
  FRegistroD201.Free;
  FRegistroD205.Free;
  FRegistroD209.Free;
  inherited;
end;

{TRegistroD201}

function TRegistroD201List.GetItem(Index: Integer): TRegistroD201;
begin
  Result := TRegistroD201(Inherited Items[Index]);
end;

function TRegistroD201List.New: TRegistroD201;
begin
  Result := TRegistroD201.Create;
  Add(Result);
end;

procedure TRegistroD201List.SetItem(Index: Integer; const Value: TRegistroD201);
begin
  Put(Index, Value);
end;

{TRegistroD205}

function TRegistroD205List.GetItem(Index: Integer): TRegistroD205;
begin
  Result := TRegistroD205(Inherited Items[Index]);
end;

function TRegistroD205List.New: TRegistroD205;
begin
  Result := TRegistroD205.Create;
  Add(Result);
end;

procedure TRegistroD205List.SetItem(Index: Integer; const Value: TRegistroD205);
begin
  Put(Index, Value);
end;

{TRegistroD209}

function TRegistroD209List.GetItem(Index: Integer): TRegistroD209;
begin
  Result := TRegistroD209(Inherited Items[Index]);
end;

function TRegistroD209List.New: TRegistroD209;
begin
  Result := TRegistroD209.Create;
  Add(Result);
end;

procedure TRegistroD209List.SetItem(Index: Integer; const Value: TRegistroD209);
begin
  Put(Index, Value);
end;

{TRegistroD300}

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

{ TRegistroD300 }

constructor TRegistroD300.Create;
begin
  FRegistroD309 := TRegistroD309List.Create;
end;

destructor TRegistroD300.Destroy;
begin
  FRegistroD309.Free;
  inherited;
end;

{TRegistroD309}

function TRegistroD309List.GetItem(Index: Integer): TRegistroD309;
begin
  Result := TRegistroD309(Inherited Items[Index]);
end;

function TRegistroD309List.New: TRegistroD309;
begin
  Result := TRegistroD309.Create;
  Add(Result);
end;

procedure TRegistroD309List.SetItem(Index: Integer; const Value: TRegistroD309);
begin
  Put(Index, Value);
end;

{TRegistroD350}

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

{ TRegistroD350 }

constructor TRegistroD350.Create;
begin
  FRegistroD359 := TRegistroD359List.Create;
end;

destructor TRegistroD350.Destroy;
begin
  FRegistroD359.Free;
  inherited;
end;

{TRegistroD359}

function TRegistroD359List.GetItem(Index: Integer): TRegistroD359;
begin
  Result := TRegistroD359(Inherited Items[Index]);
end;

function TRegistroD359List.New: TRegistroD359;
begin
  Result := TRegistroD359.Create;
  Add(Result);
end;

procedure TRegistroD359List.SetItem(Index: Integer; const Value: TRegistroD359);
begin
  Put(Index, Value);
end;

{TRegistroD500}

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

{ TRegistroD500 }

constructor TRegistroD500.Create;
begin
  FRegistroD501 := TRegistroD501List.Create;
  FRegistroD505 := TRegistroD505List.Create;
  FRegistroD509 := TRegistroD509List.Create;
end;

destructor TRegistroD500.Destroy;
begin
  FRegistroD501.Free;
  FRegistroD505.Free;
  FRegistroD509.Free;
  inherited;
end;

{TRegistroD501}

function TRegistroD501List.GetItem(Index: Integer): TRegistroD501;
begin
  Result := TRegistroD501(Inherited Items[Index]);
end;

function TRegistroD501List.New: TRegistroD501;
begin
  Result := TRegistroD501.Create;
  Add(Result);
end;

procedure TRegistroD501List.SetItem(Index: Integer; const Value: TRegistroD501);
begin
  Put(Index, Value);
end;

{TRegistroD505}

function TRegistroD505List.GetItem(Index: Integer): TRegistroD505;
begin
  Result := TRegistroD505(Inherited Items[Index]);
end;

function TRegistroD505List.New: TRegistroD505;
begin
  Result := TRegistroD505.Create;
  Add(Result);
end;

procedure TRegistroD505List.SetItem(Index: Integer; const Value: TRegistroD505);
begin
  Put(Index, Value);
end;

{TRegistroD509}

function TRegistroD509List.GetItem(Index: Integer): TRegistroD509;
begin
  Result := TRegistroD509(Inherited Items[Index]);
end;

function TRegistroD509List.New: TRegistroD509;
begin
  Result := TRegistroD509.Create;
  Add(Result);
end;

procedure TRegistroD509List.SetItem(Index: Integer; const Value: TRegistroD509);
begin
  Put(Index, Value);
end;

{TRegistroD600}

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

{ TRegistroD600 }

constructor TRegistroD600.Create;
begin
  FRegistroD601 := TRegistroD601List.Create;
  FRegistroD605 := TRegistroD605List.Create;
  FRegistroD609 := TRegistroD609List.Create;
end;

destructor TRegistroD600.Destroy;
begin
  FRegistroD601.Free;
  FRegistroD605.Free;
  FRegistroD609.Free;
  inherited;
end;

{TRegistroD601}

function TRegistroD601List.GetItem(Index: Integer): TRegistroD601;
begin
  Result := TRegistroD601(Inherited Items[Index]);
end;

function TRegistroD601List.New: TRegistroD601;
begin
  Result := TRegistroD601.Create;
  Add(Result);
end;

procedure TRegistroD601List.SetItem(Index: Integer; const Value: TRegistroD601);
begin
  Put(Index, Value);
end;

{TRegistroD605}

function TRegistroD605List.GetItem(Index: Integer): TRegistroD605;
begin
  Result := TRegistroD605(Inherited Items[Index]);
end;

function TRegistroD605List.New: TRegistroD605;
begin
  Result := TRegistroD605.Create;
  Add(Result);
end;

procedure TRegistroD605List.SetItem(Index: Integer; const Value: TRegistroD605);
begin
  Put(Index, Value);
end;

{TRegistroD609}

function TRegistroD609List.GetItem(Index: Integer): TRegistroD609;
begin
  Result := TRegistroD609(Inherited Items[Index]);
end;

function TRegistroD609List.New: TRegistroD609;
begin
  Result := TRegistroD609.Create;
  Add(Result);
end;

procedure TRegistroD609List.SetItem(Index: Integer; const Value: TRegistroD609);
begin
  Put(Index, Value);
end;

end.
