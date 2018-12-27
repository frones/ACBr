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
|* 23/08/2013: Juliana Tamizou
|*  - Distribuição da Primeira Versao
*******************************************************************************}
{$I ACBr.inc}

unit ACBrSEF2_Bloco0;

interface

Uses SysUtils, Classes ,ACBrSEF2Conversao;

type

  TRegistroSEF0001 = class;
  TRegistroSEF0205List = class;
  TRegistroSEF0215List = class;
  TRegistroSEF0400List = class;
  TRegistroSEF0450List = class;
  TRegistroSEF0460List = class;
  TRegistroSEF0465List = class;
  TRegistroSEF0470List = class;

 //LINHA 0000: ABERTURA DO ARQUIVO DIGITAL E IDENTIFICAÇÃO DO CONTRIBUINTE
  TRegistroSEF0000 = class
  private
    fNIRE: String;
    fCPF : String;
    fIE  : String;
    fUF  : String;
    fIM  : String;
    fCNPJ: String;
    fPAIS: String;

    fCOD_MUN  : String;
    fSUFRAMA  : String;
    fNOME_EMPR: String;
    fFANTASIA : String;

    fDT_INI: TDateTime;
    fDT_FIN: TDateTime;

    fCOD_FIN: TSEFIICodFinalidade;
    fCOD_CTD: TSEFIIConteudArquivo;
    fCOD_VER: TSEFIIVersaoLeiaute;
  public
    property DT_INI    : TDateTime	read fDT_INI write fDT_INI;            //Data inicial das informações contidas no arquivo	N	8	-
    property DT_FIN    : TDateTime	read fDT_FIN write fDT_FIN;            //Data final das informações contidas no arquivo	N	8	-
    property CNPJ : String read	fCNPJ write fCNPJ;                         //CNPJ do contribuinte (vedado informar CPF)	N	14	-
    property UF   : String read	fUF   write fUF;                           //Sigla da unidade da Federação do domicílio fiscal do contribuinte	C	2	-
    property IE   : String read	fIE   write fIE;                           //Inscrição estadual do contribuinte	C	-	-
    property IM   : String read	fIM   write fIM;                           //Inscrição municipal do contribuinte	C	-	-
    property CPF  : String read fCPF  write fCPF;                          //CPF do contribuinte (vedado informar CNPJ)	N	11	-
    property PAIS : String read	fPAIS write fPAIS;
    property NIRE	: String read fNIRE write fNIRE;                         //Número de Identificação do Registro de Empresas da Junta Comercial	N	11	-
    property NOME_EMPR: String	read FNOME_EMPR write FNOME_EMPR;          //Nome empresarial do contribuinte	C	-	-
    property FANTASIA : String read FFANTASIA write FFANTASIA;             //Nome de fantasia associado ao nome empresarial	C	-	-
    property COD_MUN  : String	read FCOD_MUN  write FCOD_MUN;             //Código do município do domicílio fiscal do contribuinte, conforme a tabela externa indicada no item 3.3.1	N	7	-
    property SUFRAMA  : String	read FSUFRAMA  write FSUFRAMA;             //Inscrição do contribuinte na Suframa	C	9	-
    property COD_VER : TSEFIIVersaoLeiaute read FCOD_VER write FCOD_VER; 	 //Código da versão do leiaute, de acordo com o item 3.1.1	N	4	-
    property COD_FIN : TSEFIICodFinalidade read	FCOD_FIN write FCOD_FIN;   //Código da finalidade do arquivo, conforme a tabela indicada no item 3.2.1	N	1	-
    property COD_CTD : TSEFIIConteudArquivo	read FCOD_CTD write FCOD_CTD;  //Código do conteúdo do arquivo, conforme a tabela indicada no item 3.2.2	N	2	-
  end;

  TRegistroSEF0005 = class
  private
    fCPF_RESP : String;
    fCEP      : String;
    fCEP_CP   : String;
    fCP       : String;
    fNUM      : String;
    fENDERECO : String;
    fCOMPL    : String;
    fBAIRRO   : String;
    fNOME_RESP: String;
    fEMAIL    : String;
    fFAX      : String;
    fFONE     : String;
    fCOD_ASSIN : TSEFIIQualiAssinante;
  public
    constructor Create(AOwner: TRegistroSEF0001); virtual; /// Create
    property NOME_RESP : String	read FNOME_RESP write fNOME_RESP;  // Nome do responsável	C	-
    property CPF_RESP  : String read fCPF_RESP  write fCPF_RESP;   // CPF do responsável	N	11
    property CEP       : String read fCEP       write fCEP;        // Código de Endereçamento Postal	N	8
    property NUM       : String	read fNUM       write fNUM;        // Número do imóvel	C	-
    property COMPL     : String	read fCOMPL     write fCOMPL;      // Dados complementares do endereço	C	-
    property BAIRRO    : String	read fBAIRRO    write fBAIRRO;     // Bairro em que o imóvel está situado	C	-
    property CEP_CP    : String read fCEP_CP    write fCEP_CP;     // Código de Endereçamento Postal da caixa postal	N	8
    property CP        : String read fCP        write fCP;         //	Caixa postal	N	-
    property FONE      : String	read fFONE      write fFONE;       // Número do telefone	C	-
    property FAX       : String	read fFAX       write fFAX;        // Número do fax	C	-
    property EMAIL     : String	read fEMAIL     write fEMAIL;      // Endereço do correio eletrônico	C	-
    property ENDERECO  : String	read fENDERECO  write fENDERECO;   // Endereço do imóvel	C	-
    property COD_ASSIN : TSEFIIQualiAssinante	read fCOD_ASSIN write fCOD_ASSIN; // Código de qualificação do assinante, conforme a tabela externa indicada no item 3.3.1	N	3
  end;

  //LINHA 0025: BENEFÍCIO FISCAL
  TRegistroSEF0025 = class
  private
    fCOD_BF_ICMS: TSEFIIBeniFiscalICMS;
    fCOD_BF_ISS : String;
  public
    constructor Create(AOwner: TRegistroSEF0001); virtual; /// Create
    property COD_BF_ICMS: TSEFIIBeniFiscalICMS read fCOD_BF_ICMS write fCOD_BF_ICMS;	// Código do benefício fiscal do ICMS, conforme a tabela indicada no item 6.1.1
    property COD_BF_ISS : String read fCOD_BF_ISS write fCOD_BF_ISS;	                // Código do benefício fiscal do ISS, conforme a tabela indicada no item 6.1.2
  end;

  //LINHA 0030: PERFIL DO CONTRIBUINTE
  TRegistroSEF0030 = class
  private
    fIND_ARQ : TIndicadorDocArquivo;
    fIND_ED  : TIndicadorDados;
    fIND_EC  : TIndicadorEscrContabil;
    fPRF_ISS : TIndicadorExigeEscrImposto;
    fPRF_LMC : TIndicadorExigeDiversas;
    fPRF_RI  : TIndicadorExigeDiversas;
    fPRF_RIDF: TIndicadorExigeDiversas;
    fPRF_RUDF: TIndicadorExigeDiversas;
    fPRF_RV  : TIndicadorExigeDiversas;
    fIND_ICMS: TIndicadorExigeDiversas;
    fIND_IPI : TIndicadorExigeDiversas;
    fIND_ISS : TIndicadorExigeDiversas;
    fIND_RT  : TIndicadorExigeDiversas;
    fIND_ST  : TIndicadorExigeDiversas;
    fIND_AT  : TIndicadorExigeDiversas;
    fPRF_ICMS: TIndicadorExigeEscrImposto;
    fIND_RI  : TIndicadorExigeDiversas;
  public
    constructor Create(AOwner: TRegistroSEF0001); virtual; /// Create
    property IND_ED   : TIndicadorDados         read fIND_ED   write fIND_ED;
    property IND_ARQ  : TIndicadorDocArquivo    read fIND_ARQ  write fIND_ARQ;
    property PRF_RIDF : TIndicadorExigeDiversas read fPRF_RIDF write fPRF_RIDF;
    property PRF_RUDF : TIndicadorExigeDiversas read fPRF_RUDF write fPRF_RUDF;
    property PRF_LMC  : TIndicadorExigeDiversas read fPRF_LMC  write fPRF_LMC;
    property PRF_RV   : TIndicadorExigeDiversas read fPRF_RV   write fPRF_RV;
    property PRF_RI   : TIndicadorExigeDiversas read fPRF_RI   write fPRF_RI;
    property IND_EC   : TIndicadorEscrContabil  read fIND_EC   write fIND_EC;
    property IND_ISS  : TIndicadorExigeDiversas read fIND_ISS  write fIND_ISS;
    property IND_RT   : TIndicadorExigeDiversas read fIND_RT   write fIND_RT;
    property IND_ICMS : TIndicadorExigeDiversas read fIND_ICMS write fIND_ICMS;
    property IND_ST   : TIndicadorExigeDiversas read fIND_ST   write fIND_ST;
    property IND_AT   : TIndicadorExigeDiversas read fIND_AT   write fIND_AT;
    property IND_IPI  : TIndicadorExigeDiversas read fIND_IPI  write fIND_IPI;
    property IND_RI   : TIndicadorExigeDiversas read fIND_RI   write fIND_RI;
    property PRF_ISS  : TIndicadorExigeEscrImposto read fPRF_ISS  write fPRF_ISS;
    property PRF_ICMS : TIndicadorExigeEscrImposto read fPRF_ICMS write fPRF_ICMS;
  end;

  //LINHA 0100: CONTABILISTA
  TRegistroSEF0100 = Class
  private
    fCEP_CP   : Integer;
    fCOD_MUN  : Integer;
    fCP       : Integer;
    fCEP      : String;
    fCPF      : String;
    fUF       : String;
    fNOME     : String;
    fENDERECO : String;
    fCOMPL    : String;
    fNUM      : String;
    fBAIRRO   : String;
    fFAX      : String;
    fEMAIL    : String;
    fCRC      : String;
    fFONE     : String;
    fCNPJ     : String;
    fCOD_ASSIN: TSEFIIQualiAssinante;
  public
    constructor Create(AOwner: TRegistroSEF0001); virtual; /// Create
    property NOME    : String  read fNOME     write fNOME;     // Nome do contabilista/escritório de contabilidade	C
    property CNPJ    : String  read fCNPJ     write	fCNPJ;     // CNPJ do escritório de contabilidade	N
    property CPF     : String  read fCPF	    write fCPF;      // CPF do contabilista	N
    property CRC     : String  read fCRC	    write fCRC;      // CRC do contabilista	C
    property CEP     : String  read fCEP	    write fCEP;      // Código de Endereçamento Postal	N
    property NUM     : String  read fNUM	    write fNUM;      // Número do imóvel	C
    property COMPL   : String  read fCOMPL    write fCOMPL;    // Dados complementares do endereço	C
    property BAIRRO  : String  read fBAIRRO   write fBAIRRO;   // Bairro em que o imóvel está situado	C
    property UF      : String  read fUF	      write fUF;       // Sigla da unidade da Federação do endereço do contabilista/escritório de contabilidade	C
    property CEP_CP  : Integer read fCEP_CP   write fCEP_CP;   // Código de Endereçamento Postal da caixa postal	N
    property CP	     : Integer read fCP       write fCP;       // Caixa postal	N
    property FONE    : String  read fFONE	    write fFONE;     // Número do telefone	C
    property FAX     : String  read fFAX	    write fFAX;      // Número do fax	C
    property EMAIL   : String  read fEMAIL    write fEMAIL;    // Endereço do correio eletrônico	C
    property ENDERECO: String  read fENDERECO	write fENDERECO; // Endereço do imóvel	C
    property COD_MUN : Integer read fCOD_MUN  write	fCOD_MUN;  // Código do município, conforme a tabela externa indicada no item 3.3.1	N
    property COD_ASSIN : TSEFIIQualiAssinante read fCOD_ASSIN	write FCOD_ASSIN; // Código de qualificação do assinante, conforme a tabela externa indicada no item 3.3.1	N
  end;

  ///LINHA 0150: TABELA DE CADASTRO DO PARTICIPANTE
  TRegistroSEF0150 = class
  private
    fCOD_PAIS: String;
    fCPF     : String;
    fSUFRAMA : String;
    fIE      : String;
    fIM      : String;
    fCNPJ    : String;
    fUF      : String;
    fNOME    : String;
    fIE_ST   : String;
    fCOD_PART: String;
    fCOD_MUN : Integer;
  public
    constructor Create(AOwner: TRegistroSEF0001); virtual; /// Create
    property COD_PART: String  read fCOD_PART write fCOD_PART;  // Código próprio de identificação do participante no arquivo	C
    property NOME    : String  read fNOME	    write fNOME;      // Nome pessoal ou empresarial do participante	C
    property COD_PAIS: String  read fCOD_PAIS write fCOD_PAIS;  // Código do país do participante, conforme a tabela externa indicada no item 3.3.1	N
    property CNPJ    : String  read fCNPJ     write fCNPJ;	    // CNPJ do participante	N
    property CPF     : String  read fCPF      write fCPF;	      // CPF do participante	N
    property UF      : String  read fUF       write fUF;       	// Sigla da unidade da Federação do participante	C
    property IE      : String  read fIE       write fIE;	      // Inscrição estadual do participante	C
    property IE_ST   : String  read fIE_ST    write fIE_ST;	    // Inscrição estadual do participante emitente contribuinte-substituto na unidade da Federação do destinatário substituído	C
    property COD_MUN : Integer read fCOD_MUN  write fCOD_MUN;	  // Código do município, conforme a tabela externa indicada no item 3.3.1	N
    property IM      : String  read fIM       write fIM;	      // Inscrição municipal do participante	C
    property SUFRAMA : String  read fSUFRAMA  write fSUFRAMA;	  // Número de inscrição do participante na Suframa	C
  end;

  // Registro 0150 - Lista
  TRegistroSEF0150List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEF0150;
    procedure SetItem(Index: Integer; const Value: TRegistroSEF0150);
  public
    function New(AOwner: TRegistroSEF0001): TRegistroSEF0150;
    function LocalizaRegistro(const pCOD_PART: String): boolean;
    property Items[Index: Integer]: TRegistroSEF0150 read GetItem write SetItem;
  end;

  TRegistroSEF0200 = class
  private
    FCOD_ITEM    : String;
    FDESCR_ITEM  : String;
    FCOD_GEN     : String;
    FCOD_LST     : String;
    FRegistro0205: TRegistroSEF0205List;
    FRegistro0215: TRegistroSEF0215List;
  public
    constructor Create(AOwner: TRegistroSEF0001); virtual; /// Create
    destructor Destroy; override; /// Destroy
    property COD_ITEM    : String  read fCOD_ITEM   write fCOD_ITEM;
    property DESCR_ITEM  : String  read fDESCR_ITEM write fDESCR_ITEM;
    property COD_GEN     : String  read fCOD_GEN    write fCOD_GEN;
    property COD_LST     : String  read fCOD_LST    write fCOD_LST;
    property Registro0205: TRegistroSEF0205List read FRegistro0205 write FRegistro0205;
    property Registro0215: TRegistroSEF0215List read FRegistro0215 write FRegistro0215;
  end;

  // Registro 0200 - Lista
  TRegistroSEF0200List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEF0200;
    procedure SetItem(Index: Integer; const Value: TRegistroSEF0200);
  public
    function New(AOwner: TRegistroSEF0001): TRegistroSEF0200;
    function LocalizaRegistro(const pCOD_ITEM: String): boolean;
    property Items[Index: Integer]: TRegistroSEF0200 read GetItem write SetItem;
  end;

  //LINHA 0001: ABERTURA DO BLOCO 0
  TRegistroSEF0001 = class(TOpenBlocos)
  private
    fRegistro0005: TRegistroSEF0005;
    fRegistro0025: TRegistroSEF0025;
    fRegistro0030: TRegistroSEF0030;
    fRegistro0100: TRegistroSEF0100;
    fRegistro0150: TRegistroSEF0150List;
    fRegistro0200: TRegistroSEF0200List;
    fRegistro0400: TRegistroSEF0400List;
    fRegistro0450: TRegistroSEF0450List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy
    property Registro005 : TRegistroSEF0005 read fRegistro0005 write fRegistro0005;
    property Registro025 : TRegistroSEF0025 read fRegistro0025 write fRegistro0025;
    property Registro030 : TRegistroSEF0030 read fRegistro0030 write fRegistro0030;
    property Registro0100: TRegistroSEF0100 read fRegistro0100 write fRegistro0100;
    property Registro0150: TRegistroSEF0150List read fRegistro0150 write fRegistro0150;
    property Registro0200: TRegistroSEF0200List read fRegistro0200 write fRegistro0200;
    property Registro0400: TRegistroSEF0400List read fRegistro0400 write fRegistro0400;
    property Registro0450: TRegistroSEF0450List read fRegistro0450 write fRegistro0450;
  end;

  TRegistroSEF0205 = class
  private
    fCOD_ITEM_ANT  : Integer;
    fDESCR_ITEM_ANT: Integer;

    fDT_INI: TDateTime;
    fDT_FIN: TDateTime;
  public
    constructor Create(AOwner: TRegistroSEF0200); virtual; /// Create
    property COD_ITEM_ANT   : Integer read fCOD_ITEM_ANT   write fCOD_ITEM_ANT;
    property DESCR_ITEM_ANT : Integer read FDESCR_ITEM_ANT write fDESCR_ITEM_ANT;
    property DT_INI : TDateTime read fDT_INI write fDT_INI;
    property DT_FIN : TDateTime read fDT_FIN write fDT_FIN;
  end;

  // Registro 0205 - Lista
  TRegistroSEF0205List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEF0205;
    procedure SetItem(Index: Integer; const Value: TRegistroSEF0205);
  public
    function New(AOwner: TRegistroSEF0200): TRegistroSEF0205;
    property Items[Index: Integer]: TRegistroSEF0205 read GetItem write SetItem;
  end;

  TRegistroSEF0215 = class
  private
    fCOD_ITEM_ANP: string;
    fDT_INI: TDateTime;
    fDT_FIN: TDateTime;
  public
    constructor Create(AOwner: TRegistroSEF0200); virtual; /// Create
    property COD_ITEM_ANP   : string read fCOD_ITEM_ANP   write fCOD_ITEM_ANP;
    property DT_INI : TDateTime read fDT_INI write fDT_INI;
    property DT_FIN : TDateTime read fDT_FIN write fDT_FIN;
  end;

  // Registro 0215 - Lista
  TRegistroSEF0215List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEF0215;
    procedure SetItem(Index: Integer; const Value: TRegistroSEF0215);
  public
    function New(AOwner: TRegistroSEF0200): TRegistroSEF0215;
    property Items[Index: Integer]: TRegistroSEF0215 read GetItem write SetItem;
  end;

  //REGISTRO 0400: TABELA DE NATUREZA DA OPERAÇÃO/PRESTAÇÃO
  TRegistroSEF0400 = class
  private
    fCOD_NAT     : String; //Código da natureza da operação/prestação
    fDESCR_NAT   : String;
    fCOP         : String;
  public
    constructor Create(AOwner: TRegistroSEF0001); virtual; /// Create
    destructor Destroy; override; /// Destroy
    property COD_NAT     : String read fCOD_NAT   write fCOD_NAT;
    property DESCR_NAT   : String read fDESCR_NAT write fDESCR_NAT;
    property COP         : String read fCOP       write fCOP;
  end;

  // Registro 0400 - Lista
  TRegistroSEF0400List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEF0400;
    procedure SetItem(Index: Integer; const Value: TRegistroSEF0400);
  public
    function New(AOwner: TRegistroSEF0001): TRegistroSEF0400;
    function LocalizaRegistro(const pCOD_NAT: String): boolean;
    property Items[Index: Integer]: TRegistroSEF0400 read GetItem write SetItem;
  end;

  //LINHA 0450: TABELA DE INFORMAÇÕES COMPLEMENTARES/REGISTRO DE OBSERVAÇÕES
  TRegistroSEF0450 = class
  private
    fCOD_INF : String; //Código da informação complementar do documento fiscal.
    fTXT     : String;
    fRegistro0460: TRegistroSEF0460List;
    fRegistro0465: TRegistroSEF0465List;
    fRegistro0470: TRegistroSEF0470List;
  public
    constructor Create(AOwner: TRegistroSEF0001); virtual; /// Create
    property COD_INF : String read fCOD_INF write fCOD_INF;
    property TXT     : String read fTXT     write fTXT;
    property Registro0460: TRegistroSEF0460List read fRegistro0460 write fRegistro0460;
    property Registro0465: TRegistroSEF0465List read fRegistro0465 write fRegistro0465;
    property Registro0470: TRegistroSEF0470List read fRegistro0470 write fRegistro0470;
  end;

  // Registro 0450 - Lista
  TRegistroSEF0450List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEF0450;
    procedure SetItem(Index: Integer; const Value: TRegistroSEF0450);
  public
    function New(AOwner: TRegistroSEF0001): TRegistroSEF0450;
    property Items[Index: Integer]: TRegistroSEF0450 read GetItem write SetItem;
  end;

  TRegistroSEF0460 = class
  private
    fVL_MOR  : Currency;
    fVL_PGTO : Currency;
    fVL_DESC : Currency;
    fVL_DA   : Currency;
    fVL_JRS  : Currency;
    fVL_MUL  : Currency;
    fCOD_MUN : Integer;
    fPER_REF : Integer;
    fDESCR_DA: String;
    fNUM_DA  : String;
    fUF      : String;
    fAUT_BCO : String;
    fDT_PGTO : TDateTime;
    fDT_VCTO : TDateTime;
    fIND_DA  : TSEFIIIndicadorDocArregadacao;
  public
    constructor Create(AOwner: TRegistroSEF0450); virtual; /// Create
    property AIND_DA	: TSEFIIIndicadorDocArregadacao read fIND_DA write FIND_DA; // "Indicador do documento de arrecadação:
    property DESCR_DA : String    read fDESCR_DA write fDESCR_DA;                 // Descrição complementar do documento de arrecadação	C
    property UF       : String    read fUF       write fUF;	                      // Unidade da Federação de destino do pagamento	C
    property COD_MUN  : Integer   read fCOD_MUN  write fCOD_MUN;	               	// Código do município de destino do pagamento, conforme a tabela externa indicada no item 3.3.1	N
    property PER_REF  : Integer   read fPER_REF  write fPER_REF;		              // Período fiscal de referência	N
    property NUM_DA   : String    read fNUM_DA   write fNUM_DA;	                  // Número do documento de arrecadação	C
    property VL_DA    : Currency  read fVL_DA    write fVL_DA;                   	// Valor do documento de arrecadação	N
    property DT_VCTO  : TDateTime read fDT_VCTO  write fDT_VCTO;               		// Data de vencimento do documento de arrecadação	N
    property VL_DESC  : Currency  read fVL_DESC  write fVL_DESC;                  // Valor dos descontos	N
    property VL_MOR   : Currency  read fVL_MOR   write fVL_MOR;	                  // Valor da multa de mora	N
    property VL_JRS   : Currency  read fVL_JRS   write fVL_JRS;	                  // Valor dos juros	N
    property VL_MUL   : Currency  read fVL_MUL   write fVL_MUL;	                  // Valor das multas	N
    property VL_PGTO  : Currency  read fVL_PGTO  write fVL_PGTO;	                // Valor do pagamento	N
    property DT_PGTO  : TDateTime read fDT_PGTO  write fDT_PGTO;		              // Data de pagamento do documento de arrecadação	N
    property AUT_BCO  : String    read fAUT_BCO  write fAUT_BCO;	                // Código da autenticação bancária 	C
  end;

  // Registro 0460 - Lista
  TRegistroSEF0460List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEF0460;
    procedure SetItem(Index: Integer; const Value: TRegistroSEF0460);
  public
    function New(AOwner: TRegistroSEF0450): TRegistroSEF0460;
    property Items[Index: Integer]: TRegistroSEF0460 read GetItem write SetItem;
  end;

  //LINHA 0465: DOCUMENTO FISCAL REFERENCIADO
  TRegistroSEF0465 = class
  private
    fVL_RT      : Currency;
    fVL_ISS     : Currency;
    fVL_DOC     : Currency;
    fVL_IPI     : Currency;
    fVL_ICMS_ST : Currency;
    fVOL        : Currency;
    fVL_ICMS    : Currency;
    fVL_AT      : Currency;
    fIE         : String;
    fNUM_DOC    : String;
    fCHV_NFE_CTE: String;
    fIND_EMIT   : TIndiceEmissao;
    fCOD_MUN    : String;
    fCPF        : String;
    fCOD_MOD    : TSEFIIDocFiscalReferenciado;
    fIM         : String;
    fCNPJ       : String;
    fCOD_SIT    : TCodigoSituacao;
    fIND_OPER   : TIndiceOperacao;
    fSER        : String;
    fSUB        : String;
    fUF         : String;
    fDT_DOC     : TDateTime;
  public
    constructor Create(AOwner: TRegistroSEF0450); virtual; /// Create
    property IND_OPER    : TIndiceOperacao	read FIND_OPER   write FIND_OPER;    // "Indicador de operação:0- Entrada ou aquisição1- Saída ou prestação"
    property IND_EMIT    : TIndiceEmissao read FIND_EMIT   write FIND_EMIT;  	 // "Indicador de emitente:0- Emissão própria1- Emissão por terceiros"
    property CNPJ        : String read FCNPJ        write FCNPJ;         //	CNPJ : String do contribuinte
    property CPF         : String read FCPF         write FCPF;	         // CPF do contribuinte
    property UF          : String read FUF          write FUF;           // Sigla da unidade da Federação do contribuinte
    property IE          : String read FIE          write FIE;	         // Inscrição estadual do contribuinte
    property COD_MUN     : String	read FCOD_MUN     write FCOD_MUN;      // Código do município do contribuinte, conforme a tabela externa indicada no item 3.3.1
    property IM          : String read FIM          write FIM;	         // Inscrição municipal do contribuinte
    property COD_MOD     : TSEFIIDocFiscalReferenciado	read FCOD_MOD     write FCOD_MOD;      // Código do documento fiscal, conforme a tabela indicada no item 4.1.1 ou no item 4.1.2
    property COD_SIT     : TCodigoSituacao	read FCOD_SIT     write FCOD_SIT;      // Código da situação do documento fiscal, conforme a tabela indicada no item 4.1.3
    property SER         : String	read FSER         write FSER;          // Série do documento fiscal
    property SUB         : String	read FSUB         write FSUB;          // Subsérie do documento fiscal
    property CHV_NFE_CTE : String	read FCHV_NFE_CTE write FCHV_NFE_CTE;  // Chave de acesso da NF-e/CT-e (somente para os documentos código 55 ou 57)
    property NUM_DOC     : String	read FNUM_DOC     write FNUM_DOC;      // Número do documento fiscal
    property DT_DOC      : TDateTime read FDT_DOC     write FDT_DOC;     // Na entrada ou aquisição: data da entrada da mercadoria, da aquisição do serviço, do desembaraço aduaneiro ou do lançamento no livro correspondente; na saída ou prestação: data da emissão do documento fiscal, da saída da mercadoria ou do lançamento no livro correspondente
    property VL_DOC      : Currency  read FVL_DOC     write FVL_DOC;	   // Valor do documento fiscal
    property VL_ISS      : Currency  read FVL_ISS     write FVL_ISS;	   // Valor do ISS
    property VL_RT       : Currency  read FVL_RT      write FVL_RT;	     // Valor do ISS retido
    property VL_ICMS     : Currency	 read FVL_ICMS    write FVL_ICMS;    // Valor do ICMS
    property VL_ICMS_ST  : Currency  read FVL_ICMS_ST write FVL_ICMS_ST; // Valor do ICMS da substituição tributária
    property VL_AT       : Currency	 read FVL_AT      write FVL_AT;      // Valor do ICMS da antecipação tributária, nas entradas
    property VL_IPI      : Currency  read FVL_IPI     write FVL_IPI;	   // Valor do IPI
    property VOL         : Currency  read FVOL        write FVOL;	       // Volume transportado
  end;

  // Registro 0465 - Lista
  TRegistroSEF0465List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEF0465;
    procedure SetItem(Index: Integer; const Value: TRegistroSEF0465);
  public
    function New(AOwner: TRegistroSEF0450): TRegistroSEF0465;
    property Items[Index: Integer]: TRegistroSEF0465 read GetItem write SetItem;
  end;

  // LINHA 0470: CUPOM FISCAL REFERENCIADO
  TRegistroSEF0470 = class
  private
    fVL_DOC : Currency;
    fVL_ISS : Currency;
    fVL_ICMS: Currency;
    fCRO    : Integer;
    fNUM_DOC: Integer;
    fCRZ    : Integer;
    fECF_CX : Integer;
    fECF_FAB: String;
    fDT_DOC : TDateTime;
    fCOD_MOD: TSEFIIDocFiscalReferenciado;
  public
    constructor Create(AOwner: TRegistroSEF0450); virtual; /// Create
    property COD_MOD : TSEFIIDocFiscalReferenciado read fCOD_MOD write fCOD_MOD;
    property ECF_FAB : String    read fECF_FAB write fECF_FAB;
    property ECF_CX  : Integer   read fECF_CX  write fECF_CX;
    property CRO     : Integer   read fCRO     write fCRO;
    property CRZ     : Integer   read fCRZ     write fCRZ;
    property NUM_DOC : Integer   read fNUM_DOC write fNUM_DOC;
    property DT_DOC  : TDateTime read fDT_DOC  write fDT_DOC;
    property VL_DOC  : Currency  read fVL_DOC  write fVL_DOC;
    property VL_ISS  : Currency  read fVL_ISS  write fVL_ISS;
    property VL_ICMS : Currency  read fVL_ICMS write fVL_ICMS;
  end;

  // Registro 0470 - Lista
  TRegistroSEF0470List = class(TACBrSEFIIRegistros)
  private
    function GetItem(Index: Integer): TRegistroSEF0470;
    procedure SetItem(Index: Integer; const Value: TRegistroSEF0470);
  public
    function New(AOwner: TRegistroSEF0450): TRegistroSEF0470;
    property Items[Index: Integer]: TRegistroSEF0470 read GetItem write SetItem;
  end;

  /// Registro 0990 - ENCERRAMENTO DO BLOCO 0

  TRegistroSEF0990 = class
  private
    fQTD_LIN_0: Integer; /// Quantidade total de linhas do Bloco 0
  public
    property QTD_LIN_0: Integer read fQTD_LIN_0 write fQTD_LIN_0;
  end;

implementation

{ TRegistroSEF001 }

constructor TRegistroSEF0001.Create;
begin
   inherited Create;
   FRegistro0005 := TRegistroSEF0005.Create(self);
   FRegistro0030 := TRegistroSEF0030.Create(self);
   FRegistro0100 := TRegistroSEF0100.Create(self);
   FRegistro0150 := TRegistroSEF0150List.Create;
   FRegistro0200 := TRegistroSEF0200List.Create;
   fRegistro0400 := TRegistroSEF0400List.Create;
   fRegistro0450 := TRegistroSEF0450List.Create;

   IND_MOV := icContConteudo;
end;

destructor TRegistroSEF0001.Destroy;
begin
   FRegistro0005.Free;
   if (Assigned(FRegistro0025)) then FRegistro0025.Free;
   FRegistro0030.Free;
   FRegistro0100.Free;
   FRegistro0150.Free;
   FRegistro0200.Free;
   fRegistro0400.Free;
   fRegistro0450.Free;
   inherited;
end;

{ TRegistroSEF005 }

constructor TRegistroSEF0005.Create(AOwner: TRegistroSEF0001);
begin

end;

{ TRegistroSEF0025 }

constructor TRegistroSEF0025.Create(AOwner: TRegistroSEF0001);
begin
  AOwner.fRegistro0025 := {$IFNDEF FPC}inherited Create{$ElSE}TRegistroSEF0025.Create(AOwner){$ENDIF};
end;

{ TRegistroSEF0100 }

constructor TRegistroSEF0100.Create(AOwner: TRegistroSEF0001);
begin

end;

{ TRegistroSEF0150List }

function TRegistroSEF0150List.GetItem(Index: Integer): TRegistroSEF0150;
begin
  Result := TRegistroSEF0150(Inherited Items[Index]);
end;


function TRegistroSEF0150List.LocalizaRegistro(const pCOD_PART: String): boolean;
var
intFor: integer;
begin
   Result := false;
   for intFor := 0 to Self.Count - 1 do
   begin
      if Self.Items[intFor].COD_PART = pCOD_PART then
      begin
         Result := true;
         Break;
      end;
   end;
end;

function TRegistroSEF0150List.New(AOwner: TRegistroSEF0001): TRegistroSEF0150;
begin
  Result := TRegistroSEF0150.create(AOwner);
  Add(Result);
end;

procedure TRegistroSEF0150List.SetItem(Index: Integer;
  const Value: TRegistroSEF0150);
begin
  Put(Index, Value);
end;

{ TRegistroSEF0400 }

constructor TRegistroSEF0400.Create(AOwner: TRegistroSEF0001);
begin
end;

destructor TRegistroSEF0400.Destroy;
begin
   inherited;
end;

{ TRegistroSEF0400List }

function TRegistroSEF0400List.GetItem(Index: Integer): TRegistroSEF0400;
begin
  Result := TRegistroSEF0400(Inherited Items[Index]);
end;

function TRegistroSEF0400List.LocalizaRegistro(const pCOD_NAT: String): boolean;
var
intFor: integer;
begin
   Result := false;
   for intFor := 0 to Self.Count - 1 do
   begin
      if Self.Items[intFor].COD_NAT = pCOD_NAT then
      begin
         Result := true;
         Break;
      end;
   end;
end;

function TRegistroSEF0400List.New(AOwner: TRegistroSEF0001): TRegistroSEF0400;
begin
  Result := TRegistroSEF0400.create(AOwner);
  Add(Result);
end;

procedure TRegistroSEF0400List.SetItem(Index: Integer;
  const Value: TRegistroSEF0400);
begin
  Put(Index, Value);
end;

{ TRegistroSEF0450List }

function TRegistroSEF0450List.GetItem(Index: Integer): TRegistroSEF0450;
begin
  Result := TRegistroSEF0450(Get(Index));
end;

function TRegistroSEF0450List.New(AOwner: TRegistroSEF0001): TRegistroSEF0450;
begin
  Result := TRegistroSEF0450.Create(AOwner);
  Add(Result);
end;

procedure TRegistroSEF0450List.SetItem(Index: Integer;
  const Value: TRegistroSEF0450);
begin
  Put(Index, Value);
end;

{ TRegistroSEF0460List }

function TRegistroSEF0460List.GetItem(Index: Integer): TRegistroSEF0460;
begin
  Result := TRegistroSEF0460(Get(Index));
end;

function TRegistroSEF0460List.New(AOwner: TRegistroSEF0450): TRegistroSEF0460;
begin
  Result := TRegistroSEF0460.Create(AOwner);
  Add(Result);
end;

procedure TRegistroSEF0460List.SetItem(Index: Integer;
  const Value: TRegistroSEF0460);
begin
  Put(Index, Value);
end;

{ TRegistroSEF0465 }

constructor TRegistroSEF0465.Create(AOwner: TRegistroSEF0450);
begin

end;

{ TRegistroSEF0465List }

function TRegistroSEF0465List.GetItem(Index: Integer): TRegistroSEF0465;
begin
  Result := TRegistroSEF0465(Get(Index));
end;

function TRegistroSEF0465List.New(AOwner: TRegistroSEF0450): TRegistroSEF0465;
begin
  Result := TRegistroSEF0465.Create(AOwner);
  Add(Result);
end;

procedure TRegistroSEF0465List.SetItem(Index: Integer;
  const Value: TRegistroSEF0465);
begin
  Put(Index, Value);
end;

{ TRegistroSEF0470List }

function TRegistroSEF0470List.GetItem(Index: Integer): TRegistroSEF0470;
begin
  Result := TRegistroSEF0470(Get(Index));
end;

function TRegistroSEF0470List.New(AOwner: TRegistroSEF0450): TRegistroSEF0470;
begin
  Result := TRegistroSEF0470.Create(AOwner);
  Add(Result);
end;

procedure TRegistroSEF0470List.SetItem(Index: Integer;
  const Value: TRegistroSEF0470);
begin
  Put(Index, Value);
end;

{ TRegistroSEF0200 }

constructor TRegistroSEF0200.Create(AOwner: TRegistroSEF0001);
begin
   FRegistro0205 := TRegistroSEF0205List.Create;
end;

destructor TRegistroSEF0200.Destroy;
begin
   FRegistro0205.Free;
   inherited;
end;

{ TRegistroSEF0200List }

function TRegistroSEF0200List.GetItem(Index: Integer): TRegistroSEF0200;
begin
  Result := TRegistroSEF0200(Inherited Items[Index]);
end;

function TRegistroSEF0200List.LocalizaRegistro(const pCOD_ITEM: String): boolean;
var
intFor: integer;
begin
   Result := false;
   for intFor := 0 to Self.Count - 1 do
   begin
      if Self.Items[intFor].COD_ITEM = pCOD_ITEM then
      begin
         Result := true;
         Break;
      end;
   end;
end;

function TRegistroSEF0200List.New(AOwner: TRegistroSEF0001): TRegistroSEF0200;
begin
  Result := TRegistroSEF0200.create(AOwner);
  Add(Result);
end;

procedure TRegistroSEF0200List.SetItem(Index: Integer;
  const Value: TRegistroSEF0200);
begin
  Put(Index, Value);
end;

{ TRegistroSEF0205 }

{ TRegistroSEF0205List }

function TRegistroSEF0205List.GetItem(Index: Integer): TRegistroSEF0205;
begin
  Result := TRegistroSEF0205(Get(Index));
end;

function TRegistroSEF0205List.New(AOwner: TRegistroSEF0200): TRegistroSEF0205;
begin
  Result := TRegistroSEF0205.create(AOwner);
  Add(Result);
end;

procedure TRegistroSEF0205List.SetItem(Index: Integer;
  const Value: TRegistroSEF0205);
begin
  Put(Index, Value);
end;

{ TRegistroSEF0030 }

constructor TRegistroSEF0030.Create(AOwner: TRegistroSEF0001);
begin

end;

{ TRegistroSEF0150 }

constructor TRegistroSEF0150.Create(AOwner: TRegistroSEF0001);
begin

end;

{ TRegistroSEF0205 }

constructor TRegistroSEF0205.Create(AOwner: TRegistroSEF0200);
begin

end;

{ TRegistroSEF0450 }

constructor TRegistroSEF0450.Create(AOwner: TRegistroSEF0001);
begin
   FRegistro0460 := TRegistroSEF0460List.Create;
   FRegistro0465 := TRegistroSEF0465List.Create;
   FRegistro0470 := TRegistroSEF0470List.Create;
end;

{ TRegistroSEF0460 }

constructor TRegistroSEF0460.Create(AOwner: TRegistroSEF0450);
begin

end;

{ TRegistroSEF0470 }

constructor TRegistroSEF0470.Create(AOwner: TRegistroSEF0450);
begin

end;

{ TRegistroSEF0215 }

constructor TRegistroSEF0215.Create(AOwner: TRegistroSEF0200);
begin

end;

{ TRegistroSEF0215List }

function TRegistroSEF0215List.GetItem(Index: Integer): TRegistroSEF0215;
begin
  Result := TRegistroSEF0215(Get(Index));
end;

function TRegistroSEF0215List.New(AOwner: TRegistroSEF0200): TRegistroSEF0215;
begin
  Result := TRegistroSEF0215.create(AOwner);
  Add(Result);
end;

procedure TRegistroSEF0215List.SetItem(Index: Integer;
  const Value: TRegistroSEF0215);
begin
  Put(Index, Value);
end;

end.
