{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou e Isaque Pinheiro               }
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

unit ACBrLFDBloco_0;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrLFDBlocos;

type
  TRegistro0005 = class;
  TRegistro0025List = class;
  TRegistro0030 = class;
  TRegistro0035 = class;
  TRegistro0040 = class;
  TRegistro0045 = class;
  TRegistro0100 = class;
  TRegistro0125 = class;
  TRegistro0120 = class;
  TRegistro0150List = class;
  TRegistro0175List = class;
  TRegistro0200List = class;
  TRegistro0205List = class;
  TRegistro0210List = class;
  TRegistro0215List = class;
  TRegistro0400List = class;
  TRegistro0450List = class;
  TRegistro0455List = class;
  TRegistro0460List = class;
  TRegistro0465List = class;
  TRegistro0470List = class;

  /// Registro 0000 - ABERTURA DO ARQUIVO DIGITAL E IDENTIFICAÇÃO DA ENTIDADE

  { TRegistro0000 }

  TRegistro0000 = class
  private
    FCOD_CONTEUDO: TACBrConteudoArquivo;/// Código do conteúdo do arquivo
    fCOD_VER: TACBrLVersaoLeiaute;       /// Código da versão do leiaute: 100, 101, 102
    fCOD_FIN: TACBrLCodFinalidade;       /// Código da finalidade do arquivo: 0 - Remessa do arquivo original / 1 - Remessa do arquivo substituto.
    fDT_INI: TDateTime;                 /// Data inicial das informações contidas no arquivo
    fDT_FIN: TDateTime;                 /// Data final das informações contidas no arquivo
    FFANTASIA: String;                  /// Nome de fantasia associado ao nome empresarial
    FNIRE: Integer;                     /// Número de Identificação do Registro de Empresas da Junta Comercial
    fNOME: String;                      /// Nome empresarial do contribuinte:
    fCNPJ: String;                      /// Número de inscrição do contribuinte:
    fUF: String;                        /// Sigla da unidade da federação:
    fIE: String;                        /// Inscrição Estadual do contribuinte:
    fCOD_MUN: integer;                  /// Código do município do domicílio fiscal:
    fIM: String;                        /// Inscrição Municipal do contribuinte:
    fSUFRAMA: String;                   /// Número de inscrição do contribuinte:
  public
    property COD_VER: TACBrLVersaoLeiaute read FCOD_VER write FCOD_VER;
    property COD_FIN: TACBrLCodFinalidade read FCOD_FIN write FCOD_FIN;
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
    property NOME: String read FNOME write FNOME;
    property CNPJ: String read FCNPJ write FCNPJ;
    property UF: String read FUF write FUF;
    property IE: String read FIE write FIE;
    property COD_MUN: integer read FCOD_MUN write FCOD_MUN;
    property IM: String read FIM write FIM;
    property SUFRAMA: String read FSUFRAMA write FSUFRAMA;
    property COD_CONTEUDO: TACBrConteudoArquivo read FCOD_CONTEUDO write FCOD_CONTEUDO;
    property FANTASIA: String read FFANTASIA write FFANTASIA;
    property NIRE: Integer read FNIRE write FNIRE;
  end;

  /// Registro 0001 - ABERTURA DO BLOCO 0

  { TRegistro0001 }

  TRegistro0001 = class(TOpenBlocos)
  private
    FRegistro0005: TRegistro0005;
    FRegistro0025: TRegistro0025List;
    FRegistro0030: TRegistro0030;
    FRegistro0120: TRegistro0120;
    FRegistro0200: TRegistro0200List;
    FRegistro0100: TRegistro0100;
    FRegistro0125: TRegistro0125;
    FRegistro0150: TRegistro0150List;
    FRegistro0400: TRegistro0400List;
    FRegistro0460: TRegistro0460List;
    FRegistro0450: TRegistro0450List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property Registro0005: TRegistro0005     read FRegistro0005 write FRegistro0005;
    property Registro0025: TRegistro0025List read FRegistro0025 write FRegistro0025;
    property Registro0030: TRegistro0030     read FRegistro0030 write FRegistro0030;
    property Registro0100: TRegistro0100     read FRegistro0100 write FRegistro0100;
    property Registro0125: TRegistro0125     read FRegistro0125 write FRegistro0125;
    property Registro0120: TRegistro0120     read FRegistro0120 write FRegistro0120;
    property Registro0150: TRegistro0150List read FRegistro0150 write FRegistro0150;
    property Registro0200: TRegistro0200List read FRegistro0200 write FRegistro0200;
    property Registro0400: TRegistro0400List read FRegistro0400 write FRegistro0400;
    property Registro0450: TRegistro0450List read FRegistro0450 write FRegistro0450;
    property Registro0460: TRegistro0460List read FRegistro0460 write FRegistro0460;
  end;

  /// Registro 0005 - DADOS COMPLEMENTARES DO CONTRIBUINTE

  TRegistro0005 = class
  private
    fNOMERESP: String;     // Nome do responsável
    fCOD_ASS: Integer;     // Código de qualificação do assinante
    fCPFRESP:String;       // CPF do responsável
    fCEP: String;          // Código de Endereçamento Postal:
    fCEP_CP: String;       // Código de Endereçamento Postal da caixa postal
    fCP: Integer;          // Caixa postal
    fENDERECO: String;     // Logradouro e endereço do imóvel:
    fNUM: String;          // Número do imóvel:
    fCOMPL: String;        // Dados complementares do endereço:
    fBAIRRO: String;       // Bairro em que o imóvel está situado:
    fFONE: String;         // Número do telefone:
    fFAX: String;          // Número do fax:
    fEMAIL: String;        // Endereço do correio eletrônico:
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create

    property NOMERESP: String read fNOMERESP write fNOMERESP;
    property COD_ASS: Integer read fCOD_ASS write fCOD_ASS;
    property CPFRESP: String read fCPFRESP write fCPFRESP;
    property CEP: String read FCEP write FCEP;
    property ENDERECO: String read FENDERECO write FENDERECO;
    property NUM: String read FNUM write FNUM;
    property COMPL: String read FCOMPL write FCOMPL;
    property BAIRRO: String read FBAIRRO write FBAIRRO;
    property CEP_CP: String read FCEP_CP write FCEP_CP;
    property CP: Integer read FCP write FCP;
    property FONE: String read FFONE write FFONE;
    property FAX: String read FFAX write FFAX;
    property EMAIL: String read FEMAIL write FEMAIL;
  end;


  /// Registro 0025 - DADOS DO CONTRIBUINTE SUBSTITUTO

  { TRegistro0025 }

  TRegistro0025 = class
  private
    fCODBF_ICMS: TACBrCODBFICMS; // Código do benefício fiscal do ICMS
    fCODBF_ISS : TACBrCODBFISS;  // Código do benefício fiscal do ISS
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create

    property CODBF_ICMS: TACBrCODBFICMS read fCODBF_ICMS write fCODBF_ICMS;
    property CODBF_ISS : TACBrCODBFISS  read fCODBF_ISS  write fCODBF_ISS;
  end;


  /// Registro 0025 - Lista

  { TRegistro0025List }

  TRegistro0025List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0025;
    procedure SetItem(Index: Integer; const Value: TRegistro0025);
  public
    function New(AOwner: TRegistro0001): TRegistro0025;
    property Items[Index: Integer]: TRegistro0025 read GetItem write SetItem;
  end;

  /// Registro 0030 - DADOS PERFIL DO CONTRIBUINTE

  { TRegistro0030 }

  TRegistro0030 = class
  private
    fIND_CONTARQ : TACBrConteudoArquivo;             // Indicador de entrada de dados
    fIND_ENTDADOS: TACBrTipoEntradaDados;            // Indicador de conteúdo do arquivo
    fIND_ICMS: Boolean;                              // Indicador de ICMS
    fIND_IPI : Boolean;                              // Indicador de IPI
    fIND_ISS : Boolean;                              // Indicador de ISS
    fIND_LCV : Boolean;                              // Exige Livro de Registro de Veículos
    fIND_LMC : Boolean;                              // Exige Livro de Movimentação de Combustíveis
    fIND_RI  : Boolean;                              // Exige Registro de Inventário
    fIND_RIDF: Boolean;                              // Exige Registro de Impressão de Documentos Fiscais
    fIND_RUDF: Boolean;                              // Exige Registro de Utilização de Documentos Fiscais
    fIND_ST  : Boolean;                              // Indicador de operações sujeitas à retenção/substituição tributária
    fIND_TIPOESCR_CONT  : TACBrTipoEscrContabil;     // Indicador de escrituração contábil
    fIND_TIPOESCR_FISCAL: TACBrTipoEscrFiscal;       // Indicador de escrituração fiscal
    FRegistro0035: TRegistro0035;
    FRegistro0040: TRegistro0040;
    FRegistro0045: TRegistro0045;
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_ENTDADOS: TACBrTipoEntradaDados read fIND_ENTDADOS write fIND_ENTDADOS;
    property IND_CONTARQ : TACBrConteudoArquivo  read fIND_CONTARQ  write fIND_CONTARQ;
    property IND_ISS : Boolean read fIND_ISS write fIND_ISS;
    property IND_ICMS: Boolean read fIND_ICMS write fIND_ICMS;
    property IND_IPI : Boolean read fIND_IPI write fIND_IPI;
    property IND_IST : Boolean read fIND_ST write fIND_ST;
    property IND_TIPOESCR_FISCAL : TACBrTipoEscrFiscal   read fIND_TIPOESCR_FISCAL  write fIND_TIPOESCR_FISCAL;
    property IND_TIPOESCR_CONT   : TACBrTipoEscrContabil read fIND_TIPOESCR_CONT  write fIND_TIPOESCR_CONT;
    property IND_RI : Boolean read fIND_RI write fIND_RI;
    property IND_RIDF : Boolean read fIND_RIDF write fIND_RIDF;
    property IND_RUDF : Boolean read fIND_RUDF write fIND_RUDF;
    property IND_LMC : Boolean read fIND_LMC write fIND_LMC;
    property IND_LCV : Boolean read fIND_LCV write fIND_LCV;

    /// Registros FILHOS
    property Registro0035: TRegistro0035 read FRegistro0035 write FRegistro0035;
    property Registro0040: TRegistro0040 read FRegistro0040 write FRegistro0040;
    property Registro0045: TRegistro0045 read FRegistro0045 write FRegistro0045;
  end;

  /// Registro 0035 - RETENÇÃO SIMPLIFICADA

  { TRegistro0035 }

  TRegistro0035 = class
  private
    fALIQ_RT : Currency;             // Alíquota de retenção conforme previsto no art. 21 da LC 123/06 para a faixa de receita bruta a que a ME/EPP esteve sujeita no mês anterior ao da prestação
    fANEXO_RT: TACBrAnexoRT;         // Indicador do anexo da LC 123/06 referente à receita bruta
    fDTFIM_RT: TDateTime;                // Data final de utilização da alíquota
    fDTINI_RT: TDateTime;                // Data inicial de utilização da alíquota
    fIND_RT  : TACBrTipoTributacao;  // Indicador da tributação
  public
    constructor Create(AOwner: TRegistro0030); virtual; /// Create

    property IND_RT: TACBrTipoTributacao read fIND_RT write fIND_RT;
    property ANEXO_RT: TACBrAnexoRT  read fANEXO_RT write fANEXO_RT;
    property ALIQ_RT : Currency read fALIQ_RT write fALIQ_RT;
    property DTINI_RT: TDateTime  read fDTINI_RT write fDTINI_RT;
    property DTFIM_RT: TDateTime  read fDTFIM_RT write fDTFIM_RT;
  end;

  /// Registro 0040 - CRÉDITO SIMPLIFICADO

  { TRegistro0040 }

  TRegistro0040 = class
  private
    fALIQ_CRD : Currency;       // Alíquota prevista no art. 23 da LC 123/06 para a faixa de receita bruta a que a ME/EPP esteve sujeita no mês anterior ao da operação
    fANEXO_CRD: TACBrAnexoCRD;  // Indicador do anexo da LC 123/06 referente à receita bruta
    fDTFIM_CRD: TDateTime;          // Data final de utilização da alíquota
    fDTINI_CRD: TDateTime;          // Data inicial de utilização da alíquota
    fIND_CRD  : Boolean;        // Indicador de geração do crédito simplificado
  public
    constructor Create(AOwner: TRegistro0030); virtual; /// Create

    property IND_CRD  : Boolean read fIND_CRD write fIND_CRD;
    property ANEXO_CRD: TACBrAnexoCRD  read fANEXO_CRD write fANEXO_CRD;
    property ALIQ_CRD : Currency read fALIQ_CRD write fALIQ_CRD;
    property DTINI_CRD: TDateTime  read fDTINI_CRD write fDTINI_CRD;
    property DTFIM_CRD: TDateTime  read fDTFIM_CRD write fDTFIM_CRD;
  end;

  /// Registro 0045 -SUBSTITUIÇÃO SIMPLIFICADA

  { TRegistro0045 }

  TRegistro0045 = class
  private
    fALIQ_STS : Currency;        // Valor da alíquota interna ou interestadual sobre o valor da operação ou prestação própria do substituto tributário
    fDTFIM_STS: TDateTime;           // Data inicial de utilização da alíquota
    fDTINI_STS: TDateTime;           // Data final de utilização da alíquota
  public
    constructor Create(AOwner: TRegistro0030); virtual; /// Create

    property ALIQ_STS : Currency read fALIQ_STS write fALIQ_STS;
    property DTINI_STS: TDateTime  read fDTINI_STS write fDTINI_STS;
    property DTFIM_STS: TDateTime  read fDTFIM_STS write fDTFIM_STS;
  end;



  /// Registro 0100 - DADOS DO CONTABILISTA

  { TRegistro0100 }

  TRegistro0100 = class
  private
    FCEP_CF: String;      // Código de Endereçamento Postal da caixa postal
    FCOD_ASS: Char;       // Código de qualificação do assinante
    FCP: String;          // Caixa postal
    fNOME: String;        // Nome do contabilista/escritório:
    fCPF: String;         // Número de inscrição no CPF:
    fCRC: String;         // Número de inscrição no Conselho Regional:
    fCNPJ: String;        // CNPJ do escritório de contabilidade, se houver:
    fCEP: String;         // Código de Endereçamento Postal:
    fENDERECO: String;    // Logradouro e endereço do imóvel:
    fNUM: String;         // Número do imóvel:
    fCOMPL: String;       // Dados complementares do endereço:
    fBAIRRO: String;      // Bairro em que o imóvel está situado:
    fFONE: String;        // Número do telefone:
    fFAX: String;         // Número do fax:
    fEMAIL: String;       // Endereço do correio eletrônico:
    fCOD_MUN: integer;    // Código do município, conforme tabela IBGE:
    FUF: String;          // Sigla da unidade da Federação do contabilista/escritório de contabilidade
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create

    property NOME: String read FNOME write FNOME;
    property CPF: String read FCPF write FCPF;
    property CRC: String read FCRC write FCRC;
    property CNPJ: String read FCNPJ write FCNPJ;
    property CEP: String read FCEP write FCEP;
    property CEP_CF: String read FCEP_CF write FCEP_CF;
    property CP: String read FCP write FCP;
    property ENDERECO: String read FENDERECO write FENDERECO;
    property NUM: String read FNUM write FNUM;
    property COMPL: String read FCOMPL write FCOMPL;
    property BAIRRO: String read FBAIRRO write FBAIRRO;
    property UF: String read FUF write FUF;
    property FONE: String read FFONE write FFONE;
    property FAX: String read FFAX write FFAX;
    property EMAIL: String read FEMAIL write FEMAIL;
    property COD_MUN: integer read FCOD_MUN write FCOD_MUN;
    property COD_ASS: Char read FCOD_ASS write FCOD_ASS;
  end;

  /// Registro 0120 - EMITENTE AVULSO

  TRegistro0120 = class
  private
    FCOD_MUN  : integer;  // Código do município do Fisco emitente
    FCOD_SETOR: String;   // Código do setor do Fisco emitente
    FMATRICULA: String;   // Matrícula do servidor
    FCNPJ : String;       // CNPJ do Fisco emitente
    FCPF  : String;       // CPF do servidor
    FFISCO: String;       // Fisco emitente
    FUF: String;          // UF do Fisco emitente
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create

    property CNPJ: String read FCNPJ write FCNPJ;
    property FISCO: String read FFISCO write FFISCO;
    property UF: String read FUF write FUF;
    property COD_MUN: integer read FCOD_MUN write FCOD_MUN;
    property COD_SETOR: String read FCOD_SETOR write FCOD_SETOR;
    property CPF: String read FCPF write FCPF;
    property MATRICULA: String read FMATRICULA write FMATRICULA;
  end;

  /// Registro 0125 - DADOS DO TECNICO DA EMPRESA = 70/05

  { TRegistro0125 }

  TRegistro0125 = class
  private
    fNOME: String;        // Nome do contabilista/escritório:
    fCPF: String;         // Número de inscrição no CPF:
    fCNPJ: String;        // CNPJ do escritório de contabilidade, se houver:
    fFONE: String;        // Número do telefone:
    fFAX: String;         // Número do fax:
    fEMAIL: String;       // Endereço do correio eletrônico:
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create

    property NOME: String read FNOME write FNOME;
    property CPF: String read FCPF write FCPF;
    property CNPJ: String read FCNPJ write FCNPJ;
    property FONE: String read FFONE write FFONE;
    property FAX: String read FFAX write FFAX;
    property EMAIL: String read FEMAIL write FEMAIL;
  end;


  /// Registro 0150 - TABELA DE CADASTRO DO PARTICIPANTE

  { TRegistro0150 }

  TRegistro0150 = class
  private
    fCOD_PART: String;    // Código de identificação do participante:
    FIE_ST: String;       // Inscrição Estadual do participante emitente contribuinte-substituto na unidade da Federação do destinatário substituído
    FIM: String;          // Inscrição Municipal do participante
    fNOME: String;        // Nome pessoal ou empresarial:
    fCOD_PAIS: String;    // Código do país do participante:
    fCNPJ: String;        // CNPJ do participante:
    fCPF: String;         // CPF do participante na unidade da federação do destinatário:
    fIE: String;          // Inscrição Estadual do participante:
    fCOD_MUN: integer;    // Código do município:
    fSUFRAMA: String;     // Número de inscrição na Suframa:
    fENDERECO: String;    // Logradouro e endereço do imóvel:
    fNUM: String;         // Número do imóvel:
    fCOMPL: String;       // Dados complementares do endereço:
    fBAIRRO: String;      // Bairro em que o imóvel está situado:
    FUF: String;          // Sigla da unidade da Federação do participante

    FRegistro0175: TRegistro0175List;  /// BLOCO C - Lista de Registro0175 (FILHO)

  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_PART: String read FCOD_PART write FCOD_PART;
    property NOME: String read FNOME write FNOME;
    property COD_PAIS: String read FCOD_PAIS write FCOD_PAIS;
    property CNPJ: String read FCNPJ write FCNPJ;
    property CPF: String read FCPF write FCPF;
    property IE_ST: String read FIE_ST write FIE_ST;
    property IE: String read FIE write FIE;
    property IM: String read FIM write FIM;
    property COD_MUN: integer read FCOD_MUN write FCOD_MUN;
    property SUFRAMA: String read FSUFRAMA write FSUFRAMA;
    property ENDERECO: String read FENDERECO write FENDERECO;
    property NUM: String read FNUM write FNUM;
    property COMPL: String read FCOMPL write FCOMPL;
    property BAIRRO: String read FBAIRRO write FBAIRRO;
    property UF: String read FUF write FUF;
    /// Registros FILHOS
    property Registro0175: TRegistro0175List read FRegistro0175 write FRegistro0175;
  end;

  /// Registro 0150 - Lista

  TRegistro0150List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0150;
    procedure SetItem(Index: Integer; const Value: TRegistro0150);
  public
    function New(AOwner: TRegistro0001): TRegistro0150;
    function LocalizaRegistro(pCOD_PART: String): boolean;
    property Items[Index: Integer]: TRegistro0150 read GetItem write SetItem;
  end;

   /// Registro 0175 - ENDERECO DO PARTICIPANTE

  { TRegistro0175 }

  TRegistro0175 = class
  private
    fCEP     : String;  // CEP
    fENDERECO: String;  // Logradouro e endereço do imóvel:
    fNUM     : String;  // Número do imóvel:
    fCOMPL   : String;  // Dados complementares do endereço:
    fBAIRRO  : String;  // Bairro em que o imóvel está situado:
    fCEP_CP  : String;  // Código de Endereçamento Postal da caixa postal
    fCP      : Integer; //  Caixa postal do participante
    fFONE    : String;  // Número do telefone
    fFAX     :String;   // Número do fax
  public
    constructor Create(AOwner: TRegistro0150); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property CEP: String read fCEP write fCEP;
    property ENDERECO: String read fENDERECO write fENDERECO;
    property NUM: String read fNUM write fNUM;
    property COMPL: String read fCOMPL write fCOMPL;
    property BAIRRO: String read fBAIRRO write fBAIRRO;
    property CEP_CP: String read fCEP_CP write fCEP_CP;
    property CP: Integer read fCP write fCP;
    property FONE: String read fFONE write fFONE;
    property FAX: String read fFAX write fFAX;
  end;

  /// Registro 0175 - Lista

  TRegistro0175List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0175;
    procedure SetItem(Index: Integer; const Value: TRegistro0175);
  public
    function New(AOwner: TRegistro0150): TRegistro0175;
    property Items[Index: Integer]: TRegistro0175 read GetItem write SetItem;
  end;


  /// Registro 0200 - TABELA DE IDENTIFICAÇÃO DO ITEM (PRODUTO E SERVIÇOS)

  { TRegistro0200 }

  TRegistro0200 = class
  private
    fCOD_ITEM: String;         /// Código do item:
    fDESCR_ITEM: String;       /// Descrição do item:
    fCOD_GEN: String;          /// Código gênero item, tabela indicada item 4.2.1:
    fCOD_LST: String;          /// Código serviço Anexo I - Lei nº116/03:

    FRegistro0205: TRegistro0205List;  /// BLOCO C - Lista de Registro0205 (FILHO)
    FRegistro0210: TRegistro0210List;
    FRegistro0215: TRegistro0215List;
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property DESCR_ITEM: String read FDESCR_ITEM write FDESCR_ITEM;
    property COD_GEN: String read FCOD_GEN write FCOD_GEN;
    property COD_LST: String read FCOD_LST write FCOD_LST;
    /// Registros FILHOS
    property Registro0205: TRegistro0205List read FRegistro0205 write FRegistro0205;
    property Registro0210: TRegistro0210List read FRegistro0210 write FRegistro0210;
    property Registro0215: TRegistro0215List read FRegistro0215 write FRegistro0215;
  end;

  /// Registro 0200 - Lista

  TRegistro0200List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0200;
    procedure SetItem(Index: Integer; const Value: TRegistro0200);
  public
    function New(AOwner: TRegistro0001): TRegistro0200;
    function LocalizaRegistro(pCOD_ITEM: String): boolean;
    property Items[Index: Integer]: TRegistro0200 read GetItem write SetItem;
  end;

  /// Registro 0205 - CÓDIGO ANTERIOR DO ITEM

  { TRegistro0205 }

  TRegistro0205 = class
  private
    fDESCR_ANT_ITEM: String;    /// Descrição anterior do item:
    fDT_INI: TDateTime;         /// Data inicial de utilização do código:
    fDT_FIN: TDateTime;         /// Data final de utilização do código:
    fCOD_ANT_ITEM: String;      /// Código anterior do item com relação à última informação apresentada.
  public
    constructor Create(AOwner: TRegistro0200); virtual; /// Create

    property COD_ANT_ITEM: String read FCOD_ANT_ITEM write FCOD_ANT_ITEM;
    property DESCR_ANT_ITEM: String read FDESCR_ANT_ITEM write FDESCR_ANT_ITEM;
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
  end;

  /// Registro 0205 - Lista

  TRegistro0205List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0205;
    procedure SetItem(Index: Integer; const Value: TRegistro0205);
  public
    function New(AOwner: TRegistro0200): TRegistro0205;
    property Items[Index: Integer]: TRegistro0205 read GetItem write SetItem;
  end;

  /// Registro 0210 - MERCADORIA COMPONENTE/RELAÇÃO INSUMO/PRODUTO

  { TRegistro0210 }

  TRegistro0210 = class
  private
    FCOD_ITEM_COMP: String;      // Código do item componente
    FDT_FIN_COMP: TDateTime;         // Data de término de vigência da fórmula de composição
    FDT_INI_COMP: TDateTime;         // Data de início de vigência da fórmula de composição
    FIND_ALT: TACBrIndAlteracao; // Indicador de alteração
    FUNID_COMP: String;          // Unidade do item composto/produto
    FUNID_ITEM: String;          // Unidade do item componente
    FPERDA_COMP: Double;         // Percentual de perda do insumo/produto intermediário
    FQTD_COMP  : Double;         // Quantidade do item componente no item composto
  public
    constructor Create(AOwner: TRegistro0200); virtual; /// Create

    property UNID_ITEM: String read FUNID_ITEM write FUNID_ITEM;
    property COD_ITEM_COMP: String read FCOD_ITEM_COMP write FCOD_ITEM_COMP;
    property QTD_COMP: Double read FQTD_COMP write FQTD_COMP;
    property UNID_COMP: String read FUNID_COMP write FUNID_COMP;
    property PERDA_COMP: Double read FPERDA_COMP write FPERDA_COMP;
    property DT_INI_COMP: TDateTime read FDT_INI_COMP write FDT_INI_COMP;
    property DT_FIN_COMP: TDateTime read FDT_FIN_COMP write FDT_FIN_COMP;
    property IND_ALT: TACBrIndAlteracao read FIND_ALT write FIND_ALT;

  end;

  /// Registro 0210 - Lista

  { TRegistro0210List }

  TRegistro0210List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0210;
    procedure SetItem(Index: Integer; const Value: TRegistro0210);
  public
    function New(AOwner: TRegistro0200): TRegistro0210;
    property Items[Index: Integer]: TRegistro0210 read GetItem write SetItem;
  end;

  /// Registro 0215 - CORRELAÇÃO COM A TABELA DA ANP

  { TRegistro0215 }

  TRegistro0215 = class
  private
    FCODITEM_ANP: String; // Código de correlação do item com a tabela de produtos da Agência Nacional do Petróleo
    FDT_FIN: TDateTime;      // Data inicial de utilização do código
    FDT_INI: TDateTime;      // Data final de utilização do código
  public
    constructor Create(AOwner: TRegistro0200); virtual; /// Create
    property CODITEM_ANP : String read FCODITEM_ANP write FCODITEM_ANP;
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
  end;

  /// Registro 0215 - Lista

  { TRegistro0215List }

  TRegistro0215List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0215;
    procedure SetItem(Index: Integer; const Value: TRegistro0215);
  public
    function New(AOwner: TRegistro0200): TRegistro0215;
    property Items[Index: Integer]: TRegistro0215 read GetItem write SetItem;
  end;


  /// Registro 0400 - TABELA DE NATUREZA DA OPERAÇÃO/PRESTAÇÃO

  TRegistro0400 = class
  private
    fCOD_NAT: String;        /// Código da natureza:
    fDESCR_NAT: String;      /// Descrição da natureza:
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create

    property COD_NAT: String read FCOD_NAT write FCOD_NAT;
    property DESCR_NAT: String read FDESCR_NAT write FDESCR_NAT;
  end;
  /// Registro 0400 - Lista

  TRegistro0400List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0400;
    procedure SetItem(Index: Integer; const Value: TRegistro0400);
  public
    function New(AOwner: TRegistro0001): TRegistro0400;
    function LocalizaRegistro(pCOD_NAT: String): boolean;
    property Items[Index: Integer]: TRegistro0400 read GetItem write SetItem;
  end;

  /// Registro 0450 - TABELA DE INFORMAÇÃO COMPLEMENTAR/OBSERVAÇÃO

  { TRegistro0450 }

  TRegistro0450 = class
  private
    fCOD_INF: String;     /// Código da informação complementar do documento fiscal:
    FRegistro0455: TRegistro0455List;
    FRegistro0460: TRegistro0460List;
    FRegistro0465: TRegistro0465List;
    FRegistro0470: TRegistro0470List;
    fTXT: String;         /// Texto livre:
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_INF: String read FCOD_INF write FCOD_INF;
    property TXT: String read FTXT write FTXT;

    property Registro0455: TRegistro0455List read FRegistro0455 write FRegistro0455;
    property Registro0460: TRegistro0460List read FRegistro0460 write FRegistro0460;
    property Registro0465: TRegistro0465List read FRegistro0465 write FRegistro0465;
    property Registro0470: TRegistro0470List read FRegistro0470 write FRegistro0470;
  end;

  /// Registro 0450 - Lista

  TRegistro0450List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0450;
    procedure SetItem(Index: Integer; const Value: TRegistro0450);
  public
    function New(AOwner: TRegistro0001): TRegistro0450;
    function LocalizaRegistro(pCOD_INF :string): Boolean;
    property Items[Index: Integer]: TRegistro0450 read GetItem write SetItem;
  end;

  /// Registro 0455 -  NORMA REFERENCIADA

  TRegistro0455 = class
  private
    FNORMA: String;
  public
    constructor Create(AOwner: TRegistro0450); virtual; /// Create

    property NORMA: String read FNORMA write FNORMA;
  end;

  /// Registro 0455 - Lista

  { TRegistro0455List }

  TRegistro0455List = class(TObjectList)
  private
     function GetItem(Index: Integer): TRegistro0455;
     procedure SetItem(Index: Integer; const Value: TRegistro0455);
  public
     function New(AOwner: TRegistro0450): TRegistro0455;
     property Items[Index: Integer]: TRegistro0455 read GetItem write SetItem;
  end;


  /// Registro 0460 - DOCUMENTO DE ARRECADAÇÃO REFERENCIADO

  { TRegistro0460 }

  TRegistro0460 = class
  private
    FCOD_DA: String;   // Código do modelo do documento de arrecadação
    FDT_FIM: TDateTime;    // Data final do período de referência
    FDT_INI: TDateTime;    // Data inicial do período de referência
    FDT_PAGTO: TDateTime;  // Data de pagamento do documento de arrecadação
    FDT_VENC: TDateTime;   // Data de vencimento do documento de arrecadação
    FNUM_DA: String;
    FVALOR_DA: Double;
  public
    constructor Create(AOwner: TRegistro0450); virtual; /// Create

    property COD_DA: String read FCOD_DA write FCOD_DA;
    property NUM_DA: String read FNUM_DA write FNUM_DA;
    property VALOR_DA: Double read FVALOR_DA write FVALOR_DA;
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIM: TDateTime read FDT_FIM write FDT_FIM;
    property DT_VENC: TDateTime read FDT_VENC write FDT_VENC;
    property DT_PAGTO: TDateTime read FDT_PAGTO write FDT_PAGTO;
  end;

  /// Registro 0460 - Lista

  TRegistro0460List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0460;
    procedure SetItem(Index: Integer; const Value: TRegistro0460);
  public
    function New(AOwner: TRegistro0450): TRegistro0460;
    function LocalizaRegistro(pCOD_OBS: String): boolean;
    property Items[Index: Integer]: TRegistro0460 read GetItem write SetItem;
  end;

  /// Registro 0465 - DOCUMENTO FISCAL REFERENCIADO

  { TRegistro0465 }

  TRegistro0465 = class
  private
    FCNPJ: String;                 // CNPJ do participante
    FCODMUN: Integer;              // Código do município do participante
    FCOD_MOD: String;              // Código do documento fiscal
    FCOD_SIT: String;              // Código da situação do documento fiscal,
    FCPF: String;                  // CPF do participante
    FDT_EMISSAO: TDateTime;            // Data da emissão, da entrada ou do lançamento
    FIE: String;                   // Inscrição Estadual do participante
    FIM: String;                   // Inscrição Municipal do participante
    FIND_EMIT: TACBrlEmitente;      // Indicador do emitente do título
    FIND_OPER: TACBrlTipoOperacao;  // Indicador do tipo de operação
    FNUMDOCTO: Integer;            // Número do documento fiscal
    FSERIE: String;                // Série do documento fiscal
    FSUBSERIE: Integer;            // Subsérie do documento fiscal
    FUF: String;                   // Sigla da unidade da federação do participante
    FVALOR_DOC: Double;            // Valor total do documento fiscal
    FVALOR_ICMS: Double;           // Valor do ICMS
    FVALOR_IPI: Double;            // Valor do IPI
    FVALOR_ISS: Double;            // Valor do ISS
    FVALOR_RT: Double;             // Valor do ISS retido
    FVALOR_ST: Double;             // Valor do ICMS retido por substituição tributária
  public
    constructor Create(AOwner: TRegistro0450); virtual; /// Create

    property IND_OPER: TACBrlTipoOperacao read FIND_OPER write FIND_OPER;
    property IND_EMIT: TACBrlEmitente read FIND_EMIT write FIND_EMIT;
    property CNPJ: String read FCNPJ write FCNPJ;
    property CPF: String read FCPF write FCPF;
    property UF: String read FUF write FUF;
    property IE: String read FIE write FIE;
    property CODMUN: Integer read FCODMUN write FCODMUN;
    property IM: String read FIM write FIM;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property COD_SIT: String read FCOD_SIT write FCOD_SIT;
    property SERIE: String read FSERIE write FSERIE;
    property SUBSERIE: Integer read FSUBSERIE write FSUBSERIE;
    property NUMDOCTO: Integer read FNUMDOCTO write FNUMDOCTO;
    property DT_EMISSAO: TDateTime read FDT_EMISSAO write FDT_EMISSAO;
    property VALOR_DOC: Double read FVALOR_DOC write FVALOR_DOC;
    property VALOR_ISS: Double read FVALOR_ISS write FVALOR_ISS;
    property VALOR_RT: Double read FVALOR_RT write FVALOR_RT;
    property VALOR_ICMS: Double read FVALOR_ICMS write FVALOR_ICMS;
    property VALOR_ST: Double read FVALOR_ST write FVALOR_ST;
    property VALOR_IPI: Double read FVALOR_IPI write FVALOR_IPI;
  end;

  /// Registro 0465 - Lista

  { TRegistro0465List }

  TRegistro0465List = class(TObjectList)
  private
     function GetItem(Index: Integer): TRegistro0465;
     procedure SetItem(Index: Integer; const Value: TRegistro0465);
  public
     function New(AOwner: TRegistro0450): TRegistro0465;
     property Items[Index: Integer]: TRegistro0465 read GetItem write SetItem;
  end;

  /// Registro 0470 - CUPOM FISCAL REFERENCIADO

  { TRegistro0470 }

  TRegistro0470 = class
  private
    FCOD_MOD: String;              // Código do documento fiscal
    FDT_EMISSAO: TDateTime;            // Data da emissão, da entrada ou do lançamento
    FECF_CRO: Integer;             // Posição do Contador de Reinício de OperaçãO
    FECF_CRZ: Integer;             // Posição do Contador de Redução Z
    FECF_CX: String;               // Número do caixa atribuído ao ECF
    FECF_FAB: String;              // Número de série de fabricação do ECF
    FNUMDOCTO: Integer;            // Número do documento fiscal
    FVALOR_DOC: Double;            // Valor total do documento fiscal
    FVALOR_ICMS: Double;           // Valor do ICMS
    FVALOR_ISS: Double;            // Valor do ISS
  public
    constructor Create(AOwner: TRegistro0450); virtual; /// Create

    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property ECF_FAB: String read FECF_FAB write FECF_FAB;
    property ECF_CX : String read FECF_CX write FECF_CX;
    property ECF_CRO: Integer read FECF_CRO write FECF_CRO;
    property ECF_CRZ: Integer read FECF_CRZ write FECF_CRZ;
    property NUMDOCTO: Integer read FNUMDOCTO write FNUMDOCTO;
    property DT_EMISSAO: TDateTime read FDT_EMISSAO write FDT_EMISSAO;
    property VALOR_DOC: Double read FVALOR_DOC write FVALOR_DOC;
    property VALOR_ISS: Double read FVALOR_ISS write FVALOR_ISS;
    property VALOR_ICMS: Double read FVALOR_ICMS write FVALOR_ICMS;
  end;

  /// Registro 0470 - Lista

  { TRegistro0470List }

  TRegistro0470List = class(TObjectList)
  private
     function GetItem(Index: Integer): TRegistro0470;
     procedure SetItem(Index: Integer; const Value: TRegistro0470);
  public
     function New(AOwner: TRegistro0450): TRegistro0470;
     property Items[Index: Integer]: TRegistro0470 read GetItem write SetItem;
  end;


  /// Registro 0990 - ENCERRAMENTO DO BLOCO 0

  TRegistro0990 = class
  private
    fQTD_LIN_0: Integer; /// Quantidade total de linhas do Bloco 0
  public
    property QTD_LIN_0: Integer read fQTD_LIN_0 write fQTD_LIN_0;
  end;

implementation

{ TRegistro0125 }

constructor TRegistro0125.Create(AOwner: TRegistro0001);
begin

end;

{ TRegistro0470List }

function TRegistro0470List.GetItem(Index: Integer): TRegistro0470;
begin
   Result := TRegistro0470(Inherited Items[Index]);
end;

procedure TRegistro0470List.SetItem(Index: Integer; const Value: TRegistro0470);
begin
   Put(Index, Value);
end;

function TRegistro0470List.New(AOwner: TRegistro0450): TRegistro0470;
begin
   Result := TRegistro0470.Create(AOwner);
   Add(Result);
end;

{ TRegistro0465List }

function TRegistro0465List.GetItem(Index: Integer): TRegistro0465;
begin
   Result := TRegistro0465(Inherited Items[Index]);
end;

procedure TRegistro0465List.SetItem(Index: Integer; const Value: TRegistro0465);
begin
   Put(Index, Value);
end;

function TRegistro0465List.New(AOwner: TRegistro0450): TRegistro0465;
begin
   Result := TRegistro0465.Create(AOwner);
   Add(Result);
end;

{ TRegistro0455List }

function TRegistro0455List.GetItem(Index: Integer): TRegistro0455;
begin
   Result := TRegistro0455(Inherited Items[Index]);
end;

procedure TRegistro0455List.SetItem(Index: Integer; const Value: TRegistro0455);
begin
   Put(Index, Value);
end;

function TRegistro0455List.New(AOwner: TRegistro0450): TRegistro0455;
begin
   Result := TRegistro0455.Create(AOwner);
   Add(Result);
end;

{ TRegistro0215List }

function TRegistro0215List.GetItem(Index: Integer): TRegistro0215;
begin
   Result := TRegistro0215(Inherited Items[Index]);
end;

procedure TRegistro0215List.SetItem(Index: Integer; const Value: TRegistro0215);
begin
   Put(Index, Value);
end;

function TRegistro0215List.New(AOwner: TRegistro0200): TRegistro0215;
begin
   Result := TRegistro0215.Create(AOwner);
   Add(Result);
end;

{ TRegistro0215 }

constructor TRegistro0215.Create(AOwner: TRegistro0200);
begin

end;

{ TRegistro0210List }

function TRegistro0210List.GetItem(Index: Integer): TRegistro0210;
begin
   Result := TRegistro0210(Inherited Items[Index]);
end;

procedure TRegistro0210List.SetItem(Index: Integer; const Value: TRegistro0210);
begin
   Put(Index, Value);
end;

function TRegistro0210List.New(AOwner: TRegistro0200): TRegistro0210;
begin
   Result := TRegistro0210.Create(AOwner);
   Add(Result);
end;

{ TRegistro0025List }

function TRegistro0025List.GetItem(Index: Integer): TRegistro0025;
begin
   Result := TRegistro0025(Inherited Items[Index]);
end;

procedure TRegistro0025List.SetItem(Index: Integer; const Value: TRegistro0025);
begin
   Put(Index, Value);
end;

function TRegistro0025List.New(AOwner: TRegistro0001): TRegistro0025;
begin
   Result := TRegistro0025.Create(AOwner);
   Add(Result);
end;

{ TRegistro0470 }

constructor TRegistro0470.Create(AOwner: TRegistro0450);
begin

end;

{ TRegistro0465 }

constructor TRegistro0465.Create(AOwner: TRegistro0450);
begin

end;

{ TRegistro0455 }

constructor TRegistro0455.Create(AOwner: TRegistro0450);
begin

end;

{ TRegistro0210 }

constructor TRegistro0210.Create(AOwner: TRegistro0200);
begin

end;

{ TRegistro0120 }

constructor TRegistro0120.Create(AOwner: TRegistro0001);
begin

end;

{ TRegistro0045 }

constructor TRegistro0045.Create(AOwner: TRegistro0030);
begin

end;

{ TRegistro0025 }

constructor TRegistro0025.Create(AOwner: TRegistro0001);
begin

end;

{ TRegistro0035 }

constructor TRegistro0035.Create(AOwner: TRegistro0030);
begin

end;

{ TRegistro0040 }

constructor TRegistro0040.Create(AOwner: TRegistro0030);
begin

end;


{ TRegistro0030 }

constructor TRegistro0030.Create(AOwner: TRegistro0001);
begin
   FRegistro0035 := TRegistro0035.Create(Self);
   FRegistro0040 := TRegistro0040.Create(self);
   FRegistro0045 := TRegistro0045.Create(Self);
end;

destructor TRegistro0030.Destroy;
begin
  FRegistro0035.Free;
  FRegistro0040.Free;
  FRegistro0045.Free;
  inherited Destroy;
end;

{ TRegistro0001 }

constructor TRegistro0001.Create;
begin
  FRegistro0005 := TRegistro0005.Create(Self);
  FRegistro0025 := TRegistro0025List.Create;
  FRegistro0030 := TRegistro0030.Create(Self);
  FRegistro0200 := TRegistro0200List.Create;
  FRegistro0100 := TRegistro0100.Create(Self);
  FRegistro0125 := TRegistro0125.Create(Self);
  FRegistro0120 := TRegistro0120.Create(Self);
  FRegistro0150 := TRegistro0150List.Create;
  FRegistro0400 := TRegistro0400List.Create;
  FRegistro0460 := TRegistro0460List.Create;
  FRegistro0450 := TRegistro0450List.Create;
  //
  IND_MOV := imlComDados;
end;

destructor TRegistro0001.Destroy;
begin
  FRegistro0005.Free;
  FRegistro0025.Free;
  FRegistro0030.Free;
  FRegistro0200.Free;
  FRegistro0100.Free;
  FRegistro0125.Free;
  FRegistro0120.Free;
  FRegistro0150.Free;
  FRegistro0400.Free;
  FRegistro0460.Free;
  FRegistro0450.Free;
  inherited;
end;

{ TRegistro0150List }

function TRegistro0150List.GetItem(Index: Integer): TRegistro0150;
begin
  Result := TRegistro0150(Inherited Items[Index]);
end;

function TRegistro0150List.LocalizaRegistro(pCOD_PART: String): boolean;
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

function TRegistro0150List.New(AOwner: TRegistro0001): TRegistro0150;
begin
  Result := TRegistro0150.Create(AOwner);
  Add(Result);
end;

procedure TRegistro0150List.SetItem(Index: Integer; const Value: TRegistro0150);
begin
  Put(Index, Value);
end;

{ TRegistro0175List }

function TRegistro0175List.GetItem(Index: Integer): TRegistro0175;
begin
  Result := TRegistro0175(Inherited Items[Index]);
end;

function TRegistro0175List.New(AOwner: TRegistro0150): TRegistro0175;
begin
  Result := TRegistro0175.Create(AOwner);
  Add(Result);
end;

procedure TRegistro0175List.SetItem(Index: Integer; const Value: TRegistro0175);
begin
  Put(Index, Value);
end;

{ TRegistro0200List }

function TRegistro0200List.GetItem(Index: Integer): TRegistro0200;
begin
  Result := TRegistro0200(Inherited Items[Index]);
end;

function TRegistro0200List.LocalizaRegistro(pCOD_ITEM: String): boolean;
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

function TRegistro0200List.New(AOwner: TRegistro0001): TRegistro0200;
begin
  Result := TRegistro0200.Create(AOwner);
  Add(Result);
end;

procedure TRegistro0200List.SetItem(Index: Integer; const Value: TRegistro0200);
begin
  Put(Index, Value);
end;

{ TRegistro0205List }

function TRegistro0205List.GetItem(Index: Integer): TRegistro0205;
begin
  Result := TRegistro0205(Inherited Items[Index]);
end;

function TRegistro0205List.New(AOwner: TRegistro0200): TRegistro0205;
begin
  Result := TRegistro0205.Create(AOwner);
  Add(Result);
end;

procedure TRegistro0205List.SetItem(Index: Integer; const Value: TRegistro0205);
begin
  Put(Index, Value);
end;

{ TRegistro0400List }

function TRegistro0400List.GetItem(Index: Integer): TRegistro0400;
begin
  Result := TRegistro0400(Inherited Items[Index]);
end;

function TRegistro0400List.LocalizaRegistro(pCOD_NAT: String): boolean;
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

function TRegistro0400List.New(AOwner: TRegistro0001): TRegistro0400;
begin
  Result := TRegistro0400.Create(AOwner);
  Add(Result);
end;

procedure TRegistro0400List.SetItem(Index: Integer; const Value: TRegistro0400);
begin
  Put(Index, Value);
end;

{ TRegistro0450List }

function TRegistro0450List.GetItem(Index: Integer): TRegistro0450;
begin
  Result := TRegistro0450(Inherited Items[Index]);
end;

function TRegistro0450List.LocalizaRegistro(pCOD_INF: string): Boolean;
  var
    iI: Integer;
begin
  Result := False;
  for iI := 0 to Pred(Self.Count) do
  begin
    if Self.Items[iI].COD_INF = pCOD_INF then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TRegistro0450List.New(AOwner: TRegistro0001): TRegistro0450;
begin
  Result := TRegistro0450.Create(AOwner);
  Add(Result);
end;

procedure TRegistro0450List.SetItem(Index: Integer; const Value: TRegistro0450);
begin
  Put(Index, Value);
end;

{ TRegistro0460List }

function TRegistro0460List.GetItem(Index: Integer): TRegistro0460;
begin
  Result := TRegistro0460(inherited Items[Index]);
end;

function TRegistro0460List.LocalizaRegistro(pCOD_OBS: String): boolean;
begin
  Result := False;
end;

function TRegistro0460List.New(AOwner: TRegistro0450): TRegistro0460;
begin
  Result := TRegistro0460.Create(AOwner);
  Add(Result);
end;

procedure TRegistro0460List.SetItem(Index: Integer; const Value: TRegistro0460);
begin
  Put(Index, Value);
end;

{ TRegistro0150 }

constructor TRegistro0150.Create(AOwner: TRegistro0001);
begin
  FRegistro0175 := TRegistro0175List.Create;
end;

destructor TRegistro0150.Destroy;
begin
  FRegistro0175.Free;
  inherited;
end;

{ TRegistro0200 }

constructor TRegistro0200.Create(AOwner: TRegistro0001);
begin
   FRegistro0205 := TRegistro0205List.Create;
   FRegistro0210 := TRegistro0210List.Create;
   FRegistro0215 := TRegistro0215List.Create;
end;

destructor TRegistro0200.Destroy;
begin
  FRegistro0205.Free;
  FRegistro0210.Free;
  FRegistro0215.Free;
  inherited;
end;

{ TRegistro0005 }

constructor TRegistro0005.Create(AOwner: TRegistro0001);
begin
end;


{ TRegistro0100 }

constructor TRegistro0100.Create(AOwner: TRegistro0001);
begin
end;

{ TRegistro0175 }

constructor TRegistro0175.Create(AOwner: TRegistro0150);
begin
end;

destructor TRegistro0175.Destroy;
begin
  inherited Destroy;
end;

{ TRegistro0205 }

constructor TRegistro0205.Create(AOwner: TRegistro0200);
begin
end;


{ TRegistro0400 }

constructor TRegistro0400.Create(AOwner: TRegistro0001);
begin
end;

{ TRegistro0450 }

constructor TRegistro0450.Create(AOwner: TRegistro0001);
begin
   FRegistro0455 := TRegistro0455List.Create;
   FRegistro0460 := TRegistro0460List.Create;
   FRegistro0465 := TRegistro0465List.Create;
   FRegistro0470 := TRegistro0470List.Create;
end;

destructor TRegistro0450.Destroy;
begin
   FRegistro0455.Free;
   FRegistro0460.Free;
   FRegistro0465.Free;
   FRegistro0470.Free;
   inherited Destroy;
end;

{ TRegistro0460 }

constructor TRegistro0460.Create(AOwner: TRegistro0450);
begin
end;

end.
