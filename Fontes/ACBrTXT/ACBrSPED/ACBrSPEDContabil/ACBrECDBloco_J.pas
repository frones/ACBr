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
|* 06/05/2014: Francinaldo A. da Costa
|*  - Modificações para o layout 2
|* 04/03/2015: Flavio Rubens Massaro Jr.
|* - Modificação para contemplar layout 3 referente ao ano calendario 2014
|* 03/02/2016: Anderson Nunes Kovaski
|* - Modificação para contemplar layout 4 referente ao ano calendario 2015
*******************************************************************************}

unit ACBrECDBloco_J;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECDBlocos;

type
  /// Registro J001 - ABERTURA DO BLOCO J

  TRegistroJ001 = class(TOpenBlocos)
  private
  public
  end;

  TRegistroJ100List = class;
  TRegistroJ150List = class;
  TRegistroJ200List = class;
  TRegistroJ210List = class;
  TRegistroJ800List = class;
  TRegistroJ801List = class;
  TRegistroJ215List = class;

  /// Rregistro J005 – DEMONSTRAÇÕES CONTÁBEIS

  TRegistroJ005 = class
  private
    fDT_INI: TDateTime;    /// Data inicial das demonstrações contábeis.
    fDT_FIN: TDateTime;    /// Data final das demonstrações contábeis.
    fID_DEM: Integer;      /// Identificação das demonstrações: 1 - demonstrações contábeis do empresário ou sociedade empresária a que se refere a escrituração; 2 - demonstrações consolidadas ou de outros empresários ou sociedades empresárias.
    fCAB_DEM: String;      /// Cabeçalho das demonstrações.

    FRegistroJ100: TRegistroJ100List;  /// BLOCO J - Lista de RegistroJ100 (FILHO)
    FRegistroJ150: TRegistroJ150List;  /// BLOCO J - Lista de RegistroJ150 (FILHO)
    FRegistroJ200: TRegistroJ200List;  /// BLOCO J - Lista de RegistroJ200 (FILHO)
    FRegistroJ210: TRegistroJ210List;
    FRegistroJ800: TRegistroJ800List;
    FRegistroJ801: TRegistroJ801List;

  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy
    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property ID_DEM: Integer read fID_DEM write fID_DEM;
    property CAB_DEM: String read fCAB_DEM write fCAB_DEM;
    /// Registros FILHOS
    property RegistroJ100: TRegistroJ100List read FRegistroJ100 write FRegistroJ100;
    property RegistroJ150: TRegistroJ150List read FRegistroJ150 write FRegistroJ150;
    property RegistroJ200: TRegistroJ200List read FRegistroJ200 write FRegistroJ200;
    property RegistroJ210: TRegistroJ210List read FRegistroJ210 write FRegistroJ210;
    property RegistroJ800: TRegistroJ800List read FRegistroJ800 write FRegistroJ800;
    property RegistroJ801: TRegistroJ801List read FRegistroJ801 write FRegistroJ801;

  end;

  /// Registro J005 - Lista

  TRegistroJ005List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ005;
    procedure SetItem(Index: Integer; const Value: TRegistroJ005);
  public
    function New: TRegistroJ005;
    property Items[Index: Integer]: TRegistroJ005 read GetItem write SetItem;
  end;

  /// Registro J100 – BALANÇO PATRIMONIAL

  TRegistroJ100 = class
  private
    fCOD_AGL: String;        /// Código de aglutinação das contas, atribuído pelo empresário ou sociedade empresária.
    fIND_COD_AGL: String;    /// Indicador do tipo de código de aglutinação das linhas (leiaute 7: T - Totalizador; D - Detalhe)
    fNIVEL_AGL: String;      /// Nível do Código de aglutinação (mesmo conceito do plano de contas - Registro I050).
    fCOD_AGL_SUP: String;    /// Código de aglutinação sintético/grupo de código de aglutinação de nível superior
    fIND_GRP_BAL: String;    /// Indicador de grupo do balanço: 1 ou A - Ativo; 2 ou P - Passivo e Patrimônio Líquido;
    fDESCR_COD_AGL: String;  /// Descrição do Código de aglutinação.
    fVL_CTA: Currency;       /// Valor total do Código de aglutinação no Balanço Patrimonial no exercício informado, ou de período definido em norma específica.
    fIND_DC_BAL: String;     /// Indicador da situação do saldo informado no campo anterior: D - Devedor; C - Credor.
    fVL_CTA_INI: Currency;   /// Valor inicial do código de aglutinação no Balanço Patrimonial no exercício informado, ou de período definido em norma específica.
    fIND_DC_BAL_INI: string; /// Indicador da situação do saldo inicial informado no campo anterior: D - Devedor; C – Credor.
    fIND_DC_CTA_INI: string; /// Indicador da situação do saldo inicial informado no campo anterior: D - Devedor; C – Credor.
    fVL_CTA_FIN: Currency;   /// Valor final do código de aglutinação no Balanço patrimonial no ecercíxio informado, ou de período definido de forma específica.
    fIND_DC_CTA_FIN: String; /// Indicador da situação do saldo final informado no campo anterior: D - Devedor; C - Credor;
    fNOTAS_EXP_REF: String;  /// Notas explicativas relativas às demonstrações contábeis.
  public
    property COD_AGL: String read fCOD_AGL write fCOD_AGL;
    property IND_COD_AGL: String read fIND_COD_AGL write fIND_COD_AGL;
    property NIVEL_AGL: String read fNIVEL_AGL write fNIVEL_AGL;
    property COD_AGL_SUP: String read fCOD_AGL_SUP write fCOD_AGL_SUP;
    property IND_GRP_BAL: String read fIND_GRP_BAL write fIND_GRP_BAL;
    property DESCR_COD_AGL: String read fDESCR_COD_AGL write fDESCR_COD_AGL;
    property VL_CTA: Currency read fVL_CTA write fVL_CTA;
    property IND_DC_BAL: String read fIND_DC_BAL write fIND_DC_BAL;
    property VL_CTA_INI: Currency read fVL_CTA_INI write fVL_CTA_INI;
    property IND_DC_BAL_INI: String read fIND_DC_BAL_INI write fIND_DC_BAL_INI;
    property IND_DC_CTA_INI: String read fIND_DC_CTA_INI write fIND_DC_CTA_INI;
    property VL_CTA_FIN: Currency read fVL_CTA_FIN write fVL_CTA_FIN;
    property IND_DC_CTA_FIN: String read fIND_DC_CTA_FIN write fIND_DC_CTA_FIN;
    property NOTAS_EXP_REF: String read fNOTAS_EXP_REF write fNOTAS_EXP_REF;
  end;

  /// Registro J100 - Lista

  TRegistroJ100List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ100;
    procedure SetItem(Index: Integer; const Value: TRegistroJ100);
  public
    function New: TRegistroJ100;
    function LocalizaRegistro(const pCOD_AGL: String): boolean;
    property Items[Index: Integer]: TRegistroJ100 read GetItem write SetItem;
  end;

  /// Registro J150 – DEMONSTRAÇÃO DO RESULTADO DO EXERCÍCIO

  TRegistroJ150 = class
  private
    fNU_ORDEM: String;       /// Número de ordem da linha na visualização da demonstração
    fCOD_AGL: String;        /// Código de aglutinação das contas, atribuído pelo empresário ou sociedade empresária.
    fIND_COD_AGL: string;     /// Indicador do tipo de código de aglutinação das linhas (leiaute 7: T - Totalizador; D - Detalhe)
    fNIVEL_AGL: String;      /// Nível do Código de aglutinação (mesmo conceito do plano de contas - Registro I050).
    fCOD_AGL_SUP: String;    /// Código de aglutinação sintético/grupo de código de aglutinação de nível superior
    fDESCR_COD_AGL: String;  /// Descrição do Código de aglutinação.
    fVL_CTA_INI: Currency;       /// Valor do saldo final da linha no período imediatamente anterior
    fIND_DC_CTA_INI: String;     /// Indicador da situação do valor final da linha no período imediatamente anterior
    fVL_CTA_FIN: Currency;       /// Valor final da linha antes do encerramento do exercício
    fIND_DC_CTA_FIN: String;     /// Indicador da situação do valor final da linha antes do encerramento
    fVL_CTA: Currency;           /// Valor total do Código de aglutinação na Demonstração do Resultado do Exercício no período informado.
    fIND_DC_CTA: String;         /// Indicador da situação do valor total do código de aglutinação: D - Devedor; C - Credor;
    fIND_GRP_DRE: String;         /// Indicador de grupo da DRE: D ou R
    fIND_VL: String;             /// Indicador da situação do valor informado no campo anterior: D - Despesa ou valor que represente parcela redutora do lucro;R - Receita ou valor que represente incremento do lucro;P - Subtotal ou total positivo;N - Subtotal ou total negativo.
    fVL_CTA_ULT_DRE: Currency;   /// Valor inicial total constante na Demonstração do Resultado do Exercício do último período informado.
    fIND_VL_ULT_DRE: String;     /// Indicador da situação do valor informado no campo anterior: D - Despesa ou valor que represente parcela redutora do lucro;R - Receita ou valor que represente incremento do lucro;P - Subtotal ou total positivo;N - Subtotal ou total negativo.
    fNOTAS_EXP_REF: String;      /// Notas explicativas relativas às demonstrações contábeis.    
  public
    property NU_ORDEM: String read fNU_ORDEM write fNU_ORDEM;
    property COD_AGL: String read fCOD_AGL write fCOD_AGL;
    property IND_COD_AGL: String read fIND_COD_AGL write fIND_COD_AGL;
    property NIVEL_AGL: String read fNIVEL_AGL write fNIVEL_AGL;
    property COD_AGL_SUP: String read fCOD_AGL_SUP write fCOD_AGL_SUP;
    property DESCR_COD_AGL: String read fDESCR_COD_AGL write fDESCR_COD_AGL;
    property VL_CTA_INI: Currency read fVL_CTA_INI write fVL_CTA_INI;
    property IND_DC_CTA_INI: String read fIND_DC_CTA_INI write fIND_DC_CTA_INI;
    property VL_CTA_FIN: Currency read fVL_CTA_FIN write fVL_CTA_FIN;
    property IND_DC_CTA_FIN: String read fIND_DC_CTA_FIN write fIND_DC_CTA_FIN;
    property VL_CTA: Currency read fVL_CTA write fVL_CTA;
    property IND_VL: String read fIND_VL write fIND_VL;
    property VL_CTA_ULT_DRE: Currency read fVL_CTA_ULT_DRE write fVL_CTA_ULT_DRE;    
    property IND_VL_ULT_DRE: String read fIND_VL_ULT_DRE write fIND_VL_ULT_DRE;
    property IND_DC_CTA: String read fIND_DC_CTA write fIND_DC_CTA;
    property IND_GRP_DRE: String read fIND_GRP_DRE write fIND_GRP_DRE;
    property NOTAS_EXP_REF: String read fNOTAS_EXP_REF write fNOTAS_EXP_REF;
  end;

  /// Registro J150 - Lista

  TRegistroJ150List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ150;
    procedure SetItem(Index: Integer; const Value: TRegistroJ150);
  public
    function New: TRegistroJ150;
    function LocalizaRegistro(const pCOD_AGL: String): boolean;
    property Items[Index: Integer]: TRegistroJ150 read GetItem write SetItem;
  end;

  /// Registro J200 - TABELA DE HISTÓRICO DE FATOS CONTÁBEIS QUE MODIFICAM A CONTA LUCROS ACUMULADOS OU A CONTA PREJUÍZOS ACUMULADOS OU TODO O PATRIMÔNIO LÍQUIDO

  TRegistroJ200 = class
  private
    fCOD_HIST_FAT: String;    /// Código do histórico do fato contábil.
    fDESC_FAT: String;  /// Descrição do fato contábil.
  public
    property COD_HIST_FAT: String read fCOD_HIST_FAT write fCOD_HIST_FAT;
    property DESC_FAT: String read fDESC_FAT write fDESC_FAT;
  end;

  /// Registro J200 - Lista

  TRegistroJ200List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ200;
    procedure SetItem(Index: Integer; const Value: TRegistroJ200);
  public
    function New: TRegistroJ200;
    function LocalizaRegistro(const pCOD_HIST_FAT: String): boolean;
    property Items[Index: Integer]: TRegistroJ200 read GetItem write SetItem;

  end;

  /// Registro J210 – DLPA – DEMONSTRAÇÃO DE LUCROS OU PREJUÍZOS ACUMULADOS/DMPL – DEMONSTRAÇÃO DE MUTAÇÕES DO PATRIMÔNIO LÍQUIDO
  TRegistroJ210 = class
  private
    fIND_TIP: String;        /// Indicador do tipo de demonstração: 0 – DLPA, 1 – DMPL
    fCOD_AGL: String;        /// Código de aglutinação das contas, atribuído pelo empresário ou sociedade empresária.
    fDESCR_COD_AGL: String;  /// Descrição do Código de aglutinação.
    fVL_CTA: Currency;       /// Saldo final do código de aglutinação na demonstração do período informado.
    fIND_DC_CTA: String;     /// Indicador da situação do saldo FINAL informado no campo anterior: D - Devedor; C - Credor.
    fVL_CTA_INI: Currency;   /// Saldo inicial do código de aglutinação na demonstração do período informado
    fIND_DC_CTA_INI: String; /// Indicador da situação do saldo inicial informado no campo anterior: D – Devedor C – Credor
    fVL_CTA_FIN: Currency;   /// Saldo final do código de aglutinação na demonstração do período informado
    fIND_DC_CTA_FIN: String; /// INdicador da situação do saldo final informado: D - Devedor; C - Credor
    fNOTAS_EXP_REF: String;  /// Notas explicativas relativas às demonstrações contábeis.    
    ///
    FRegistroJ215: TRegistroJ215List;  /// BLOCO J - Lista de RegistroJ215 (FILHO)
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_TIP: String read fIND_TIP write fIND_TIP;
    property COD_AGL: String read fCOD_AGL write fCOD_AGL;
    property DESCR_COD_AGL: String read fDESCR_COD_AGL write fDESCR_COD_AGL;
    property VL_CTA: Currency read fVL_CTA write fVL_CTA;
    property IND_DC_CTA: String read fIND_DC_CTA write fIND_DC_CTA;
    property VL_CTA_INI: Currency read fVL_CTA_INI write fVL_CTA_INI;
    property IND_DC_CTA_INI: String read fIND_DC_CTA_INI write fIND_DC_CTA_INI;
    property VL_CTA_FIN: Currency read fVL_CTA_FIN write fVL_CTA_FIN;
    property IND_DC_CTA_FIN: String read fIND_DC_CTA_FIN write fIND_DC_CTA_FIN;
    property NOTAS_EXP_REF: String read fNOTAS_EXP_REF write fNOTAS_EXP_REF;
    /// Registros FILHOS
    property RegistroJ215: TRegistroJ215List read FRegistroJ215 write FRegistroJ215;
  end;

  /// Registro J210 - Lista
  TRegistroJ210List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ210;
    procedure SetItem(Index: Integer; const Value: TRegistroJ210);
  public
    function New: TRegistroJ210;
    function LocalizaRegistro(const pCOD_AGL: String): boolean;
    property Items[Index: Integer]: TRegistroJ210 read GetItem write SetItem;
  end;

  /// Registro J215 - FATO CONTÁBIL QUE ALTERA A CONTA LUCROS ACUMULADOS OU A CONTA PREJUÍZOS ACUMULADOS OU TODO O PATRIMÔNIO LÍQUIDO
  TRegistroJ215 = class
  private
    fCOD_HIST_FAT: String;    /// Código do histórico do fato contábil.
    fVL_FAT_CONT: Currency;   /// Valor do fato contábil.
    fIND_DC_FAT: String;      /// Indicador de situação do saldo informado no campo anterior
    fDESC_FAT: String;         /// Descrição do fato contábil
  public
    property COD_HIST_FAT: String read fCOD_HIST_FAT write fCOD_HIST_FAT;
    property VL_FAT_CONT: Currency read fVL_FAT_CONT write fVL_FAT_CONT;
    property IND_DC_FAT: String read fIND_DC_FAT write fIND_DC_FAT;
    property DESC_FAT: String read fDESC_FAT write fDESC_FAT;
  end;

  /// Registro J215 - Lista

  TRegistroJ215List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ215;
    procedure SetItem(Index: Integer; const Value: TRegistroJ215);
  public
    function New: TRegistroJ215;
    function LocalizaRegistro(const pCOD_HIST_FAT: String): boolean;
    property Items[Index: Integer]: TRegistroJ215 read GetItem write SetItem;
  end;


  TRegistroJ800 = class
  private
    fTIPO_DOC: String; // Tipo de documento
    fDESC_RTF: String; // Descrição do arquivo
    fHASH_RTF: String; // Hash do arquivo
    fARQ_RTF: String;  // Seqüência de bytes que representem um único arquivo no formato RTF (Rich Text Format).
  public
    property TIPO_DOC: String read fTIPO_DOC write fTIPO_DOC;
    property DESC_RTF: String read fDESC_RTF write fDESC_RTF;
    property HASH_RTF: String read fHASH_RTF write fHASH_RTF;
    property ARQ_RTF: String read fARQ_RTF write fARQ_RTF;
  end;

  /// Registro J800 - Lista

  TRegistroJ800List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ800;
    procedure SetItem(Index: Integer; const Value: TRegistroJ800);
  public
    function New: TRegistroJ800;
    property Items[Index: Integer]: TRegistroJ800 read GetItem write SetItem;
  end;

  /// Rregistro J801 – TERMO DE VERIFICAÇÃO PARA FINS DE SUBSTITUIÇÃO DA ECD

  TRegistroJ801 = class
  private
    fTIPO_DOC: String; // Tipo de documento
    fDESC_RTF: String; // Descrição do arquivo
    fCOD_MOT_SUBS: String; /// Código do motivo da substituição
    fHASH_RTF: String; // Hash do arquivo
    fARQ_RTF: String;  // Seqüência de bytes que representem um único arquivo no formato RTF (Rich Text Format).
  public
    property TIPO_DOC: String read fTIPO_DOC write fTIPO_DOC;
    property DESC_RTF: String read fDESC_RTF write fDESC_RTF;
    property COD_MOT_SUBS: String read fCOD_MOT_SUBS write fCOD_MOT_SUBS;
    property HASH_RTF: String read fHASH_RTF write fHASH_RTF;
    property ARQ_RTF: String read fARQ_RTF write fARQ_RTF;
  end;

  /// Registro J801 - Lista

  TRegistroJ801List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ801;
    procedure SetItem(Index: Integer; const Value: TRegistroJ801);
  public
    function New: TRegistroJ801;
    property Items[Index: Integer]: TRegistroJ801 read GetItem write SetItem;
  end;

(*
  /// Rregistro J800 – OUTRAS INFORMAÇÕES

  TRegistroJ800 = class
  private
    fARQ_RTF: String;  /// Seqüência de bytes que representem um único arquivo no formato RTF (Rich Text Format).
  public
    property ARQ_RTF: String read fARQ_RTF write fARQ_RTF;
  end;

  /// Registro J800 - Lista

  TRegistroJ800List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ800;
    procedure SetItem(Index: Integer; const Value: TRegistroJ800);
  public
    function New: TRegistroJ800;
    property Items[Index: Integer]: TRegistroJ800 read GetItem write SetItem;
  end;

  *)

  /// Rregistro J900 – TERMO DE ENCERRAMENTO

  TRegistroJ900 = class
  private
    fNUM_ORD: String;        /// Número de ordem do instrumento de escrituração.
    fNAT_LIVRO: String;      /// Natureza do livro; finalidade a que se destinou o instrumento.
    fNOME: String;           /// Nome empresarial.
    fQTD_LIN: Integer;           /// Quantidade total de linhas do arquivo digital.
    fDT_INI_ESCR: TDateTime;     /// Data de inicio da escrituração.
    fDT_FIN_ESCR: TDateTime;     /// Data de término da escrituração.
  public
    property NUM_ORD: String read fNUM_ORD write fNUM_ORD;
    property NAT_LIVRO: String read fNAT_LIVRO write fNAT_LIVRO;
    property NOME: String read fNOME write fNOME;
    property QTD_LIN: Integer read fQTD_LIN write fQTD_LIN;
    property DT_INI_ESCR: TDateTime read fDT_INI_ESCR write fDT_INI_ESCR;
    property DT_FIN_ESCR: TDateTime read fDT_FIN_ESCR write fDT_FIN_ESCR;
  end;


  /// Rregistro J930 – IDENTIFICAÇÃO DOS SIGNATÁRIOS DA ESCRITURAÇÃO

  TRegistroJ930 = class
  private
    fIDENT_NOM: String;      /// Nome do signatário.
    fIDENT_CPF: String;      /// CPF.
    fIDENT_QUALIF: String;   /// Qualificação do assinante, conforme tabela do Departamento Nacional de Registro do Comércio - DNRC.
    fCOD_ASSIN: String;      /// Código de qualificação do assinante, conforme tabela do Departamento Nacional de Registro do Comércio - DNRC.
    fIND_CRC: String;        /// Número de inscrição do contabilista no Conselho Regional de Contabilidade.
    fEMAIL: String;          /// Email do signatário
    fFONE: String;           /// Telefone do signatário.
    fUF_CRC: String;         /// Indicação da unidade da federação que expediu o CRC.
    fNUM_SEQ_CRC: String;    /// Número sequencial no seguinte formato: UF/ano/número
    fDT_CRC: TDateTime;      /// Data de validade do CRC do contador
    fIND_RESP_LEGAL: string;  /// Identificação do signatário que será validado como responsável legal da empresa junto as bases da RFB
  public
    property IDENT_NOM: String read fIDENT_NOM write fIDENT_NOM;
    property IDENT_CPF: String read fIDENT_CPF write fIDENT_CPF;
    property IDENT_QUALIF: String read fIDENT_QUALIF write fIDENT_QUALIF;
    property COD_ASSIN: String read fCOD_ASSIN write fCOD_ASSIN;
    property IND_CRC: String read fIND_CRC write fIND_CRC;
    property EMAIL: String read fEMAIL write fEMAIL;
    property FONE: String read fFONE write fFONE;
    property UF_CRC: String read fUF_CRC write fUF_CRC;
    property NUM_SEQ_CRC: String read fNUM_SEQ_CRC write fNUM_SEQ_CRC;
    property DT_CRC: TDateTime read fDT_CRC write fDT_CRC;
    property IND_RESP_LEGAL: String read fIND_RESP_LEGAL write fIND_RESP_LEGAL;
  end;

  /// Registro J930 - Lista

  TRegistroJ930List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ930;
    procedure SetItem(Index: Integer; const Value: TRegistroJ930);
  public
    function New: TRegistroJ930;
    property Items[Index: Integer]: TRegistroJ930 read GetItem write SetItem;
  end;

  /// Rregistro J932 – SIGNATÁRIOS DO TERMO DE VERIFICAÇÃO PARA FINS DE SUSBTITUIÇÃO DA ECD

  TRegistroJ932 = class
  private
    fIDENT_QUALIF_T: String;
    fEMAIL_T: String;
    fIND_CRC_T: String;
    fCOD_ASSIN_T: String;
    fUF_CRC_T: String;
    fNUM_SEQ_CRC_T: String;
    fIDENT_CPF_CNPJ_T: String;
    fIDENT_NOM_T: String;
    fFONE_T: String;
    fDT_CRC_T: TDateTime;
  public
    property IDENT_NOM_T: String read fIDENT_NOM_T write fIDENT_NOM_T;
    property IDENT_CPF_CNPJ_T: String read fIDENT_CPF_CNPJ_T write fIDENT_CPF_CNPJ_T;
    property IDENT_QUALIF_T: String read fIDENT_QUALIF_T write fIDENT_QUALIF_T;
    property COD_ASSIN_T: String read fCOD_ASSIN_T write fCOD_ASSIN_T;
    property IND_CRC_T: String read fIND_CRC_T write fIND_CRC_T;
    property EMAIL_T: String read fEMAIL_T write fEMAIL_T;
    property FONE_T: String read fFONE_T write fFONE_T;
    property UF_CRC_T: String read fUF_CRC_T write fUF_CRC_T;
    property NUM_SEQ_CRC_T: String read fNUM_SEQ_CRC_T write fNUM_SEQ_CRC_T;
    property DT_CRC_T: TDateTime read fDT_CRC_T write fDT_CRC_T;
  end;

  /// Registro J932 - Lista

  TRegistroJ932List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ932;
    procedure SetItem(Index: Integer; const Value: TRegistroJ932);
  public
    function New: TRegistroJ932;
    property Items[Index: Integer]: TRegistroJ932 read GetItem write SetItem;
  end;

  /// Rregistro J935 – IDENTIFICAÇÃO DOS AUDITORES INDEPENDENTES

  TRegistroJ935 = class
  private
    FCOD_CVM_AUDITOR: String;
    fNI_CPF_CNPJ: String;
    FNOME_AUDITOR: String;
    procedure SetCOD_CVM_AUDITOR(const Value: String);  // Nome do auditor independente.
    procedure SetNOME_AUDITOR(const Value: String);     // Registro do auditor independente na CVM.
    procedure setNI_CPF_CNPJ(const Value: String);     // CPF do auditor independente / CNPJ da pessoa jurídica de auditoria independente

  public
    property NOME_AUDITOR: String read FNOME_AUDITOR write SetNOME_AUDITOR;
    property NI_CPF_CNPJ: String read fNI_CPF_CNPJ write setNI_CPF_CNPJ;
    property COD_CVM_AUDITOR: String read FCOD_CVM_AUDITOR write SetCOD_CVM_AUDITOR;
  end;

  /// Registro J935 - Lista

  TRegistroJ935List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ935;
    procedure SetItem(Index: Integer; const Value: TRegistroJ935);
  public
    function New: TRegistroJ935;
    property Items[Index: Integer]: TRegistroJ935 read GetItem write SetItem;
  end;

  /// Registro J990 - ENCERRAMENTO DO BLOCO J

  TRegistroJ990 = class
  private
    fQTD_LIN_J: Integer;    /// Quantidade total de linhas do Bloco J
  public
    property QTD_LIN_J: Integer read FQTD_LIN_J write FQTD_LIN_J;
  end;

implementation

{ TRegistroJ005List }

function TRegistroJ005List.GetItem(Index: Integer): TRegistroJ005;
begin
  Result := TRegistroJ005(Inherited Items[Index]);
end;

function TRegistroJ005List.New: TRegistroJ005;
begin
  Result := TRegistroJ005.Create;
  Add(Result);
end;

procedure TRegistroJ005List.SetItem(Index: Integer; const Value: TRegistroJ005);
begin
  Put(Index, Value);
end;

{ TRegistroJ100List }

function TRegistroJ100List.GetItem(Index: Integer): TRegistroJ100;
begin
  Result := TRegistroJ100(Inherited Items[Index]);
end;

function TRegistroJ100List.LocalizaRegistro(const pCOD_AGL: String): boolean;
var
intFor: integer;
begin
   Result := false;
   for intFor := 0 to Self.Count - 1 do
   begin
      if Self.Items[intFor].COD_AGL = pCOD_AGL then
      begin
         Result := true;
         Break;
      end;
   end;
end;

function TRegistroJ100List.New: TRegistroJ100;
begin
  Result := TRegistroJ100.Create;
  Add(Result);
end;

procedure TRegistroJ100List.SetItem(Index: Integer; const Value: TRegistroJ100);
begin
  Put(Index, Value);
end;

{ TRegistroJ150List }

function TRegistroJ150List.GetItem(Index: Integer): TRegistroJ150;
begin
  Result := TRegistroJ150(Inherited Items[Index]);
end;

function TRegistroJ150List.LocalizaRegistro(const pCOD_AGL: String): boolean;
var
intFor: integer;
begin
   Result := false;
   for intFor := 0 to Self.Count - 1 do
   begin
      if Self.Items[intFor].COD_AGL = pCOD_AGL then
      begin
         Result := true;
         Break;
      end;
   end;
end;

function TRegistroJ150List.New: TRegistroJ150;
begin
  Result := TRegistroJ150.Create;
  Add(Result);
end;

procedure TRegistroJ150List.SetItem(Index: Integer; const Value: TRegistroJ150);
begin
  Put(Index, Value);
end;

{ TRegistroJ200List }

function TRegistroJ200List.GetItem(Index: Integer): TRegistroJ200;
begin
  Result := TRegistroJ200(Inherited Items[Index]);
end;

function TRegistroJ200List.LocalizaRegistro(const pCOD_HIST_FAT: String): boolean;
var
intFor: integer;
begin
   Result := false;
   for intFor := 0 to Self.Count - 1 do
   begin
      if Self.Items[intFor].COD_HIST_FAT = pCOD_HIST_FAT then
      begin
         Result := true;
         Break;
      end;
   end;
end;

function TRegistroJ200List.New: TRegistroJ200;
begin
  Result := TRegistroJ200.Create;
  Add(Result);
end;

procedure TRegistroJ200List.SetItem(Index: Integer; const Value: TRegistroJ200);
begin
  Put(Index, Value);
end;

{ TRegistroJ210List }

function TRegistroJ210List.GetItem(Index: Integer): TRegistroJ210;
begin
  Result := TRegistroJ210(Inherited Items[Index]);
end;

function TRegistroJ210List.LocalizaRegistro(const pCOD_AGL: String): boolean;
var
intFor: integer;
begin
   Result := false;
   for intFor := 0 to Self.Count - 1 do
   begin
      if Self.Items[intFor].COD_AGL = pCOD_AGL then
      begin
         Result := true;
         Break;
      end;
   end;
end;

function TRegistroJ210List.New: TRegistroJ210;
begin
  Result := TRegistroJ210.Create;
  Add(Result);
end;

procedure TRegistroJ210List.SetItem(Index: Integer; const Value: TRegistroJ210);
begin
  Put(Index, Value);
end;

{ TRegistroJ215List }

function TRegistroJ215List.GetItem(Index: Integer): TRegistroJ215;
begin
  Result := TRegistroJ215(Inherited Items[Index]);
end;

function TRegistroJ215List.New: TRegistroJ215;
begin
  Result := TRegistroJ215.Create;
  Add(Result);
end;

procedure TRegistroJ215List.SetItem(Index: Integer; const Value: TRegistroJ215);
begin
  Put(Index, Value);
end;

function TRegistroJ215List.LocalizaRegistro(const pCOD_HIST_FAT : string): boolean;
var
intFor: integer;
begin
   Result := false;
   for intFor := 0 to Self.Count - 1 do
   begin
      if Self.Items[intFor].COD_HIST_FAT = pCOD_HIST_FAT then
      begin
         Result := true;
         Break;
      end;
   end;
end;

{ TRegistroJ800List }

function TRegistroJ800List.GetItem(Index: Integer): TRegistroJ800;
begin
  Result := TRegistroJ800(Inherited Items[Index]);
end;

function TRegistroJ800List.New: TRegistroJ800;
begin
  Result := TRegistroJ800.Create;
  Add(Result);
end;

procedure TRegistroJ800List.SetItem(Index: Integer; const Value: TRegistroJ800);
begin
  Put(Index, Value);
end;


function TRegistroJ801List.GetItem(Index: Integer): TRegistroJ801;
begin
  Result := TRegistroJ801(Inherited Items[Index]);
end;

function TRegistroJ801List.New: TRegistroJ801;
begin
  Result := TRegistroJ801.Create;
  Add(Result);
end;

procedure TRegistroJ801List.SetItem(Index: Integer; const Value: TRegistroJ801);
begin
  Put(Index, Value);
end;


{ TRegistroJ930List }

function TRegistroJ930List.GetItem(Index: Integer): TRegistroJ930;
begin
  Result := TRegistroJ930(Inherited Items[Index]);
end;

function TRegistroJ930List.New: TRegistroJ930;
begin
  Result := TRegistroJ930.Create;
  Add(Result);
end;

procedure TRegistroJ930List.SetItem(Index: Integer; const Value: TRegistroJ930);
begin
  Put(Index, Value);
end;

{ TRegistroJ005 }

constructor TRegistroJ005.Create;
begin
   FRegistroJ100 := TRegistroJ100List.Create;
   FRegistroJ150 := TRegistroJ150List.Create;
   FRegistroJ200 := TRegistroJ200List.Create;
   FRegistroJ210 := TRegistroJ210List.Create;
   FRegistroJ800 := TRegistroJ800List.Create;
   FRegistroJ801 := TRegistroJ801List.Create;

end;

destructor TRegistroJ005.Destroy;
begin
  FRegistroJ100.Free;
  FRegistroJ150.Free;
  FRegistroJ200.Free;
  FRegistroJ210.Free;
  FRegistroJ800.Free;
  FRegistroJ801.Free;

  inherited;
end;

{ TRegistroJ210 }

constructor TRegistroJ210.Create;
begin
  FRegistroJ215 := TRegistroJ215List.Create;
end;

destructor TRegistroJ210.Destroy;
begin
  FRegistroJ215.Free;
  inherited;
end;

{ TRegistroJ935 }

procedure TRegistroJ935.SetCOD_CVM_AUDITOR(const Value: String);
begin
  FCOD_CVM_AUDITOR := Value;
end;

procedure TRegistroJ935.setNI_CPF_CNPJ(const Value: String);
begin
  fNI_CPF_CNPJ := Value;
end;

procedure TRegistroJ935.SetNOME_AUDITOR(const Value: String);
begin
  FNOME_AUDITOR := Value;
end;

{ TRegistroJ935List }

function TRegistroJ935List.GetItem(Index: Integer): TRegistroJ935;
begin
  Result := TRegistroJ935(Inherited Items[Index]);
end;

function TRegistroJ935List.New: TRegistroJ935;
begin
  Result := TRegistroJ935.Create;
  Add(Result);
end;

procedure TRegistroJ935List.SetItem(Index: Integer; const Value: TRegistroJ935);
begin
  Put(Index, Value);
end;

{ TRegistroJ932List }

function TRegistroJ932List.GetItem(Index: Integer): TRegistroJ932;
begin
  Result := TRegistroJ932(Inherited Items[Index]);
end;

function TRegistroJ932List.New: TRegistroJ932;
begin
  Result := TRegistroJ932.Create;
  Add(Result);
end;

procedure TRegistroJ932List.SetItem(Index: Integer;
  const Value: TRegistroJ932);
begin
  Put(Index, Value);
end;

end.
