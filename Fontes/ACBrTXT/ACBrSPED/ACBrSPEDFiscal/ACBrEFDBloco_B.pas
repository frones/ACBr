{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro e Renan Eustaquio               }
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

unit ACBrEFDBloco_B;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEFDBlocos;

type
  TRegistroB020List = class;
  TRegistroB025List = class;
  TRegistroB030List = class;
  TRegistroB035List = class;
  TRegistroB350List = class;
  TRegistroB420List = class;
  TRegistroB440List = class;
  TRegistroB460List = class;
  TRegistroB470List = class;
  TRegistroB500List = class;
  TRegistroB510List = class;

  /// Registro B001 - ABERTURA DO BLOCO B
  TRegistroB001 = class(TOpenBlocos)
  private
    FRegistroB020: TRegistroB020List;
    FRegistroB030: TRegistroB030List;
    FRegistroB350: TRegistroB350List;
    FRegistroB420: TRegistroB420List;
    FRegistroB440: TRegistroB440List;
    FRegistroB460: TRegistroB460List;
    FRegistroB470: TRegistroB470List;
    FRegistroB500: TRegistroB500List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy
    property RegistroB020: TRegistroB020List read FRegistroB020 write FRegistroB020;
    property RegistroB030: TRegistroB030List read FRegistroB030 write FRegistroB030;
    property RegistroB350: TRegistroB350List read FRegistroB350 write FRegistroB350;
    property RegistroB420: TRegistroB420List read FRegistroB420 write FRegistroB420;
    property RegistroB440: TRegistroB440List read FRegistroB440 write FRegistroB440;
    property RegistroB460: TRegistroB460List read FRegistroB460 write FRegistroB460;
    property RegistroB470: TRegistroB470List read FRegistroB470 write FRegistroB470;
    property RegistroB500: TRegistroB500List read FRegistroB500 write FRegistroB500;

  end;

  /// REGISTRO B020: NOTA FISCAL (CÓDIGO 01), NOTA FISCAL DE SERVIÇOS (CÓDIGO03), NOTA FISCAL DE SERVIÇOS AVULSA (CÓDIGO 3B),
  /// NOTA FISCAL DE PRODUTOR(CÓDIGO 04), CONHECIMENTO DE TRANSPORTE RODOVIÁRIO DE CARGAS
  /// (CÓDIGO 08), NF-e (CÓDIGO 55) e NFC-e (CÓDIGO 65).
 TRegistroB020 = class
  private
    fIND_OPER: TACBrIndOper;          /// Indicador do tipo de operação: 0- Aquisição; 1- Prestação
    fIND_EMIT: TACBrIndEmit;          /// Indicador do emitente do documento fiscal: 0- Emissão própria; 1- Terceiros
    fCOD_PART: String;                /// Código do participante (campo 02 do Registro 0150):
                                      ///   - do prestador, no caso de declarante na condição de tomador;
                                      ///   - do tomador, no caso de declarante na condição de prestador
    fCOD_MOD: String;                 /// Código do modelo do documento fiscal, conforme a Tabela 4.1.3
    fCOD_SIT: TACBrCodSit;            /// Código da situação do documento fiscal, conforme a Tabela 4.1.2
    fSER: String;                     /// Série do documento fiscal
    fNUM_DOC: String;                 /// Número do documento fiscal
    fCHV_NFE: String;                 /// Chave da Nota Fiscal Eletrônica
    fDT_DOC: TDateTime;               /// Data da emissão do documento fiscal
    fCOD_MUN_SERV: String;            /// Código do município onde o serviço foi prestado,conforme a tabela IBGE.
    fVL_CONT: currency;               /// Valor contábil (valor total do documento)
    fVL_MAT_TERC: currency;           /// Valor do material fornecido por terceiros na prestação do serviço
    fVL_SUB: currency;                /// Valor da subempreitada
    fVL_ISNT_ISS: currency;           /// Valor das operações isentas ou não-tributadas pelo ISS
    fVL_DED_BC: currency;             /// Valor da dedução da base de cálculo
    fVL_BC_ISS: currency;             /// Valor da base de cálculo do ISS
    fVL_BC_ISS_RT: currency;          /// Valor da base de cálculo de retenção do ISS
    fVL_ISS_RT: currency;             /// Valor do ISS retido pelo tomador
    fVL_ISS: currency;                /// Valor do ISS destacado
    fCOD_INF_OBS: String;             /// Código da observação do lançamento fiscal(campo 02 do Registro 0460)
    fRegistroB025: TRegistroB025List; /// Bloco b - Lista de RegistroB025 (FILHO)
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override;                       /// Destroy
    property IND_OPER: TACBrIndOper           read fIND_OPER      write fIND_OPER;
    property IND_EMIT: TACBrIndEmit           read fIND_EMIT      write fIND_EMIT;
    property COD_PART: String                 read fCOD_PART      write fCOD_PART;
    property COD_MOD: String                  read fCOD_MOD       write fCOD_MOD;
    property COD_SIT: TACBrCodSit             read fCOD_SIT       write fCOD_SIT;
    property SER: String                      read fSER           write fSER;
    property NUM_DOC: String                  read fNUM_DOC       write fNUM_DOC;
    property CHV_NFE: String                  read fCHV_NFE       write fCHV_NFE;
    property DT_DOC: TDateTime                read fDT_DOC        write fDT_DOC;
    property COD_MUN_SERV: String             read fCOD_MUN_SERV  write fCOD_MUN_SERV;
    property VL_CONT: currency                read fVL_CONT       write fVL_CONT;
    property VL_MAT_TERC: currency            read fVL_MAT_TERC   write fVL_MAT_TERC;
    property VL_SUB: currency                 read fVL_SUB        write fVL_SUB;
    property VL_ISNT_ISS: currency            read fVL_ISNT_ISS   write fVL_ISNT_ISS;
    property VL_DED_BC: currency              read fVL_DED_BC     write fVL_DED_BC;
    property VL_BC_ISS: currency              read fVL_BC_ISS     write fVL_BC_ISS;
    property VL_BC_ISS_RT: currency           read fVL_BC_ISS_RT  write fVL_BC_ISS_RT;
    property VL_ISS_RT: currency              read fVL_ISS_RT     write fVL_ISS_RT;
    property VL_ISS: currency                 read fVL_ISS        write fVL_ISS;
    property COD_INF_OBS: String              read fCOD_INF_OBS   write fCOD_INF_OBS;
    property RegistroB025: TRegistroB025List  read fRegistroB025  write fRegistroB025; // BLOCO B - Lista de RegistroB025
  end;

  /// Registro B020 - Lista
  TRegistroB020List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB020; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroB020); /// SetItem
  public
    function New(): TRegistroB020;
    property Items[Index: Integer]: TRegistroB020 read GetItem write SetItem;
  end;

  /// REGISTRO B025: DETALHAMENTO POR COMBINAÇÃO DE ALÍQUOTA E ITEM DA LISTA DE SERVIÇOS DA LC 116/2003)
  TRegistroB025 = class
  private
    fVL_CONT_P: currency;    /// Parcela correspondente ao "Valor Contábil" referente à combinação da alíquota e item da lista
    fVL_BC_ISS_P: currency;  /// Parcela correspondente ao "Valor da base de cálculo do ISS" referente à combinação da alíquota e item da lista
    fALIQ_ISS: currency;     /// Alíquota do ISS
    fVL_ISS_P: currency;     /// Parcela correspondente ao "Valor do ISS" referente à combinação da alíquota e item da lista
    fVL_ISNT_ISS_P: currency; /// Parcela correspondente ao "Valor das operações isentas ou nãotributadas pelo ISS" referente à combinação da alíquota e item da lista
    fCOD_SERV: String;         /// Item da lista de serviços, conforme Tabela 4.6.3
  public
    property VL_CONT_P: currency     read fVL_CONT_P     write fVL_CONT_P;
    property VL_BC_ISS_P: currency   read fVL_BC_ISS_P   write fVL_BC_ISS_P;
    property ALIQ_ISS: currency      read fALIQ_ISS      write fALIQ_ISS;
    property VL_ISS_P: currency      read fVL_ISS_P      write fVL_ISS_P;
    property VL_ISNT_ISS_P: currency read fVL_ISNT_ISS_P write fVL_ISNT_ISS_P;
    property COD_SERV: String        read fCOD_SERV      write fCOD_SERV;
  end;

  /// Registro B025 - Lista
  TRegistroB025List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB025; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroB025); /// SetItem
  public
    function New: TRegistroB025;
    property Items[Index: Integer]: TRegistroB025 read GetItem write SetItem;
  end;

  /// REGISTRO B030: NOTA FISCAL DE SERVIÇOS SIMPLIFICADA (CÓDIGO 3A)
 TRegistroB030 = class
  private
    fCOD_MOD: String;                 /// Código do modelo do documento fiscal, conforme a Tabela 4.1.3
    fSER: String;                     /// Série do documento fiscal
    fNUM_DOC_INI: String;             /// Número do primeiro documento fiscal emitido no dia
    fNUM_DOC_FIN: String;             /// Número do último documento fiscal emitido no dia
    fDT_DOC: TDateTime;               /// Data da emissão do documento fiscal
    fQTD_CANC: currency;              /// Quantidade de documentos cancelados
    fVL_CONT: currency;               /// Valor contábil (valor total do documento)
    fVL_ISNT_ISS: currency;           /// Valor das operações isentas ou não-tributadas pelo ISS
    fVL_BC_ISS: currency;             /// Valor da base de cálculo do ISS
    fVL_ISS: currency;                /// Valor do ISS destacado
    fCOD_INF_OBS: String;             /// Código da observação do lançamento fiscal(campo 02 do Registro 0460)
    fRegistroB035: TRegistroB035List; /// Bloco b - Lista de RegistroB035 (FILHO)
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override;                       /// Destroy
    property COD_MOD: String                  read fCOD_MOD       write fCOD_MOD;
    property SER: String                      read fSER           write fSER;
    property NUM_DOC_INI: String              read fNUM_DOC_INI   write fNUM_DOC_INI;
    property NUM_DOC_FIN: String              read fNUM_DOC_FIN   write fNUM_DOC_FIN;
    property DT_DOC: TDateTime                read fDT_DOC        write fDT_DOC;
    property QTD_CANC: currency               read fQTD_CANC      write fQTD_CANC;
    property VL_CONT: currency                read fVL_CONT       write fVL_CONT;
    property VL_ISNT_ISS: currency            read fVL_ISNT_ISS   write fVL_ISNT_ISS;
    property VL_BC_ISS: currency              read fVL_BC_ISS     write fVL_BC_ISS;
    property VL_ISS: currency                 read fVL_ISS        write fVL_ISS;
    property COD_INF_OBS: String              read fCOD_INF_OBS   write fCOD_INF_OBS;
    property RegistroB035: TRegistroB035List  read fRegistroB035  write fRegistroB035; // BLOCO B - Lista de RegistroB035
  end;

  /// Registro B030 - Lista
  TRegistroB030List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB030; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroB030); /// SetItem
  public
    function New(): TRegistroB030;
    property Items[Index: Integer]: TRegistroB030 read GetItem write SetItem;
  end;

  /// REGISTRO B035: DETALHAMENTO POR COMBINAÇÃO DE ALÍQUOTA E ITEM DA LISTA DE SERVIÇOS DA LC 116/2003)
  TRegistroB035 = class
  private
    fVL_CONT_P: currency;    /// Parcela correspondente ao "Valor Contábil" referente à combinação da alíquota e item da lista
    fVL_BC_ISS_P: currency;  /// Parcela correspondente ao "Valor da base de cálculo do ISS" referente à combinação da alíquota e item da lista
    fALIQ_ISS: currency;     /// Alíquota do ISS
    fVL_ISS_P: currency;     /// Parcela correspondente ao "Valor do ISS" referente à combinação da alíquota e item da lista
    fVL_ISNT_ISS_P: currency; /// Parcela correspondente ao "Valor das operações isentas ou nãotributadas pelo ISS" referente à combinação da alíquota e item da lista
    fCOD_SERV: String;         /// Item da lista de serviços, conforme Tabela 4.6.3
  public
    property VL_CONT_P: currency     read fVL_CONT_P     write fVL_CONT_P ;
    property VL_BC_ISS_P: currency   read fVL_BC_ISS_P   write fVL_BC_ISS_P;
    property ALIQ_ISS: currency      read fALIQ_ISS      write fALIQ_ISS;
    property VL_ISS_P: currency      read fVL_ISS_P      write fVL_ISS_P;
    property VL_ISNT_ISS_P: currency read fVL_ISNT_ISS_P write fVL_ISNT_ISS_P;
    property COD_SERV: String        read fCOD_SERV      write fCOD_SERV;
  end;

  /// Registro B035 - Lista
  TRegistroB035List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB035; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroB035); /// SetItem
  public
    function New: TRegistroB035;
    property Items[Index: Integer]: TRegistroB035 read GetItem write SetItem;
  end;

  //REGISTRO B350: SERVIÇOS PRESTADOS POR INSTITUIÇÕES FINANCEIRAS
  TRegistroB350 = class
  private
    fCOD_CTD: String;     /// Código da conta do plano de contas
    fCTA_ISS: String;     /// Descrição da conta no plano de contas
    fCTA_COSIF: String;   /// Código COSIF a que está subordinada a conta do ISS das instituições financeiras
    fQTD_OCOR: currency;  /// Quantidade de ocorrências na conta
    fCOD_SERV: String;    /// Item da lista de serviços, conforme Tabela 4.6.3.
    fVL_CONT: currency;   /// Valor contábil
    fVL_BC_ISS: currency; /// Valor da base de cálculo do ISS
    fALIQ_ISS: currency;  /// Alíquota do ISS
    fVL_ISS: currency;    /// Valor do ISS
    fCOD_INF_OBS: String; /// Código da observação do lançamento fiscal (campo 02 do Registro 0460)
  public
    constructor Create(AOwner: TRegistroB001); virtual; /// Create
    destructor Destroy; override;                       /// Destroy
    property COD_CTD: String     read fCOD_CTD     write fCOD_CTD;
    property CTA_ISS: String     read fCTA_ISS     write fCTA_ISS;
    property CTA_COSIF: String   read fCTA_COSIF   write fCTA_COSIF;
    property QTD_OCOR: currency  read fQTD_OCOR    write fQTD_OCOR;
    property COD_SERV: String    read fCOD_SERV    write fCOD_SERV;
    property VL_CONT: currency   read fVL_CONT     write fVL_CONT;
    property VL_BC_ISS: currency read fVL_BC_ISS   write fVL_BC_ISS;
    property ALIQ_ISS: currency  read fALIQ_ISS    write fALIQ_ISS;
    property VL_ISS: currency    read fVL_ISS      write fVL_ISS;
    property COD_INF_OBS: String read fCOD_INF_OBS write fCOD_INF_OBS;
  end;

  /// Registro B350 - Lista
  TRegistroB350List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB350; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroB350); /// SetItem
  public
    function New(AOwner: TRegistroB001): TRegistroB350;
    property Items[Index: Integer]: TRegistroB350 read GetItem write SetItem;
  end;

  //REGISTRO B420: TOTALIZAÇÃO DOS VALORES DE SERVIÇOS PRESTADOS POR COMBINAÇÃO DE ALÍQUOTA E ITEM DA LISTA DE SERVIÇOS DA LC 116/2003
  TRegistroB420 = class
  private
    fVL_CONT: currency;     /// Totalização do Valor Contábil das prestações do declarante referente à combinação da alíquota e item da lista
    fVL_BC_ISS: currency;   /// Totalização do Valor da base de cálculo do ISS das prestações do declarante referente à combinação da alíquota e item da lista
    fALIQ_ISS: currency;    /// Alíquota do ISS
    fVL_ISNT_ISS: currency; /// Totalização do valor das operações isentas ou não-tributadas pelo ISS referente à combinação da alíquota e item da lista
    fVL_ISS: currency;      /// Totalização, por combinação da alíquota e item da lista, do Valor do ISS
    fCOD_SERV: String;      /// Item da lista de serviços, conforme Tabela 4.6.3.
  public
    constructor Create(AOwner: TRegistroB001); virtual; /// Create
    destructor Destroy; override;                       /// Destroy
    property VL_CONT: currency      read fVL_CONT     write fVL_CONT;
    property VL_BC_ISS: currency    read fVL_BC_ISS   write fVL_BC_ISS;
    property ALIQ_ISS: currency     read fALIQ_ISS    write fALIQ_ISS;
    property VL_ISNT_ISS: currency  read fVL_ISNT_ISS write fVL_ISNT_ISS;
    property VL_ISS: currency       read fVL_ISS      write fVL_ISS;
    property COD_SERV: String       read fCOD_SERV    write fCOD_SERV;
  end;

  /// Registro B420 - Lista
  TRegistroB420List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB420; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroB420); /// SetItem
  public
    function New(AOwner: TRegistroB001): TRegistroB420;
    property Items[Index: Integer]: TRegistroB420 read GetItem write SetItem;
  end;

  //REGISTRO B440: TOTALIZAÇÃO DOS VALORES RETIDOS
  TRegistroB440 = class
  private
    fIND_OPER:TACBrIndOper;  /// Indicador do tipo de operação: 0- Aquisição; 1- Prestação
    fCOD_PART: String;       /// Código do participante (campo 02 do Registro 0150):- do prestador, no caso de aquisição de serviço pelo declarante; - do tomador, no caso de prestação de serviço pelo declarante
    fVL_CONT_RT: currency;   /// Totalização do Valor Contábil das prestações e/ou aquisições do declarante pela combinação de tipo de operação e participante.
    fVL_BC_ISS_RT: currency; /// Totalização do Valor da base de cálculo de retenção do ISS das prestações e/ou aquisições do declarante pela combinação de tipo de operação e participante.
    fVL_ISS_RT: currency;    /// Totalização do Valor do ISS retido pelo tomador das prestações e/ou aquisições do declarante pela combinação de tipo de operação e participante.
  public
    constructor Create(AOwner: TRegistroB001); virtual; /// Create
    destructor Destroy; override;                       /// Destroy
    property IND_OPER:TACBrIndOper  read fIND_OPER     write fIND_OPER;
    property COD_PART: String       read fCOD_PART     write fCOD_PART;
    property VL_CONT_RT: currency   read fVL_CONT_RT   write fVL_CONT_RT;
    property VL_BC_ISS_RT: currency read fVL_BC_ISS_RT write fVL_BC_ISS_RT;
    property VL_ISS_RT: currency    read fVL_ISS_RT    write fVL_ISS_RT;
  end;

  /// Registro B440 - Lista
  TRegistroB440List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB440; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroB440); /// SetItem
  public
    function New(AOwner: TRegistroB001): TRegistroB440;
    property Items[Index: Integer]: TRegistroB440 read GetItem write SetItem;
  end;

  //REGISTRO B460: DEDUÇÕES DO ISS
  TRegistroB460 = class
  private
   fIND_DED:TACBrIndicadorDeducao;   /// Indicador do tipo de dedução: 0- Compensação do ISS calculado a maior; 1- Benefício fiscal por incentivo à cultura; 2- Decisão administrativa ou judicial; 9- Outros
   fVL_DED: currency;                /// Valor da dedução
   fNUM_PROC: String;                /// Número do processo ao qual o ajuste está vinculado, se houver
   fIND_PROC:TACBrIndicadorProcesso; /// Indicador da origem do processo: 0- Sefin; 1- Justiça Federal; 2- Justiça Estadual; 9- Outros
   fPROC: String;                    /// Descrição do processo que embasou o lançamento
   fCOD_INF_OBS: String;             /// Código da observação do lançamento fiscal (campo 02 do Registro 0460)
   fIND_OBR:TACBrIndicadorObrigacao; /// Indicador da obrigação onde será aplicada a dedução: 0 - ISS Próprio; 1 - ISS Substituto (devido pelas aquisições de serviços do declarante).2 - ISS Uniprofissionais.
  public
   constructor Create(AOwner: TRegistroB001); virtual; /// Create
   destructor Destroy; override;                       /// Destroy
   property IND_DED:TACBrIndicadorDeducao   read fIND_DED     write fIND_DED;
   property VL_DED: currency                read fVL_DED      write fVL_DED;
   property NUM_PROC: String                read fNUM_PROC    write fNUM_PROC;
   property IND_PROC:TACBrIndicadorProcesso read fIND_PROC    write fIND_PROC;
   property PROC: String                    read fPROC        write fPROC;
   property COD_INF_OBS: String             read fCOD_INF_OBS write fCOD_INF_OBS;
   property IND_OBR:TACBrIndicadorObrigacao read fIND_OBR     write fIND_OBR;
  end;

  /// Registro B460 - Lista
  TRegistroB460List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB460; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroB460); /// SetItem
  public
    function New(AOwner: TRegistroB001): TRegistroB460;
    property Items[Index: Integer]: TRegistroB460 read GetItem write SetItem;
  end;

  //REGISTRO B470: APURAÇÃO DO ISS
  TRegistroB470 = class
  private
    fVL_CONT: currency;        /// A - Valor total referente às prestações de serviço do período
    fVL_MAT_TERC: currency;    /// B - Valor total do material fornecido por terceiros na prestação do serviço
    fVL_MAT_PROP: currency;    /// C - Valor do material próprio utilizado na prestação do serviço
    fVL_SUB: currency;         /// D - Valor total das subempreitadas N - 02 O
    fVL_ISNT: currency;        /// E - Valor total das operações isentas ou não-tributadas pelo ISS
    fVL_DED_BC: currency;      /// F - Valor total das deduções da base de cálculo (B + C + D + E)
    fVL_BC_ISS: currency;      /// G - Valor total da base de cálculo do ISS N - 02 O
    fVL_BC_ISS_RT: currency;   /// H - Valor total da base de cálculo de retenção do ISS referente às prestações do declarante.
    fVL_ISS: currency;         /// I - Valor total do ISS destacado N - 02 O
    fVL_ISS_RT: currency;      /// J - Valor total do ISS retido pelo tomador nas prestações do declarante
    fVL_DED: currency;         /// K - Valor total das deduções do ISS próprio
    fVL_ISS_REC: currency;     /// L - Valor total apurado do ISS próprio a recolher (I - J - K)
    fVL_ISS_ST: currency;      /// M - Valor total do ISS substituto a recolher pelas aquisições do declarante(tomador)
    fVL_ISS_REC_UNI: currency; /// U Valor do ISS próprio a recolher pela Sociedade Uniprofissional
  public
    constructor Create(AOwner: TRegistroB001); virtual; /// Create
    destructor Destroy; override;                       /// Destroy
    property VL_CONT: currency        read fVL_CONT        write fVL_CONT;
    property VL_MAT_TERC: currency    read fVL_MAT_TERC    write fVL_MAT_TERC;
    property VL_MAT_PROP: currency    read fVL_MAT_PROP    write fVL_MAT_PROP;
    property VL_SUB: currency         read fVL_SUB         write fVL_SUB;
    property VL_ISNT: currency        read fVL_ISNT        write fVL_ISNT;
    property VL_DED_BC: currency      read fVL_DED_BC      write fVL_DED_BC;
    property VL_BC_ISS: currency      read fVL_BC_ISS      write fVL_BC_ISS;
    property VL_BC_ISS_RT: currency   read fVL_BC_ISS_RT   write fVL_BC_ISS_RT;
    property VL_ISS: currency         read fVL_ISS         write fVL_ISS;
    property VL_ISS_RT: currency      read fVL_ISS_RT      write fVL_ISS_RT;
    property VL_DED: currency         read fVL_DED         write fVL_DED;
    property VL_ISS_REC: currency     read fVL_ISS_REC     write fVL_ISS_REC;
    property VL_ISS_ST: currency      read fVL_ISS_ST      write fVL_ISS_ST;
    property VL_ISS_REC_UNI: currency read fVL_ISS_REC_UNI write fVL_ISS_REC_UNI;
  end;

  /// Registro B470 - Lista
  TRegistroB470List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB470; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroB470); /// SetItem
  public
    function New(AOwner: TRegistroB001): TRegistroB470;
    property Items[Index: Integer]: TRegistroB470 read GetItem write SetItem;
  end;

 //REGISTRO B500: APURAÇÃO DO ISS SOCIEDADE UNIPROFISSIONAL
  TRegistroB500 = class
  private
    fVL_REC: currency;   ///Valor mensal das receitas auferidas pela sociedade uniprofissional
    fQTD_PROF: currency; ///Quantidade de profissionais habilitados
    fVL_OR: currency;    //Valor do ISS devido
    FRegistroB510 :TRegistroB510List;
  public
    constructor Create(); virtual; /// Create
    destructor Destroy; override; /// Destroy
    property VL_REC: currency   read fVL_REC   write fVL_REC;
    property QTD_PROF: currency read fQTD_PROF write fQTD_PROF;
    property VL_OR: currency    read fVL_OR    write fVL_OR;
    property RegistroB510: TRegistroB510List read FRegistroB510 write FRegistroB510;
  end;

  /// Registro B500 - Lista
  TRegistroB500List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB500; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroB500); /// SetItem
  public
    function New(): TRegistroB500;
    property Items[Index: Integer]: TRegistroB500 read GetItem write SetItem;
  end;

 //REGISTRO B510: APURAÇÃO DO ISS SOCIEDADE UNIPROFISSIONAL
  TRegistroB510 = class
  private
    fIND_PROF: String; /// Indicador de habilitação: 0- Profissional habilitado 1- Profissional não habilitado
    fIND_ESC: String;  /// Indicador de escolaridade: 0- Nível superior 1- Nível médio
    fIND_SOC: String;  /// Indicador de participação societária: 0- Sócio 1- Não sócio
    fCPF: String;      /// Número de inscrição do profissional no CPF.
    fNOME: String;     /// Nome do profissional
  public
    property IND_PROF: String read fIND_PROF write fIND_PROF;
    property IND_ESC: String  read fIND_ESC  write fIND_ESC;
    property IND_SOC: String  read fIND_SOC  write fIND_SOC;
    property CPF: String      read fCPF      write fCPF;
    property NOME: String     read fNOME     write fNOME;
  end;

  /// Registro B510 - Lista
  TRegistroB510List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB510; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroB510); /// SetItem
  public
    function New: TRegistroB510;
    property Items[Index: Integer]: TRegistroB510 read GetItem write SetItem;
  end;


  /// Registro B990 - ENCERRAMENTO DO BLOCO B

  TRegistroB990 = class
  private
    fQTD_LIN_B: Integer; /// Quantidade total de linhas do Bloco B
  public
    property QTD_LIN_B: Integer read fQTD_LIN_B write fQTD_LIN_B;
  end;

implementation


{ TRegistroB001 }

constructor TRegistroB001.Create;
begin
  inherited Create;
  FRegistroB020 := TRegistroB020List.Create;
  FRegistroB030 := TRegistroB030List.Create;
  FRegistroB350 := TRegistroB350List.Create;
  FRegistroB420 := TRegistroB420List.Create;
  FRegistroB440 := TRegistroB440List.Create;
  FRegistroB460 := TRegistroB460List.Create;
  FRegistroB470 := TRegistroB470List.Create;
  FRegistroB500 := TRegistroB500List.Create;
  IND_MOV := imSemDados;
end;

destructor TRegistroB001.Destroy;
begin
  FRegistroB020.free;
  FRegistroB030.free;
  FRegistroB350.free;
  FRegistroB420.free;
  FRegistroB440.free;
  FRegistroB460.free;
  FRegistroB470.free;
  FRegistroB500.free;
  inherited;
end;

{ TRegistroB020 }

constructor TRegistroB020.Create();
begin
  inherited Create;
  FRegistroB025 := TRegistroB025List.Create;
end;

destructor TRegistroB020.Destroy;
begin
  FRegistroB025.free;
  inherited;
end;

{ TRegistroB020List }

function TRegistroB020List.GetItem(Index: Integer): TRegistroB020;
begin
  Result := TRegistroB020(Inherited Items[Index]);
end;

function TRegistroB020List.New(): TRegistroB020;
begin
  Result := TRegistroB020.Create();
  Add(Result);
end;

procedure TRegistroB020List.SetItem(Index: Integer; const Value: TRegistroB020);
begin
  Put(Index, Value);
end;

{ TRegistroB025List }

function TRegistroB025List.GetItem(Index: Integer): TRegistroB025;
begin
  Result := TRegistroB025(Inherited Items[Index]);
end;

function TRegistroB025List.New: TRegistroB025;
begin
  Result := TRegistroB025.Create;
  Add(Result);
end;

procedure TRegistroB025List.SetItem(Index: Integer; const Value: TRegistroB025);
begin
  Put(Index, Value);
end;

{ TRegistroB030 }

constructor TRegistroB030.Create();
begin
  inherited Create;
  FRegistroB035 := TRegistroB035List.Create;
end;

destructor TRegistroB030.Destroy;
begin
  FRegistroB035.Free;
  inherited;
end;

{ TRegistroB030List }

function TRegistroB030List.GetItem(Index: Integer): TRegistroB030;
begin
  Result := TRegistroB030(Inherited Items[Index]);
end;

function TRegistroB030List.New(): TRegistroB030;
begin
  Result := TRegistroB030.Create();
  Add(Result);
end;

procedure TRegistroB030List.SetItem(Index: Integer; const Value: TRegistroB030);
begin
  Put(Index, Value);
end;

{ TRegistroB035List }

function TRegistroB035List.GetItem(Index: Integer): TRegistroB035;
begin
  Result := TRegistroB035(Inherited Items[Index]);
end;

function TRegistroB035List.New: TRegistroB035;
begin
  Result := TRegistroB035.Create;
  Add(Result);
end;

procedure TRegistroB035List.SetItem(Index: Integer; const Value: TRegistroB035);
begin
  Put(Index, Value);
end;

{ TRegistroB350List }

function TRegistroB350List.GetItem(Index: Integer): TRegistroB350;
begin
  Result := TRegistroB350(Inherited Items[Index]);
end;

function TRegistroB350List.New(AOwner: TRegistroB001): TRegistroB350;
begin
  Result := TRegistroB350.Create(AOwner);
  Add(Result);

end;

procedure TRegistroB350List.SetItem(Index: Integer; const Value: TRegistroB350);
begin
  Put(Index, Value);
end;

{ TRegistroB420List }

function TRegistroB420List.GetItem(Index: Integer): TRegistroB420;
begin
  Result := TRegistroB420(Inherited Items[Index]);
end;

function TRegistroB420List.New(AOwner: TRegistroB001): TRegistroB420;
begin
  Result := TRegistroB420.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB420List.SetItem(Index: Integer; const Value: TRegistroB420);
begin
  Put(Index, Value);
end;

{ TRegistroBB440List }

function TRegistroB440List.GetItem(Index: Integer): TRegistroB440;
begin
  Result := TRegistroB440(Inherited Items[Index]);
end;

function TRegistroB440List.New(AOwner: TRegistroB001): TRegistroB440;
begin
  Result := TRegistroB440.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB440List.SetItem(Index: Integer; const Value: TRegistroB440);
begin
  Put(Index, Value);
end;

{ TRegistroB460List }
function TRegistroB460List.GetItem(Index: Integer): TRegistroB460;
begin
  Result := TRegistroB460(Inherited Items[Index]);
end;

function TRegistroB460List.New(AOwner: TRegistroB001): TRegistroB460;
begin
  Result := TRegistroB460.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB460List.SetItem(Index: Integer; const Value: TRegistroB460);
begin
  Put(Index, Value);
end;

function TRegistroB470List.GetItem(Index: Integer): TRegistroB470;
begin
  Result := TRegistroB470(Inherited Items[Index]);
end;

function TRegistroB470List.New(AOwner: TRegistroB001): TRegistroB470;
begin
  Result := TRegistroB470.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB470List.SetItem(Index: Integer; const Value: TRegistroB470);
begin
  Put(Index, Value);
end;

{ TRegistroB500List }

function TRegistroB500List.GetItem(Index: Integer): TRegistroB500;
begin
  Result := TRegistroB500(Inherited Items[Index]);
end;

function TRegistroB500List.New(): TRegistroB500;
begin
  Result := TRegistroB500.Create();
  Add(Result);
end;

procedure TRegistroB500List.SetItem(Index: Integer; const Value: TRegistroB500);
begin
  Put(Index, Value);
end;

{ TRegistroB510List }

function TRegistroB510List.GetItem(Index: Integer): TRegistroB510;
begin
  Result := TRegistroB510(Inherited Items[Index]);
end;

function TRegistroB510List.New: TRegistroB510;
begin
  Result := TRegistroB510.Create;
  Add(Result);
end;

procedure TRegistroB510List.SetItem(Index: Integer; const Value: TRegistroB510);
begin
  Put(Index, Value);
end;

{ TRegistroB500 }

constructor TRegistroB500.Create();
begin
  inherited Create;
  FRegistroB510 := TRegistroB510List.Create;  /// BLOCO B - Lista de RegistroB510 (FILHO)
end;

destructor TRegistroB500.Destroy;
begin
  FRegistroB510.Free;
  inherited;
end;

{ TRegistroB350 }

constructor TRegistroB350.Create(AOwner: TRegistroB001);
begin

end;

destructor TRegistroB350.Destroy;
begin

  inherited;
end;

{ TRegistroB440 }

constructor TRegistroB440.Create(AOwner: TRegistroB001);
begin

end;

destructor TRegistroB440.Destroy;
begin

  inherited;
end;

{ TRegistroB460 }

constructor TRegistroB460.Create(AOwner: TRegistroB001);
begin

end;

destructor TRegistroB460.Destroy;
begin

  inherited;
end;

{ TRegistroB470 }

constructor TRegistroB470.Create(AOwner: TRegistroB001);
begin

end;

destructor TRegistroB470.Destroy;
begin

  inherited;
end;

{ TRegistroB420 }

constructor TRegistroB420.Create(AOwner: TRegistroB001);
begin

end;

destructor TRegistroB420.Destroy;
begin

  inherited;
end;

end.
