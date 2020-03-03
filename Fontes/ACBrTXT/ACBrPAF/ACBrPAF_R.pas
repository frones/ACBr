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

unit ACBrPAF_R;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils;

type
  TRegistroR03List = class;
  TRegistroR05List = class;
  TRegistroR07List = class;
  TRegistroR07xList = class;

  TRegistroR02List = class;
  TRegistroR06List =class;
  TRegistroR04List =class;

  /// REGISTRO TIPO R01 - IDENTIFICAÇÃO DO ECF, DO USUÁRIO, DO PAF-ECF E DA EMPRESA DESENVOLVEDORA E DADOS DO ARQUIVO

  { TRegistroR01 }

  TRegistroR01 = class
  private
    fRegistroValido: boolean;
    fNUM_FAB: string;        /// Número de fabricação do ECF
    fMF_ADICIONAL: string;   /// Letra indicativa de MF adicional
    fTIPO_ECF: string;       /// Tipo de ECF
    fMARCA_ECF: string;      /// Marca do ECF
    fMODELO_ECF: string;     /// Modelo do ECF
    fVERSAO_SB: string;      /// Versão atual do Software Básico do ECF gravada na MF
    fDT_INST_SB: TDateTime;  /// Data de instalação da versão atual do Software Básico gravada naMemória Fiscal do ECF
    fHR_INST_SB: TDateTime;  /// Horário de instalação da versão atual do Software Básico gravada na Memória Fiscal do ECF
    fNUM_SEQ_ECF: integer;   /// Nº de ordem seqüencial do ECF no estabelecimento usuário
    fCNPJ: string;           /// CNPJ do estabelecimento usuário do ECF
    fIE: string;             /// Inscrição Estadual do estabelecimento usuário
    fCNPJ_SH: string;        /// CNPJ da empresa desenvolvedora do PAF-ECF
    fIE_SH: string;          /// Inscrição Estadual da empresa desenvolvedora do PAF-ECF, se houver
    fIM_SH: string;          /// Inscrição Municipal da empresa desenvolvedora do PAF-ECF, se houver
    fNOME_SH: string;        /// Denominação da empresa desenvolvedora do PAF-ECF
    fNOME_PAF: string;       /// Nome Comercial do PAF-ECF
    fVER_PAF: string;        /// Versão atual do PAF-ECF
    fCOD_MD5: string;        /// Código MD-5 da Lista de Arquivos Autenticados do PAF-ECF
    fDT_INI: TDateTime;      /// Data do início do período informado no arquivo
    fDT_FIN: TDateTime;      /// Data do fim do período informado no arquivo
    fER_PAF_ECF: string;     /// Versão da Especificação de Requisitos do PAF-ECF
    fInclusaoExclusao: Boolean;
    FRegistroR02: TRegistroR02List;
    FRegistroR06: TRegistroR06List;
    FRegistroR04: TRegistroR04List;
    procedure SetRegistroR02(const Value: TRegistroR02List);
    procedure SetRegistroR04(const Value: TRegistroR04List);
    procedure SetRegistroR06(const Value: TRegistroR06List); /// ER 1.08 inclusão/exclusão de registros
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property InclusaoExclusao: Boolean read fInclusaoExclusao write fInclusaoExclusao default False;
    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property NUM_FAB: string read FNUM_FAB write FNUM_FAB;
    property MF_ADICIONAL: string read fMF_ADICIONAL write fMF_ADICIONAL;
    property TIPO_ECF: string read FTIPO_ECF write FTIPO_ECF;
    property MARCA_ECF: string read FMARCA_ECF write FMARCA_ECF;
    property MODELO_ECF: string read FMODELO_ECF write FMODELO_ECF;
    property VERSAO_SB: string read FVERSAO_SB write FVERSAO_SB;
    property DT_INST_SB: TDateTime read FDT_INST_SB write FDT_INST_SB;
    property HR_INST_SB: TDateTime read FHR_INST_SB write FHR_INST_SB;
    property NUM_SEQ_ECF: integer read FNUM_SEQ_ECF write FNUM_SEQ_ECF;
    property CNPJ: string read FCNPJ write FCNPJ;
    property IE: string read FIE write FIE;
    property CNPJ_SH: string read FCNPJ_SH write FCNPJ_SH;
    property IE_SH: string read FIE_SH write FIE_SH;
    property IM_SH: string read FIM_SH write FIM_SH;
    property NOME_SH: string read FNOME_SH write FNOME_SH;
    property NOME_PAF: string read FNOME_PAF write FNOME_PAF;
    property VER_PAF: string read FVER_PAF write FVER_PAF;
    property COD_MD5: string read FCOD_MD5 write FCOD_MD5;
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
    property ER_PAF_ECF: string read FER_PAF_ECF write FER_PAF_ECF;

    property RegistroR02 : TRegistroR02List read FRegistroR02 write SetRegistroR02;
    property RegistroR04 : TRegistroR04List read FRegistroR04 write SetRegistroR04;
    property RegistroR06 : TRegistroR06List read FRegistroR06 write SetRegistroR06;

  end;

  TRegistroR01List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroR01;
    procedure SetItem(Index: Integer; const Value: TRegistroR01);
  public
    function New: TRegistroR01;
    property Items[Index: Integer]: TRegistroR01 read GetItem write SetItem;
  end;

  /// REGISTRO TIPO R02 - RELAÇÃO DE REDUÇÕES Z

  TRegistroR02 = class
  private
    fRegistroValido: boolean;
    fNUM_USU: integer;       /// Nº de ordem do usuário do ECF relativo à respectiva Redução Z
    fCRZ: integer;           /// Nº do Contador de Redução Z relativo à respectiva redução
    fCOO: integer;           /// Nº do Contador de Ordem de Operação relativo à respectiva Redução Z
    fCRO: integer;           /// Nº do Contador de Reinício de Operação relativo à respectiva Redução Z
    fDT_MOV: TDateTime;      /// Data das operações relativas à respectiva Redução Z
    fDT_EMI: TDateTime;      /// Data de emissão da Redução Z
    fHR_EMI: TDateTime;      /// Hora de emissão da Redução Z
    fVL_VBD: currency;       /// Valor acumulado neste totalizador relativo à respectiva Redução Z, com duas casas decimais.
    fPAR_ECF: string;        /// Parâmetro do ECF para incidência de desconto sobre itens sujeitos ao ISSQN conforme item 7.2.1.4

    fRegistroR03: TRegistroR03List; /// Registro FILHO
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property NUM_USU: integer read FNUM_USU write FNUM_USU;
    property CRZ: integer read FCRZ write FCRZ;
    property COO: integer read FCOO write FCOO;
    property CRO: integer read FCRO write FCRO;
    property DT_MOV: TDateTime read FDT_MOV write FDT_MOV;
    property DT_EMI: TDateTime read FDT_EMI write FDT_EMI;
    property HR_EMI: TDateTime read FHR_EMI write FHR_EMI;
    property VL_VBD: currency read FVL_VBD write FVL_VBD;
    property PAR_ECF: string read FPAR_ECF write FPAR_ECF;

    property RegistroR03: TRegistroR03List read FRegistroR03 write FRegistroR03;

  end;

  /// REGISTRO R02 - Lista

  TRegistroR02List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroR02;
    procedure SetItem(Index: Integer; const Value: TRegistroR02);
  public
    function New: TRegistroR02;
    property Items[Index: Integer]: TRegistroR02 read GetItem write SetItem;
  end;

  /// REGISTRO TIPO R03 - DETALHE DA REDUÇÃO Z

  TRegistroR03 = class
  private
    fRegistroValido: boolean;
    fTOT_PARCIAL: string;       /// Código do totalizador conforme tabela abaixo
    fVL_ACUM: currency;         /// Valor acumulado no totalizador, relativo à respectiva Redução Z, com duas casas decimais
  public
    constructor Create; virtual; /// Create

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property TOT_PARCIAL: string read FTOT_PARCIAL write FTOT_PARCIAL;
    property VL_ACUM: currency read FVL_ACUM write FVL_ACUM;
  end;

  /// REGISTRO R03 - Lista

  TRegistroR03List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroR03;
    procedure SetItem(Index: Integer; const Value: TRegistroR03);
  public
    function New: TRegistroR03;
    property Items[Index: Integer]: TRegistroR03 read GetItem write SetItem;
  end;

  /// REGISTRO TIPO R04 - CUPOM FISCAL, NOTA FISCAL DE VENDA A CONSUMIDOR E BILHETE DE PASSAGEM

  TRegistroR04 = class
  private
    fRegistroValido: boolean;
    fNUM_USU: integer;          /// Nº de ordem do usuário do ECF
    fNUM_CONT: integer;         /// Nº do contador do respectivo documento emitido
    fCOO: integer;              /// Nº do COO relativo ao respectivo documento
    fDT_INI: TDateTime;         /// Data de início da emissão do documento impressa no cabeçalho do documento
    fSUB_DOCTO: currency;       /// Valor total do documento, com  duas casas decimais.
    fSUB_DESCTO: currency;      /// Valor do desconto ou Percentual aplicado sobre o valor do subtotal do documento, com duas casas decimais.
    fTP_DESCTO: string;         /// Informar “V” para valor monetário ou “P” para percentual
    fSUB_ACRES: currency;       /// Valor do acréscimo ou Percentual aplicado sobre o valor do subtotal do documento, com duas casas decimais
    fTP_ACRES: string;          /// Informar “V”  para valor monetário ou “P” para percentual
    fVL_TOT: currency;          /// Valor total do Cupom Fiscal após desconto/acréscimo, com duas casas decimais.
    fCANC: string;              /// Informar "S" ou "N", conforme tenha ocorrido ou não, o cancelamento do documento.
    fVL_CA: currency;           /// Valor do cancelamento de acréscimo no subtotal
    fORDEM_DA: string;          /// Indicador de ordem de aplicação de desconto/acréscimo em Subtotal. ‘D’ ou ‘A’ caso tenha ocorrido primeiro desconto ou acréscimo, respectivamente
    fNOME_CLI: string;          /// Nome do Cliente
    fCNPJ_CPF: string;          /// CPF ou CNPJ do adquirente

    fRegistroR05: TRegistroR05List; /// Registro FILHO
    fRegistroR07: TRegistroR07List; /// Registro FILHO
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property NUM_USU: integer read FNUM_USU write FNUM_USU;
    property NUM_CONT: integer read FNUM_CONT write FNUM_CONT;
    property COO: integer read FCOO write FCOO;
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property SUB_DOCTO: currency read FSUB_DOCTO write FSUB_DOCTO;
    property SUB_DESCTO: currency read FSUB_DESCTO write FSUB_DESCTO;
    property TP_DESCTO: string read FTP_DESCTO write FTP_DESCTO;
    property SUB_ACRES: currency read FSUB_ACRES write FSUB_ACRES;
    property TP_ACRES: string read FTP_ACRES write FTP_ACRES;
    property VL_TOT: currency read FVL_TOT write FVL_TOT;
    property CANC: string read FCANC write FCANC;
    property VL_CA: currency read FVL_CA write FVL_CA;
    property ORDEM_DA: string read FORDEM_DA write FORDEM_DA;
    property NOME_CLI: string read FNOME_CLI write FNOME_CLI;
    property CNPJ_CPF: string read FCNPJ_CPF write FCNPJ_CPF;

    property RegistroR05: TRegistroR05List read FRegistroR05 write FRegistroR05;
    property RegistroR07: TRegistroR07List read FRegistroR07 write FRegistroR07;
  end;

  /// REGISTRO R04 - Lista

  TRegistroR04List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroR04;
    procedure SetItem(Index: Integer; const Value: TRegistroR04);
  public
    function New: TRegistroR04;
    property Items[Index: Integer]: TRegistroR04 read GetItem write SetItem;
  end;

  /// REGISTRO TIPO R05 - DETALHE DO CUPOM FISCAL, DA NOTA FISCAL DE VENDA A CONSUMIDOR OU DO BILHETE DE PASSAGEM

  { TRegistroR05 }

  TRegistroR05 = class
  private
    FNUM_CONT: integer;
    fRegistroValido: boolean;
    fNUM_ITEM: integer;         /// Número do item registrado no documento
    fCOD_ITEM: string;          /// Código do produto ou serviço registrado no documento
    fDESC_ITEM: string;         /// Descrição do produto ou serviço constante no Cupom Fiscal
    fQTDE_ITEM: currency;       /// Quantidade comercializada, sem a separação das casas decimais
    fUN_MED: string;            /// Unidade de medida
    fVL_UNIT: currency;         /// Valor unitário do produto ou serviço, sem a separação das casas decimais.
    fDESCTO_ITEM: currency;     /// Valor  do desconto  incidente sobre o valor do item, com duas casas decimais.
    fACRES_ITEM: currency;      /// Valor do acréscimo incidente sobre o valor do item, com duas casas decimais.
    fVL_TOT_ITEM: currency;     /// Valor total líquido do item, com duas casas decimais.
    fCOD_TOT_PARC: string;      /// Código do totalizador relativo ao produto ou serviço conforme tabela abaixo.
    fIND_CANC: string;          /// Informar "S" ou "N", conforme tenha ocorrido ou não, o cancelamento total do item  no documento. Informar "P" quando ocorrer o cancelamento parcial do item.
    fQTDE_CANC: currency;       /// Quantidade cancelada, no caso de cancelamento parcial de item, sem a separação das casas decimais.
    fVL_CANC: currency;         /// Valor cancelado, no caso de cancelamento parcial de item
    fVL_CANC_ACRES: currency;   /// Valor do cancelamento de acréscimo no item
    fIAT: string;               /// Indicador de Arredondamento ou Truncamento relativo à regra de cálculo do valor total líquido do item, sendo “T”  para truncamento ou “A” para arredondamento
    fIPPT: string;              /// Indicador de Produção Própria ou de Terceiro relativo à mercadoria, sendo “P” para mercadoria de produção própria ou “T” para mercadoria produzida por terceiros
    fQTDE_DECIMAL: integer;     /// Parâmetro de número de casas decimais da quantidade
    fVL_DECIMAL: integer;       /// Parâmetro de número de casas decimais de valor unitário
  public
    constructor Create; virtual; /// Create

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property NUM_ITEM: integer read FNUM_ITEM write FNUM_ITEM;
    property COD_ITEM: string read FCOD_ITEM write FCOD_ITEM;
    property DESC_ITEM: string read FDESC_ITEM write FDESC_ITEM;
    property QTDE_ITEM: currency read FQTDE_ITEM write FQTDE_ITEM;
    property UN_MED: string read FUN_MED write FUN_MED;
    property VL_UNIT: currency read FVL_UNIT write FVL_UNIT;
    property DESCTO_ITEM: currency read FDESCTO_ITEM write FDESCTO_ITEM;
    property ACRES_ITEM: currency read FACRES_ITEM write FACRES_ITEM;
    property VL_TOT_ITEM: currency read FVL_TOT_ITEM write FVL_TOT_ITEM;
    property COD_TOT_PARC: string read FCOD_TOT_PARC write FCOD_TOT_PARC;
    property IND_CANC: string read FIND_CANC write FIND_CANC;
    property QTDE_CANC: currency read FQTDE_CANC write FQTDE_CANC;
    property VL_CANC: currency read FVL_CANC write FVL_CANC;
    property VL_CANC_ACRES: currency read FVL_CANC_ACRES write FVL_CANC_ACRES;
    property IAT: string read FIAT write FIAT;
    property IPPT: string read FIPPT write FIPPT;
    property QTDE_DECIMAL: integer read FQTDE_DECIMAL write FQTDE_DECIMAL;
    property VL_DECIMAL: integer read FVL_DECIMAL write FVL_DECIMAL;
    property NUM_CONT: integer read FNUM_CONT write FNUM_CONT;
  end;

  /// REGISTRO R05 - Lista

  TRegistroR05List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroR05;
    procedure SetItem(Index: Integer; const Value: TRegistroR05);
  public
    function New: TRegistroR05;
    property Items[Index: Integer]: TRegistroR05 read GetItem write SetItem;
  end;

  /// REGISTRO TIPO R06 - DEMAIS DOCUMENTOS EMITIDOS PELO ECF

  TRegistroR06 = class
  private
    fRegistroValido: boolean;
    fNUM_USU: integer;          /// Nº de ordem do usuário do ECF
    fCOO: integer;              /// Nº do COO relativo ao respectivo documento
    fGNF: integer;              /// Número do GNF relativo ao respectivo documento, quando houver
    fGRG: integer;              /// Número do GRG relativo ao respectivo documento (vide item 7.6.1.2)
    fCDC: integer;              /// Número do CDC relativo ao respectivo documento (vide item 7.6.1.3)
    fDENOM: string;             /// Símbolo  referente à denominação do documento fiscal, conforme tabela abaixo
    fDT_FIN: TDateTime;         /// Data final de emissão (impressa no rodapé do documento)
    fHR_FIN: TDateTime;         /// Hora final de emissão (impressa no rodapé do documento)

    fRegistroR07: TRegistroR07List;
    procedure setNUM_USU(const Value: integer);
    procedure setCOO(const Value: integer); /// Registro FILHO
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property NUM_USU: integer read FNUM_USU write setNUM_USU;
    property COO: integer read FCOO write setCOO;
    property GNF: integer read FGNF write FGNF;
    property GRG: integer read FGRG write FGRG;
    property CDC: integer read FCDC write FCDC;
    property DENOM: string read FDENOM write FDENOM;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
    property HR_FIN: TDateTime read FHR_FIN write FHR_FIN;

    property RegistroR07: TRegistroR07List read FRegistroR07 write FRegistroR07;
  end;

  /// REGISTRO R06 - Lista

  TRegistroR06List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroR06;
    procedure SetItem(Index: Integer; const Value: TRegistroR06);
  public
    function New: TRegistroR06;
    property Items[Index: Integer]: TRegistroR06 read GetItem write SetItem;
  end;

  /// REGISTRO TIPO R07 - DETALHE DO CUPOM FISCAL E DO DOCUMENTO NÃO FISCAL  - MEIO DE PAGAMENTO

  TRegistroR07 = class
  private
    fRegistroValido: boolean;
    fCCF: integer;              /// Número do Contador de Cupom Fiscal relativo ao respectivo Cupom Fiscal emitido
    fGNF: integer;              /// Número do GNF relativo ao respectivo documento, quando houver
    fMP: string;                /// Descrição do totalizador parcial de meio de pagamento
    fVL_PAGTO: currency;        /// Valor do pagamento efetuado, com duas casas decimais
    fIND_EST: string;           /// Informar "S" ou "N", conforme tenha ocorrido ou não, o estorno do pagamento, ou “P” para estorno parcial do pagamento
    fVL_EST: currency;          /// Valor do estorno efetuado, com duas casas decimais
    fTipoRegistroPai: string;
    fRegistroPai: Pointer;
    FCOO: integer;
  public
    constructor Create(const cTipoRegistroPai:string; cRegistroPai:Pointer); virtual; /// Create

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property COO: integer read FCOO write FCOO;
    property CCF: integer read FCCF write FCCF;
    property GNF: integer read FGNF write FGNF;
    property MP: string read FMP write FMP;
    property VL_PAGTO: currency read FVL_PAGTO write FVL_PAGTO;
    property IND_EST: string read FIND_EST write FIND_EST;
    property VL_EST: currency read FVL_EST write FVL_EST;
    property TipoRegistroPai: string read fTipoRegistroPai write fTipoRegistroPai;
    property RegistroPai: Pointer read fRegistroPai write fRegistroPai;
  end;

  /// REGISTRO R07 - Lista

  TRegistroR07List = class(TObjectList)
  private
    fTipoRegistroPai: string;
    fRegistroPai: Pointer;
    function GetItem(Index: Integer): TRegistroR07;
    procedure SetItem(Index: Integer; const Value: TRegistroR07);
  public
    function New: TRegistroR07;
    property Items[Index: Integer]: TRegistroR07 read GetItem write SetItem;
    property TipoRegistroPai: string read fTipoRegistroPai write fTipoRegistroPai;
    property RegistroPai: Pointer read fRegistroPai write fRegistroPai;
  end;

  TRegistroR07xList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroR07;
    procedure SetItem(Index: Integer; const Value: TRegistroR07);
  public
    function New: TRegistroR07;
    property Items[Index: Integer]: TRegistroR07 read GetItem write SetItem;
  end;

implementation

{ TRegistroR02List }

function TRegistroR02List.GetItem(Index: Integer): TRegistroR02;
begin
  Result := TRegistroR02(inherited Items[Index]);
end;

function TRegistroR02List.New: TRegistroR02;
begin
  Result := TRegistroR02.Create;
  Add(Result);
end;

procedure TRegistroR02List.SetItem(Index: Integer; const Value: TRegistroR02);
begin
  Put(Index, Value);
end;

{ TRegistroR03List }

function TRegistroR03List.GetItem(Index: Integer): TRegistroR03;
begin
  Result := TRegistroR03(inherited Items[Index]);
end;

function TRegistroR03List.New: TRegistroR03;
begin
  Result := TRegistroR03.Create;
  Add(Result);
end;

procedure TRegistroR03List.SetItem(Index: Integer; const Value: TRegistroR03);
begin
  Put(Index, Value);
end;

{ TRegistroR04List }

function TRegistroR04List.GetItem(Index: Integer): TRegistroR04;
begin
  Result := TRegistroR04(inherited Items[Index]);
end;

function TRegistroR04List.New: TRegistroR04;
begin
  Result := TRegistroR04.Create;
  Add(Result);
end;

procedure TRegistroR04List.SetItem(Index: Integer; const Value: TRegistroR04);
begin
  Put(Index, Value);
end;

{ TRegistroR05List }

function TRegistroR05List.GetItem(Index: Integer): TRegistroR05;
begin
  Result := TRegistroR05(inherited Items[Index]);
end;

function TRegistroR05List.New: TRegistroR05;
begin
  Result := TRegistroR05.Create;
  Add(Result);
end;

procedure TRegistroR05List.SetItem(Index: Integer; const Value: TRegistroR05);
begin
  Put(Index, Value);
end;

{ TRegistroR06List }

function TRegistroR06List.GetItem(Index: Integer): TRegistroR06;
begin
  Result := TRegistroR06(inherited Items[Index]);
end;

function TRegistroR06List.New: TRegistroR06;
begin
  Result := TRegistroR06.Create;
  Add(Result);
end;

procedure TRegistroR06List.SetItem(Index: Integer; const Value: TRegistroR06);
begin
  Put(Index, Value);
end;

{ TRegistroR07List }

function TRegistroR07List.GetItem(Index: Integer): TRegistroR07;
begin
  Result := TRegistroR07(inherited Items[Index]);
end;

function TRegistroR07List.New: TRegistroR07;
begin
  Result := TRegistroR07.Create(TipoRegistroPai, RegistroPai);
  Add(Result);
end;

procedure TRegistroR07List.SetItem(Index: Integer; const Value: TRegistroR07);
begin
  Put(Index, Value);
end;

{ TRegistroR01 }

constructor TRegistroR01.Create;
begin
   fRegistroValido := True;
   fInclusaoExclusao := False;
   FRegistroR02 := TRegistroR02List.Create;
   FRegistroR04 := TRegistroR04List.Create;
   FRegistroR06 := TRegistroR06List.Create;
end;

destructor TRegistroR01.Destroy;
begin
  FRegistroR02.Free;
  FRegistroR04.Free;
  FRegistroR06.Free;
  inherited;
end;

procedure TRegistroR01.SetRegistroR02(const Value: TRegistroR02List);
begin
  FRegistroR02 := Value;
end;

procedure TRegistroR01.SetRegistroR04(const Value: TRegistroR04List);
begin
  FRegistroR04 := Value;
end;

procedure TRegistroR01.SetRegistroR06(const Value: TRegistroR06List);
begin
  FRegistroR06 := Value;
end;

{ TRegistroR02 }

constructor TRegistroR02.Create;
begin
  fRegistroValido := True;
  FRegistroR03 := TRegistroR03List.Create;
end;

destructor TRegistroR02.Destroy;
begin
  FRegistroR03.Free;
  inherited;
end;

{ TRegistroR03 }

constructor TRegistroR03.Create;
begin
  fRegistroValido := True;
end;

{ TRegistroR04 }

constructor TRegistroR04.Create;
begin
  fRegistroValido := True;
  FRegistroR05 := TRegistroR05List.Create;
  FRegistroR07 := TRegistroR07List.Create;
  FRegistroR07.fTipoRegistroPai := 'R04';
  FRegistroR07.fRegistroPai := Self;
end;

destructor TRegistroR04.Destroy;
begin
  FRegistroR05.Free;
  FRegistroR07.Free;
  inherited;
end;

{ TRegistroR05 }

constructor TRegistroR05.Create;
begin
  fRegistroValido := True;
  FNUM_CONT       := -1;
end;

{ TRegistroR06 }

constructor TRegistroR06.Create;
begin
  fRegistroValido := True;
  FRegistroR07 := TRegistroR07List.Create;
  FRegistroR07.fTipoRegistroPai := 'R06';
  FRegistroR07.fRegistroPai := Self;
end;

destructor TRegistroR06.Destroy;
begin
  FRegistroR07.Free;
  inherited;
end;

procedure TRegistroR06.setCOO(const Value: integer);
begin
  FCOO := Value;
end;

procedure TRegistroR06.setNUM_USU(const Value: integer);
begin
  FNUM_USU := Value;
end;

{ TRegistroR07 }

constructor TRegistroR07.Create(const cTipoRegistroPai:string; cRegistroPai:Pointer);
begin
  fRegistroValido := True;
  fTipoRegistroPai := cTipoRegistroPai;
  fRegistroPai := cRegistroPai;
end;

{ TRegistroR07xList }


function TRegistroR07xList.GetItem(Index: Integer): TRegistroR07;
begin
  Result := TRegistroR07(inherited Items[Index]);
end;

function TRegistroR07xList.New: TRegistroR07;
begin
  Result := TRegistroR07.Create('x',self);
  Add(Result);
end;

procedure TRegistroR07xList.SetItem(Index: Integer; const Value: TRegistroR07);
begin
  Put(Index, Value);
end;

{ TRegistroR01List }

function TRegistroR01List.GetItem(Index: Integer): TRegistroR01;
begin
  Result := TRegistroR01(inherited Items[Index]);
end;

function TRegistroR01List.New: TRegistroR01;
begin
  Result := TRegistroR01.Create;
  Add(Result);
end;

procedure TRegistroR01List.SetItem(Index: Integer; const Value: TRegistroR01);
begin
  Put(Index, Value);
end;

end.
