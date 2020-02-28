{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou, Isaque Pinheiro e              }
{							   Nilson Sergio								   }	  
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

unit ACBrLFDBloco_A;

interface

uses
  SysUtils, Classes, DateUtils, ACBrLFDBlocos;

type

  TRegistroA020List = class;
  TRegistroA025List = class;
  TRegistroA030 = class;
  TRegistroA035 = class;
  TRegistroA040 = class;
  TRegistroA045 = class;
  TRegistroA050List = class;
  TRegistroA055List = class;
  TRegistroA200List = class;
  TRegistroA300List = class;
  TRegistroA310List = class;
  TRegistroA320List = class;
  TRegistroA330List = class;
  TRegistroA350List = class;
  TRegistroA355 = class;
  TRegistroA360List = class;
  TRegistroA365 = class;
  TRegistroA370List = class;
  TRegistroA380List = class;

  /// Registro A001 - ABERTURA DO BLOCO A

  { TRegistroA001 }

  TRegistroA001 = class(TOpenBlocos)
  private
    FRegistroA020: TRegistroA020List;
    FRegistroA300: TRegistroA300List;
    FRegistroA320: TRegistroA320List;
    FRegistroA350: TRegistroA350List;
    FRegistroA370: TRegistroA370List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroA020: TRegistroA020List read FRegistroA020 write FRegistroA020;
    property RegistroA300: TRegistroA300List read FRegistroA300 write FRegistroA300;
    property RegistroA320: TRegistroA320List read FRegistroA320 write FRegistroA320;
    property RegistroA350: TRegistroA350List read FRegistroA350 write FRegistroA350;
    property RegistroA370: TRegistroA370List read FRegistroA370 write FRegistroA370;
  end;

  /// Registro A020 - DOCUMENTO - NOTA FISCAL DE SERVIÇOS

  { TRegistroA020 }

  TRegistroA020 = class
  private
    FIND_OPER: TACBrlTipoOperacao; /// Indicador do tipo de operação
    FIND_EMIT: TACBrlEmitente; /// Indicador do emitente do documento fiscal
    FCOD_PART: String; /// Código do participante (campo 02 do Registro 0150)
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FCOD_SIT: TACBrlSituacaoDocto; /// Código da situação do documento fiscal
    FSER: String; /// Série do documento fiscal
    FSUB: String; /// Subsérie do documento fiscal
    FNUM_DOC: String; /// Número do documento fiscal
    FDT_DOC: TDateTime; /// Data da emissão do documento fiscal
    FCOP: String; // Código da classe da prestação
    FCOD_MUN_SERV: Integer; /// Código do município onde o serviço foi prestado
    FCOD_NAT: String;
    FVL_DOC: Currency; /// Valor total do documento fiscal
    FIND_PGTO: TACBrlTipoPagamento; /// Indicador do tipo de pagamento
    FVL_SUB: Currency; /// Valor subcontratado
    FVL_DESC: Currency; /// Valor total do desconto
    FVL_SERV: Currency; /// Valor total dos serviços prestados
    FVL_MAT_PROP: Currency; /// Valor do material próprio utilizado na prestação do serviço
    FVL_MAT_TERC: Currency; /// Valor do material de terceiros utilizado na prestação do serviço     
    FVL_DA: Currency; /// Valor de outras despesas acessórias
//    FVL_OP_ICMS: Currency; /// Valor das operações tributadas pelo ICMS
    FVL_BC_ISS: Currency; /// Valor da base de cálculo do ISS
    FVL_ISS: Currency; /// Valor do ISS
    FVL_BC_RT_ISS: Currency; /// Valor da base de cálculo de retenção do ISS
    FVL_RT_ISS: Currency; /// Valor do ISS retido pelo tomador
    FCOD_INF_OBS: String; /// Código de referência à informação complementar

    FRegistroA025: TRegistroA025List;
    FRegistroA030: TRegistroA030;
    FRegistroA035: TRegistroA035;
    FRegistroA040: TRegistroA040;
    FRegistroA050: TRegistroA050List;
    FRegistroA200: TRegistroA200List;
  public
    constructor Create(AOwner: TRegistroA001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_OPER: TACBrlTipoOperacao read FIND_OPER write FIND_OPER;
    property IND_EMIT: TACBrlEmitente read FIND_EMIT write FIND_EMIT;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property COD_SIT: TACBrlSituacaoDocto read FCOD_SIT write FCOD_SIT;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CFPS: String read FCOP write FCOP;
    property COD_MUN_SERV: Integer read FCOD_MUN_SERV write FCOD_MUN_SERV;
    property COD_NAT: String read FCOD_NAT write FCOD_NAT;
    property VL_DOC: Currency read FVL_DOC write FVL_DOC;
    property IND_PGTO: TACBrlTipoPagamento read FIND_PGTO write FIND_PGTO;
    property VL_SUB: Currency read FVL_SUB write FVL_SUB;
    property VL_DESC: Currency read FVL_DESC write FVL_DESC;
    property VL_SERV: Currency read FVL_SERV write FVL_SERV;
    property VL_MAT_PROP: Currency read FVL_MAT_PROP write FVL_MAT_PROP;
    property VL_MAT_TERC: Currency read FVL_MAT_TERC write FVL_MAT_TERC;
    property VL_DA: Currency read FVL_DA write FVL_DA;
//    property VL_OP_ICMS: Currency read FVL_OP_ICMS write FVL_OP_ICMS;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property VL_BC_RT_ISS: Currency read FVL_BC_RT_ISS write FVL_BC_RT_ISS;
    property VL_RT_ISS: Currency read FVL_RT_ISS write FVL_RT_ISS;
    property COD_INF_OBS: String read FCOD_INF_OBS write FCOD_INF_OBS;

    property RegistroA025: TRegistroA025List read FRegistroA025 write FRegistroA025;
    property RegistroA030: TRegistroA030 read FRegistroA030 write FRegistroA030;
    property RegistroA040: TRegistroA040 read FRegistroA040 write FRegistroA040;
    property RegistroA035: TRegistroA035 read FRegistroA035 write FRegistroA035;
    property RegistroA050: TRegistroA050List read FRegistroA050 write FRegistroA050;
    property RegistroA200: TRegistroA200List read FRegistroA200 write FRegistroA200;
  end;

  /// Registro A020 - Lista

  { TRegistroA020List }

  TRegistroA020List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA020;
    procedure SetItem(Index: Integer; const Value: TRegistroA020);
  public
    function New(AOwner: TRegistroA001): TRegistroA020;
    property Items[Index: Integer]: TRegistroA020 read GetItem write SetItem;
  end;

    /// Registro A025 - Complemento do Documento - Tributos Federais

  { TRegistroA025 }

  TRegistroA025 = class
  private
    FVL_BC_IRRF: Currency; /// Valor da base de cálculo do Imposto de Renda Retido na Fonte
    FALIQ_IRRF: Currency; /// Alíquota do Imposto de Renda Retido na Fonte
    FVL_IRRF: Currency; /// Valor do Imposto de Renda Retido na Fonte
    FALIQ_PIS: Currency; /// Alíquota do PIS
    FVL_PIS: Currency; /// Valor do PIS
    FALIQ_COFINS: Currency; /// Alíquota do COFINS
    FVL_COFINS: Currency; /// Valor da COFINS
    FVL_BC_PREV: Currency; /// Valor da base de cálculo de retenção da Previdência Social
    FVL_PREV: Currency; /// Valor destacado para retenção da Previdência Social
  public
    constructor Create(AOwner: TRegistroA020); virtual; /// Create

    property VL_BC_IRRF: Currency read FVL_BC_IRRF write FVL_BC_IRRF;
    property ALIQ_IRRF: Currency read FALIQ_IRRF write FALIQ_IRRF;
    property VL_IRRF: Currency read FVL_IRRF write FVL_IRRF;
    property ALIQ_PIS: Currency read FALIQ_PIS write FALIQ_PIS;
    property VL_PIS: Currency read FVL_PIS write FVL_PIS;
    property ALIQ_COFINS: Currency read FALIQ_COFINS write FALIQ_COFINS;
    property VL_COFINS: Currency read FVL_COFINS write FVL_COFINS;
    property VL_BC_PREV: Currency read FVL_BC_PREV write FVL_BC_PREV;
    property VL_PREV: Currency read FVL_PREV write FVL_PREV;
  end;

  /// Registro A025 - Lista

  { TRegistroA025List }

  TRegistroA025List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA025;
    procedure SetItem(Index: Integer; const Value: TRegistroA025);
  public
    function New(AOwner: TRegistroA020): TRegistroA025;
    property Items[Index: Integer]: TRegistroA025 read GetItem write SetItem;
  end;


  /// Registro A030 - Complemento do Documento - Subcontratação

  { TRegistroA030 }

  TRegistroA030 = class
  private
    FCNPJ: String; /// Número de inscrição do subcontratado no CNPJ
    FCPF: String; /// Número de inscrição do subcontratado no CPF
    FCOD_MUN: Integer; /// Código do município do domicílio fiscal do subcontratado conforme a tabela indicada
    FIM: String; /// Inscrição Municipal do subcontratado
    FVL_SUB: String; /// Valor subcontratado
    FVL_BC_ISS_SUB: Currency; /// Valor da base de cálculo do ISS da subcontratação
    FVL_ISS_SUB: Currency; /// Valor do ISS da subcontratação
  public
    constructor Create(AOwner: TRegistroA020); virtual; /// Create

    property CNPJ: String read FCNPJ write FCNPJ;
    property CPF: String read FCPF write FCPF;
    property COD_MUN: Integer read FCOD_MUN write FCOD_MUN;
    property IM: String read FIM write FIM;
    property VL_SUB: String read FVL_SUB write FVL_SUB;
    property VL_BC_ISS_SUB: Currency read FVL_BC_ISS_SUB write FVL_BC_ISS_SUB;
    property VL_ISS_SUB: Currency read FVL_ISS_SUB write FVL_ISS_SUB;
  end;

  /// Registro A035 - COMPLEMENTO DO DOCUMENTO - NOTA CONJUGADA/ICMS

  { TRegistroA035 }

  TRegistroA035 = class
  private
    FVL_OP_ICMS: Currency; /// Valor das operações tributadas pelo ICMS
    FVL_MERC: Currency; /// Valor das mercadorias constantes no documento fiscal
    FVL_FRT: Currency; /// Valor do frete
    FVL_SEG: Currency; /// Valor do seguro
    FVL_OUT_DA: Currency; /// Valor de outras despesas acessórias
    FVL_BC_ICMS: Currency; /// Valor da base de cálculo do ICMS
    FVL_ICMS: Currency; /// Valor do ICMS
    FVL_BC_ST: Currency; /// Valor da base de cálculo do ICMS substituição tributária
    FVL_ICMS_ST: Currency; /// Valor do ICMS da substituição tributária
    FVL_ICMS_ST_NI: Currency; /// Valor do ICMS da substituição tributária do frete do transportador não-inscrito
    FVL_IPI: Currency; /// Valor do IPI
  public
    constructor Create(AOwner: TRegistroA020); virtual; /// Create

    property VL_OP_ICMS: Currency read FVL_OP_ICMS write FVL_OP_ICMS;
    property VL_MERC: Currency read FVL_MERC write FVL_MERC;
    property VL_FRT: Currency read FVL_FRT write FVL_FRT;
    property VL_SEG: Currency read FVL_SEG write FVL_SEG;
    property VL_OUT_DA: Currency read FVL_OUT_DA write FVL_OUT_DA;
    property VL_BC_ICMS: Currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: Currency read FVL_ICMS write FVL_ICMS;
    property VL_BC_ST: Currency read FVL_BC_ST write FVL_BC_ST;
    property VL_ICMS_ST: Currency read FVL_ICMS_ST write FVL_ICMS_ST;
    property VL_ICMS_ST_NI: Currency read FVL_ICMS_ST_NI write FVL_ICMS_ST_NI;
    property VL_IPI: Currency read FVL_IPI write FVL_IPI;
  end;

  /// Registro A040 - COMPLEMENTO DO DOCUMENTO - TRANSPORTADOR

  { TRegistroA040 }

  TRegistroA040 = class
  private
    FIND_FRT: TACBrTipoFrete; /// Indicador de frete
    FCOD_PART: String; /// Código do participante - transportador
    FRTC: String; /// Número de registro do transportador de carga na agência específica
    FID_VEIC: String; /// Placa de identificação do veículo
    FUF: String; /// Sigla da unidade da federação que consta na placa do veículo
    FVOL: Double; /// Volume transportado
    FQTD_VOL: Double; /// Quantidade de volumes transportados
    FESPECIE: String; /// Espécie dos volumes transportados
    FMARCA: String; /// Marca dos volumes transportados
    FNUM: String; /// Numeração dos volumes transportados
    FPESO_BRT: Double; /// Peso bruto dos volumes transportados (em Kg)
    FPESO_LIQ: Double; /// Peso líquido dos volumes transportados (em Kg)

    FRegistroA045: TRegistroA045;
  public
    constructor Create(AOwner: TRegistroA020); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_FRT: TACBrTipoFrete read FIND_FRT write FIND_FRT;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property RTC: String read FRTC write FRTC;
    property ID_VEIC: String read FID_VEIC write FID_VEIC;
    property UF: String read FUF write FUF;
    property VOL: Double read FVOL write FVOL;
    property QTD_VOL: Double read FQTD_VOL write FQTD_VOL;
    property ESPECIE: String read FESPECIE write FESPECIE;
    property MARCA: String read FMARCA write FMARCA;
    property NUM: String read FNUM write FNUM;
    property PESO_BRT: Double read FPESO_BRT write FPESO_BRT;
    property PESO_LIQ: Double read FPESO_LIQ write FPESO_LIQ;

    property RegistroA045: TRegistroA045 read FRegistroA045 write FRegistroA045;
  end;

  /// Registro A045 - COLETA E ENTREGA

  { TRegistroA045 }

  TRegistroA045 = class
  private
    FIND_MOD: TACBrTipoTransporte; /// Indicador do tipo de transporte da carga
    FCNPJ_COL: String; /// Número de inscrição do contribuinte no CNPJ do local de coleta
    FUF_COL: String; /// Sigla da unidade da federação do contribuinte do local de coleta
    FIE_COL: String; /// Inscrição Estadual do contribuinte do local de coleta
    FCOD_MUN_COL: Integer; /// Código do município do local de coleta
    FIM_COL: String; /// Inscrição Municipal do contribuinte do local de coleta
    FEND_COL: String; /// Endereço completo do local de coleta
    FCNPJ_ENTG: String; /// Número de inscrição do contribuinte no CNPJ do local de entrega
    FUF_ENTG: String; /// Sigla da unidade da federação do contribuinte do local de entrega
    FIE_ENTG: String; /// Inscrição Estadual do contribuinte do local de entrega
    FCOD_MUN_ENTG: Integer; /// Código do município do local de entrega
    FIM_ENTG: String; /// Inscrição Municipal do contribuinte do local de entrega
    FEND_ENTG: String; /// Endereço completo do local de entrega
  public
    constructor Create(AOwner: TRegistroA040); virtual; /// Create

    property IND_MOD: TACBrTipoTransporte read FIND_MOD write FIND_MOD;
    property CNPJ_COL: String read FCNPJ_COL write FCNPJ_COL;
    property UF_COL: String read FUF_COL write FUF_COL;
    property IE_COL: String read FIE_COL write FIE_COL;
    property COD_MUN_COL: Integer read FCOD_MUN_COL write FCOD_MUN_COL;
    property IM_COL: String read FIM_COL write FIM_COL;
    property END_COL: String read FEND_COL write FEND_COL;
    property CNPJ_ENTG: String read FCNPJ_ENTG write FCNPJ_ENTG;
    property UF_ENTG: String read FUF_ENTG write FUF_ENTG;
    property IE_ENTG: String read FIE_ENTG write FIE_ENTG;
    property COD_MUN_ENTG: Integer read FCOD_MUN_ENTG write FCOD_MUN_ENTG;
    property IM_ENTG: String read FIM_ENTG write FIM_ENTG;
    property END_ENTG: String read FEND_ENTG write FEND_ENTG;
  end;

  /// Registro A050 - COMPLEMENTO DO DOCUMENTO - TÍTULO DE CRÉDITO

  { TRegistroA050 }

  TRegistroA050 = class
  private
    FIND_TIT: TACBrTipoTitulo; /// Indicador do tipo de título de crédito
    FDESCR_TIT: String; /// Descrição complementar do título de crédito
    FNUM_TIT: String; /// Número ou código identificador do título de crédito
    FDT_TIT: TDateTime; /// Data de emissão do título de crédito
    FVL_TIT: Currency; /// Valor original do título de crédito
    FQTD_PARC: Integer; /// Quantidade de parcelas a pagar

    FRegistroA055: TRegistroA055List;
  public
    constructor Create(AOwner: TRegistroA020); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_TIT: TACBrTipoTitulo read FIND_TIT write FIND_TIT;
    property DESCR_TIT: String read FDESCR_TIT write FDESCR_TIT;
    property NUM_TIT: String read FNUM_TIT write FNUM_TIT;
    property DT_TIT: TDateTime read FDT_TIT write FDT_TIT;
    property VL_TIT: Currency read FVL_TIT write FVL_TIT;
    property QTD_PARC: Integer read FQTD_PARC write FQTD_PARC;

    property RegistroA055: TRegistroA055List read FRegistroA055 write FRegistroA055;
  end;

  /// Registro A050 - Lista

  { TRegistroA050List }

  TRegistroA050List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA050;
    procedure SetItem(Index: Integer; const Value: TRegistroA050);
  public
    function New(AOwner: TRegistroA020): TRegistroA050;
    property Items[Index: Integer]: TRegistroA050 read GetItem write SetItem;
  end;

  /// Registro A055 - PARCELA DO TÍTULO

  { TRegistroA055 }

  TRegistroA055 = class
  private
    FNUM_PARC: Integer; /// Número da parcela a pagar
    FDT_VCTO: TDateTime; /// Data de vencimento da parcela
    FVL_PARC: Currency; /// Valor da parcela a pagar
  public
    constructor Create(AOwner: TRegistroA050); virtual; /// Create

    property NUM_PARC: Integer read FNUM_PARC write FNUM_PARC;
    property DT_VCTO: TDateTime read FDT_VCTO write FDT_VCTO;
    property VL_PARC: Currency read FVL_PARC write FVL_PARC;
  end;

  /// Registro A055 - Lista

  { TRegistroA055List }

  TRegistroA055List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA055;
    procedure SetItem(Index: Integer; const Value: TRegistroA055);
  public
    function New(AOwner: TRegistroA050): TRegistroA055;
    property Items[Index: Integer]: TRegistroA055 read GetItem write SetItem;
  end;

  /// Registro A200 - ITENS DO DOCUMENTO

  { TRegistroA200 }

  TRegistroA200 = class
  private
    FNUM_ITEM: Integer; /// Número seqüencial do item no documento fiscal
    FCOD_ITEM: String; /// Código do item (campo 02 do Registro 0200)
    FUNID: String; /// Unidade do item
    FVL_UNIT: Currency; /// Valor unitário
    FQTD: Double; /// Quantidade do item
    FVL_DESC_I: Currency; /// Valor total do desconto
    FVL_ACMO_I: Currency; /// Valor do acréscimo
    FVL_ITEM: Currency; /// Valor do item
    FCTISS: String; /// Código de Tributação do ISS
    FVL_BC_ISS_I: Currency; /// Valor da base de cálculo do ISS
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS_I: Currency; /// Valor do ISS
  public
    constructor Create(AOwner: TRegistroA020); virtual; /// Create

    property NUM_ITEM: Integer read FNUM_ITEM write FNUM_ITEM;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property VL_UNIT: Currency read FVL_UNIT write FVL_UNIT;
    property QTD: Double read FQTD write FQTD;
    property VL_DESC_I: Currency read FVL_DESC_I write FVL_DESC_I;
    property VL_ACMO_I: Currency read FVL_ACMO_I write FVL_ACMO_I;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property CTISS: String read FCTISS write FCTISS;
    property VL_BC_ISS_I: Currency read FVL_BC_ISS_I write FVL_BC_ISS_I;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS_I: Currency read FVL_ISS_I write FVL_ISS_I;
  end;

  /// Registro A200 - Lista

  { TRegistroA200List }

  TRegistroA200List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA200;
    procedure SetItem(Index: Integer; const Value: TRegistroA200);
  public
    function New(AOwner: TRegistroA020): TRegistroA200;
    property Items[Index: Integer]: TRegistroA200 read GetItem write SetItem;
  end;

  /// Registro A300 - DOCUMENTO - NOTA FISCAL DE SERVIÇOS SIMPLIFICADA

  { TRegistroA300 }

  TRegistroA300 = class
  private
    FCPF_CONS: String; /// Número de inscrição do tomador do serviço no CPF
    FCNPJ_CONS: String; /// Número de inscrição do tomador do serviço no CNPJ
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FCOD_SIT: TACBrlSituacaoDocto; /// Código da situação do documento fiscal
    FSER: String; /// Série do documento fiscal
    FSUB: String; /// Subsérie do documento fiscal
    FNUM_DOC: String; /// Número do documento fiscal
    FDT_DOC: TDateTime; /// Data da emissão do documento fiscal
    FCOP: String; /// Código Fiscal de Prestações de Serviços
    FVL_DOC: Currency; /// Valor total do documento fiscal
    FVL_DESC: Currency; /// Valor total do desconto
    FVL_ACMO: Currency; /// Valor dos acréscimos
    FVL_SERV: Currency; /// Valor total dos serviços prestados
    FVL_MAT_PROP: Currency; /// Valor do material próprio utilizado nos serviços
    FVL_DA: Currency; /// Valor das despesas acessórias
    FVL_BC_ISS: Currency; /// Valor da base de cálculo do ISS
    FVL_ISS: Currency; /// Valor do ISS
    FCOD_INF_OBS: String; /// Código de referência à informação complementar

    FRegistroA310: TRegistroA310List;
  public
    constructor Create(AOwner: TRegistroA001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property CPF_CONS: String read FCPF_CONS write FCPF_CONS;
    property CNPJ_CONS: String read FCNPJ_CONS write FCNPJ_CONS;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property COD_SIT: TACBrlSituacaoDocto read FCOD_SIT write FCOD_SIT;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property COP: String read FCOP write FCOP;
    property VL_DOC: Currency read FVL_DOC write FVL_DOC;
    property VL_DESC: Currency read FVL_DESC write FVL_DESC;
    property VL_ACMO: Currency read FVL_ACMO write FVL_ACMO;
    property VL_SERV: Currency read FVL_SERV write FVL_SERV;
    property VL_MAT_PROP: Currency read FVL_MAT_PROP write FVL_MAT_PROP;
    property VL_DA: Currency read FVL_DA write FVL_DA;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property COD_INF_OBS: String read FCOD_INF_OBS write FCOD_INF_OBS;

    property RegistroA310: TRegistroA310List read FRegistroA310 write FRegistroA310;
  end;

  /// Registro A300 - Lista

  { TRegistroA300List }

  TRegistroA300List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA300;
    procedure SetItem(Index: Integer; const Value: TRegistroA300);
  public
    function New(AOwner: TRegistroA001): TRegistroA300;
    property Items[Index: Integer]: TRegistroA300 read GetItem write SetItem;
  end;

  /// Registro A310 - ITENS DO DOCUMENTO

  { TRegistroA310 }

  TRegistroA310 = class
  private
    FNUM_ITEM: Integer; /// Número seqüencial do item no documento fiscal
    FCOD_ITEM: String; /// Código do item (campo 02 do Registro 0200)
    FUNID: String; /// Unidade do item
    FVL_UNIT: Currency; /// Valor unitário
    FQTD: Double; /// Quantidade do item
    FVL_DESC_I: Currency; /// Valor do desconto
    FVL_ACMO_I: Currency; /// Valor do acréscimo
    FVL_ITEM: Currency; /// Valor líquido do item
    FCTISS: String; /// Código de Tributação do ISS
    FVL_BC_ISS_I: Currency; /// Valor da base de cálculo do ISS
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS_I: Currency; /// Valor do ISS
  public
    constructor Create(AOwner: TRegistroA300); virtual; /// Create

    property NUM_ITEM: Integer read FNUM_ITEM write FNUM_ITEM;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property VL_UNIT: Currency read FVL_UNIT write FVL_UNIT;
    property QTD: Double read FQTD write FQTD;
    property VL_DESC_I: Currency read FVL_DESC_I write FVL_DESC_I;
    property VL_ACMO_I: Currency read FVL_ACMO_I write FVL_ACMO_I;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property CTISS: String read FCTISS write FCTISS;
    property VL_BC_ISS_I: Currency read FVL_BC_ISS_I write FVL_BC_ISS_I;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS_I: Currency read FVL_ISS_I write FVL_ISS_I;
  end;

  /// Registro A310 - Lista

  { TRegistroA310List }

  TRegistroA310List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA310;
    procedure SetItem(Index: Integer; const Value: TRegistroA310);
  public
    function New(AOwner: TRegistroA300): TRegistroA310;
    property Items[Index: Integer]: TRegistroA310 read GetItem write SetItem;
  end;

  /// Registro A320 - DOCUMENTOS - RESUMO DA NOTA FISCAL DE SERVIÇOS SIMPLIFICADA

  { TRegistroA320 }

  TRegistroA320 = class
  private
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FQTD_CANC: Integer; /// Quantidade de documentos cancelados
    FSER: String; /// Série dos documentos fiscais
    FSUB: String; /// Subsérie dos documentos fiscais
    FNUM_DOC_INI: Integer; /// Número do primeiro documento fiscal emitido no dia
    FNUM_DOC_FIN: Integer; /// Número do último documento fiscal emitido no dia
    FDT_DOC: TDateTime; /// Data da emissão dos documentos fiscais
    FCOP: String; /// Código Fiscal de Prestações de Serviços
    FVL_DOC: Integer; /// Valor acumulado dos documentos
    FVL_SERV: Currency; /// Valor acumulado dos serviços prestados
    FVL_MAT_PROP: Currency; /// Valor acumulado dos materiais próprios utilizados nos serviços
    FVL_DA: Currency; /// Valor acumulado das despesas acessórias indicados nos documentos
    FVL_BC_ISS: Currency; /// Valor acumulado da base de cálculo do ISS
    FVL_ISS: Currency; /// Valor acumulado do ISS

    FRegistroA330: TRegistroA330List;
  public
    constructor Create(AOwner: TRegistroA001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property QTD_CANC: Integer read FQTD_CANC write FQTD_CANC;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property NUM_DOC_INI: Integer read FNUM_DOC_INI write FNUM_DOC_INI;
    property NUM_DOC_FIN: Integer read FNUM_DOC_FIN write FNUM_DOC_FIN;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property COP: String read FCOP write FCOP;
    property VL_DOC: Integer read FVL_DOC write FVL_DOC;
    property VL_SERV: Currency read FVL_SERV write FVL_SERV;
    property VL_MAT_PROP: Currency read FVL_MAT_PROP write FVL_MAT_PROP;
    property VL_DA: Currency read FVL_DA write FVL_DA;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;

    property RegistroA330: TRegistroA330List read FRegistroA330 write FRegistroA330;
  end;

  /// Registro A320 - Lista

  { TRegistroA320List }

  TRegistroA320List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA320;
    procedure SetItem(Index: Integer; const Value: TRegistroA320);
  public
    function New(AOwner: TRegistroA001): TRegistroA320;
    property Items[Index: Integer]: TRegistroA320 read GetItem write SetItem;
  end;

  /// Registro A330 - ITENS DOS DOCUMENTOS

  { TRegistroA330 }

  TRegistroA330 = class
  private
    FCOD_ITEM: String; /// Código do item
    FUNID: String; /// Unidade do item
    FQTD: Double; /// Quantidade acumulada do item
    FVL_ITEM: Currency; /// Valor acumulado do item
    FVL_BC_ISS_I: Currency; /// Valor acumulado da base de cálculo do ISS
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS_I: Currency; /// Valor acumulado do ISS
  public
    constructor Create(AOwner: TRegistroA320); virtual; /// Create

    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property QTD: Double read FQTD write FQTD;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property VL_BC_ISS_I: Currency read FVL_BC_ISS_I write FVL_BC_ISS_I;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS_I: Currency read FVL_ISS_I write FVL_ISS_I;
  end;

  /// Registro A330 - Lista

  { TRegistroA330List }

  TRegistroA330List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA330;
    procedure SetItem(Index: Integer; const Value: TRegistroA330);
  public
    function New(AOwner: TRegistroA320): TRegistroA330;
    property Items[Index: Integer]: TRegistroA330 read GetItem write SetItem;
  end;

  /// Registro A350 - DOCUMENTO - CUPOM FISCAL/ISS

  { TRegistroA350 }

  TRegistroA350 = class
  private
    FCPF_CONS: String; /// Número de inscrição do tomador do serviço no CPF
    FCNPJ_CONS: String; /// Número de inscrição do tomador do serviço no CNPJ
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FCOD_SIT: TACBrlSituacaoDocto; /// Código da situação do documento fiscal
    FECF_CX: Integer; /// Número do caixa atribuído ao ECF
    FECF_FAB: String; /// Número de série de fabricação do ECF
    FCRO: Integer; /// Posição do Contador de Reinício de Operação
    FCRZ: Integer; /// Posição do Contador de Redução Z
    FNUM_DOC: String; /// Número do documento fiscal
    FDT_DOC: TDateTime; /// Data da emissão do documento fiscal
    FCOP: String; /// Código Fiscal de Prestações de Serviços
    FVL_ACMO_ICMS: Currency;
    FVL_CANC_ICMS: Currency;
    FVL_DESC_ICMS: Currency;
    FVL_DOC: Currency; /// Valor do documento fiscal
    FVL_CANC_ISS: Currency; /// Valor dos cancelamentos referentes ao ISS
    FVL_DESC_ISS: Currency; /// Valor dos descontos referentes ao ISS
    FVL_ACMO_ISS: Currency; /// Valor dos acréscimos referentes ao ISS
    FVL_BC_ISS: Currency; /// Valor da base de cálculo do ISS
    FVL_ISS: Currency; /// Valor do ISS
    FVL_ISN_ISS: Currency; /// Valor das prestações isentas do ISS
    FVL_NT_ISS: Currency; /// Valor das prestações sob não-incidência ou não-tributadas pelo ISS

    FRegistroA355: TRegistroA355;
    FRegistroA360: TRegistroA360List;
    FVL_RET_ISS: Currency;
  public
    constructor Create(AOwner: TRegistroA001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property CPF_CONS: String read FCPF_CONS write FCPF_CONS;
    property CNPJ_CONS: String read FCNPJ_CONS write FCNPJ_CONS;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property COD_SIT: TACBrlSituacaoDocto read FCOD_SIT write FCOD_SIT;
    property ECF_CX: Integer read FECF_CX write FECF_CX;
    property ECF_FAB: String read FECF_FAB write FECF_FAB;
    property CRO: Integer read FCRO write FCRO;
    property CRZ: Integer read FCRZ write FCRZ;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property COP: String read FCOP write FCOP;
    property VL_DOC: Currency read FVL_DOC write FVL_DOC;
    property VL_CANC_ISS: Currency read FVL_CANC_ISS write FVL_CANC_ISS;
    property VL_CANC_ICMS: Currency read FVL_CANC_ICMS write FVL_CANC_ICMS;
    property VL_DESC_ISS: Currency  read FVL_DESC_ISS  write FVL_DESC_ISS;
    property VL_DESC_ICMS: Currency read FVL_DESC_ICMS write FVL_DESC_ICMS;
    property VL_ACMO_ISS: Currency read FVL_ACMO_ISS write FVL_ACMO_ISS;
    property VL_ACMO_ICMS: Currency read FVL_ACMO_ICMS write FVL_ACMO_ICMS;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property VL_ISN_ISS: Currency read FVL_ISN_ISS write FVL_ISN_ISS;
    property VL_NT_ISS: Currency read FVL_NT_ISS write FVL_NT_ISS;
    property VL_RET_ISS: Currency read FVL_RET_ISS write FVL_RET_ISS;

    property RegistroA355: TRegistroA355 read FRegistroA355 write FRegistroA355;
    property RegistroA360: TRegistroA360List read FRegistroA360 write FRegistroA360;
  end;

  /// Registro A350 - Lista

  { TRegistroA350List }

  TRegistroA350List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA350;
    procedure SetItem(Index: Integer; const Value: TRegistroA350);
  public
    function New(AOwner: TRegistroA001): TRegistroA350;
    property Items[Index: Integer]: TRegistroA350 read GetItem write SetItem;
  end;

  /// Registro A355 - COMPLEMENTO DO DOCUMENTO - CUPOM CONJUGADO/ICMS

  { TRegistro A355 }

  TRegistroA355 = class
  private
    FVL_CANC_ICMS: Currency; /// Valor dos cancelamentos referentes ao ICMS
    FVL_DESC_ICMS: Currency; /// Valor dos descontos referentes ao ICMS
    FVL_ACMO_ICMS: Currency; /// Valor dos acréscimos referentes ao ICMS
    FVL_OP_ICMS: Currency; /// Valor das operações tributadas pelo ICMS
    FVL_BC_ICMS: Currency; /// Valor da base de cálculo do ICMS
    FVL_ICMS: Currency; /// Valor do ICMSF
    FVL_ISN: Currency; /// Valor das operações isentas do ICMS
    FVL_NT: Currency; /// Valor das operações não-tributadas pelo ICMS
    FVL_ICMS_ST: Currency; /// Valor das operações com substituição tributária do ICMS
  public
    constructor Create(AOwner: TRegistroA350); virtual; /// Create

    property VL_CANC_ICMS: Currency read FVL_CANC_ICMS write FVL_CANC_ICMS;
    property VL_DESC_ICMS: Currency read FVL_DESC_ICMS write FVL_DESC_ICMS;
    property VL_ACMO_ICMS: Currency read FVL_ACMO_ICMS write FVL_ACMO_ICMS;
    property VL_OP_ICMS: Currency read FVL_OP_ICMS write FVL_OP_ICMS;
    property VL_BC_ICMS: Currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: Currency read FVL_ICMS write FVL_ICMS;
    property VL_ISN: Currency read FVL_ISN write FVL_ISN;
    property VL_NT: Currency read FVL_NT write FVL_NT;
    property VL_ICMS_ST: Currency read FVL_ICMS_ST write FVL_ICMS_ST;
  end;

  /// Registro A360 - ITENS DO DOCUMENTO

  { TRegistroA360 }

  TRegistroA360 = class
  private
    FNUM_ITEM: Integer; /// Número seqüencial do item no documento fiscal
    FCOD_ITEM: String; /// Código do item
    FUNID: String; /// Unidade do item
    FCTISS: String; /// Código de Tributação do ISS

    FQTD: Double; /// Quantidade do item
    FQTDCANC: Integer;

    FVL_CANC_I: Currency;
    FVL_RT_ISS_I: Currency;
    FVL_UNIT: Currency; /// Valor unitário

    FVL_DESC_I: Currency; /// Valor do desconto
    FVL_ACMO_I: Currency; /// Valor do acréscimo
    FVL_ITEM: Currency; /// Valor do item

    FVL_BC_ISS_I: Currency; /// Valor da base de cálculo do ISS
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS_I: Currency; /// Valor do ISS
    FVL_ISN_ISS_I: Currency; /// Valor da prestação isenta do ISS
    FVL_NT_ISS_I: Currency; /// Valor da prestação sob não-incidência ou não-tributada pelo ISS

    FRegistroA365: TRegistroA365;
  public
    constructor Create(AOwner: TRegistroA350); virtual; /// Create

    property NUM_ITEM: Integer read FNUM_ITEM write FNUM_ITEM;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property VL_UNIT: Currency read FVL_UNIT write FVL_UNIT;
    property QTD: Double read FQTD write FQTD;
    property QTDCANC: Integer read FQTDCANC write FQTDCANC;
    property VL_DESC_I: Currency read FVL_DESC_I write FVL_DESC_I;
    property VL_ACMO_I: Currency read FVL_ACMO_I write FVL_ACMO_I;
    property VL_CANC_I: Currency read FVL_CANC_I write FVL_CANC_I;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property CTISS: String read FCTISS write FCTISS;
    property VL_BC_ISS_I: Currency read FVL_BC_ISS_I write FVL_BC_ISS_I;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS_I: Currency read FVL_ISS_I write FVL_ISS_I;
    property VL_ISN_ISS_I: Currency read FVL_ISN_ISS_I write FVL_ISN_ISS_I;
    property VL_NT_ISS_I: Currency read FVL_NT_ISS_I write FVL_NT_ISS_I;
    property VL_RT_ISS_I: Currency read FVL_RT_ISS_I write FVL_RT_ISS_I;


    property RegistroA365: TRegistroA365 read FRegistroA365 write FRegistroA365;
  end;

  /// Registro A360 - Lista

  { TRegistroA360List }

  TRegistroA360List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA360;
    procedure SetItem(Index: Integer; const Value: TRegistroA360);
  public
    function New(AOwner: TRegistroA350): TRegistroA360;
    property Items[Index: Integer]: TRegistroA360 read GetItem write SetItem;
  end;

  /// Registro A365 - COMPLEMENTO DO ITEM/ICMS

  { TRegistroA365 }

  TRegistroA365 = class
  private
    FVL_BC_ICMS_I: Currency; // Valor da base de cálculo do ICMS
    FALIQ_ICMS: Currency; /// Alíquota do ICMS
    FVL_ICMS_I: Currency; /// Valor do ICMS
    FVL_ISN_I: Currency; /// Valor da operação isenta do ICMS
    FVL_NT_I: Currency; /// Valor da operação não-tributada pelo ICMS
    FVL_ICMS_ST_I: Currency; /// Valor da operação com substituição tributária do ICMS
  public
    constructor Create(AOwner: TRegistroA360); virtual; /// Create

    property VL_BC_ICMS_I: Currency read FVL_BC_ICMS_I write FVL_BC_ICMS_I;
    property ALIQ_ICMS: Currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_ICMS_I: Currency read FVL_ICMS_I write FVL_ICMS_I;
    property VL_ISN_I: Currency read FVL_ISN_I write FVL_ISN_I;
    property VL_NT_I: Currency read FVL_NT_I write FVL_NT_I;
    property VL_ICMS_ST_I: Currency read FVL_ICMS_ST_I write FVL_ICMS_ST_I;
  end;

  /// Registro A370 - DOCUMENTOS - RESUMO DE CUPOM FISCAL/ISS

  { TRegistroA370 }

  TRegistroA370 = class
  private
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FQTD_CANC: Double; /// Quantidade de documentos cancelados
    FECF_CX: Integer; /// Número do caixa atribuído ao ECF
    FECF_FAB: String; /// Número de série de fabricação do ECF
    FCRO: Integer; /// Posição do Contador de Reinício de Operação
    FCRZ: Integer; /// Posição do Contador de Redução Z
    FNUM_DOC_INI: Integer; /// Número do primeiro documento fiscal emitido no dia
    FNUM_DOC_FIN: Integer; /// Número do último documento fiscal emitido no dia
    FDT_DOC: TDateTime; /// Data da Redução Z
    FCOP: String; /// Código da classe da prestação
    FVL_DOC: Currency; /// Valor acumulado das vendas líquidas
    FVL_CANC_ISS: Currency; /// Valor acumulado dos cancelamentos referentes ao ISS
    FVL_CANC_ICMS: Currency; /// Valor total dos cancelamentos referentes ao ICMS
    FVL_DESC_ISS: Currency; /// Valor acumulado dos descontos referentes ao ISS
    FVL_DESC_ICMS: Currency; /// Valor total dos descontos referentes ao ICMS
    FVL_ACMO_ISS: Currency; /// Valor acumulado dos acréscimos referentes ao ISS
    FVL_ACMO_ICMS: Currency; /// Valor total dos acréscimos referentes ao ICMS
    FVL_OP_ICMS: Currency; /// Valor total das operações tributadas pelo ICMS
    FVL_BC_ICMS: Currency; /// Valor total da base de cálculo do ICMS
    FVL_ICMS: Currency; /// Valor total do ICMS
    FVL_ISN: Currency; /// Valor total das operações isentas do ICMS
    FVL_NT: Currency; /// Valor total das operações não-tributadas pelo ICMS
    FVL_ICMS_ST: Currency; /// Valor total das operações com substituição tributária do ICMS
    FVL_BC_ISS: Currency; /// Valor acumulado da base de cálculo do ISS
    FVL_ISS: Currency; /// Valor acumulado do ISS
    FVL_ISN_ISS: Currency; /// Valor acumulado das prestações isentas do ISS
    FVL_NT_ISS: Currency; /// Valor acumulado das prestações sob não-incidência ou não-tributadas pelo ISS
    FVL_RT_ISS: Currency; /// Valor acumulado das prestações com ISS retido por substituição tributária

    FRegistroA380: TRegistroA380List;
  public
    constructor Create(AOwner: TRegistroA001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property QTD_CANC: Double read FQTD_CANC write FQTD_CANC;
    property ECF_CX: Integer read FECF_CX write FECF_CX;
    property ECF_FAB: String read FECF_FAB write FECF_FAB;
    property CRO: Integer read FCRO write FCRO;
    property CRZ: Integer read FCRZ write FCRZ;
    property NUM_DOC_INI: Integer read FNUM_DOC_INI write FNUM_DOC_INI;
    property NUM_DOC_FIN: Integer read FNUM_DOC_FIN write FNUM_DOC_FIN;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property COP: String read FCOP write FCOP;
    property VL_DOC: Currency read FVL_DOC write FVL_DOC;
    property VL_CANC_ISS: Currency read FVL_CANC_ISS write FVL_CANC_ISS;
    property VL_CANC_ICMS: Currency read FVL_CANC_ICMS write FVL_CANC_ICMS;
    property VL_DESC_ISS: Currency read FVL_DESC_ISS write FVL_DESC_ISS;
    property VL_DESC_ICMS: Currency read FVL_DESC_ICMS write FVL_DESC_ICMS;
    property VL_ACMO_ISS: Currency read FVL_ACMO_ISS write FVL_ACMO_ISS;
    property VL_ACMO_ICMS: Currency read FVL_ACMO_ICMS write FVL_ACMO_ICMS;
    property VL_OP_ICMS: Currency read FVL_OP_ICMS write FVL_OP_ICMS;
    property VL_BC_ICMS: Currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: Currency read FVL_ICMS write FVL_ICMS;
    property VL_ISN: Currency read FVL_ISN write FVL_ISN;
    property VL_NT: Currency read FVL_NT write FVL_NT;
    property VL_ICMS_ST: Currency read FVL_ICMS_ST write FVL_ICMS_ST;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property VL_ISN_ISS: Currency read FVL_ISN_ISS write FVL_ISN_ISS;
    property VL_NT_ISS: Currency read FVL_NT_ISS write FVL_NT_ISS;
    property VL_RT_ISS: Currency read FVL_RT_ISS write FVL_RT_ISS;

    property RegistroA380: TRegistroA380List read FRegistroA380 write FRegistroA380;
  end;

  /// Registro A370 - Lista

  { TRegistroA370List }

  TRegistroA370List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA370;
    procedure SetItem(Index: Integer; const Value: TRegistroA370);
  public
    function New(AOwner: TRegistroA001): TRegistroA370;
    property Items[Index: Integer]: TRegistroA370 read GetItem write SetItem;
  end;

  /// Registro A380 - ITENS DOS DOCUMENTOS

  { TRegistroA380 }

  TRegistroA380 = class
  private
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FQTD_CANC: Double; /// Quantidade cancelada acumulada, nos casos de cancelamento parcial de item
    FECF_CX: Integer; /// Número do caixa (número de ordem seqüencial do ECF)
    FECF_FAB: Integer; /// Número de série de fabricação do ECF
    FCRO: Integer; /// Posição do Contador de Reinício de Operação
    FCRZ: Integer; /// Posição do Contador de Redução Z
    FNUM_DOC_INI: Integer; /// Número do primeiro documento fiscal (CCF) emitido no dia
    FNUM_DOC_FIN: Integer; /// Número do último documento fiscal (CCF) emitido no dia
    FDT_DOC: TDateTime; /// Data da Redução Z
    FCOP: String; /// Código da classe da prestação
    FVL_DOC: Currency; /// Valor total das vendas líquidas
    FVL_CANC_ISS: Currency; /// Valor total dos cancelamentos referentes ao ISS
    FVL_CANC_ICMS: Currency; /// Valor total dos cancelamentos referentes ao ICMS
    FVL_DESC_ISS: Currency; /// Valor total dos descontos referentes ao ISS
    FVL_DESC_ICMS: Currency; /// Valor total dos descontos referentes ao ICMS
    FVL_ACMO_ISS: Currency; /// Valor total dos acréscimos referentes ao ISS
    FVL_ACMO_ICMS: Currency; /// Valor total dos acréscimos referentes ao ICMS
  public
    constructor Create(AOwner: TRegistroA370); virtual; /// Create

    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property QTD_CANC: Double read FQTD_CANC write FQTD_CANC;
    property ECF_CX: Integer read FECF_CX write FECF_CX;
    property ECF_FAB: Integer read FECF_FAB write FECF_FAB;
    property CRO: Integer read FCRO write FCRO;
    property CRZ: Integer read FCRZ write FCRZ;
    property NUM_DOC_INI: Integer read FNUM_DOC_INI write FNUM_DOC_INI;
    property NUM_DOC_FIN: Integer read FNUM_DOC_FIN write FNUM_DOC_FIN;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property COP: String read FCOP write FCOP;
    property VL_DOC: Currency read FVL_DOC write FVL_DOC;
    property VL_CANC_ISS: Currency read FVL_CANC_ISS write FVL_CANC_ISS;
    property VL_CANC_ICMS: Currency read FVL_CANC_ICMS write FVL_CANC_ICMS;
    property VL_DESC_ISS: Currency read FVL_DESC_ISS write FVL_DESC_ISS;
    property VL_DESC_ICMS: Currency read FVL_DESC_ICMS write FVL_DESC_ICMS;
    property VL_ACMO_ISS: Currency read FVL_ACMO_ISS write FVL_ACMO_ISS;
    property VL_ACMO_ICMS: Currency read FVL_ACMO_ICMS write FVL_ACMO_ICMS;
  end;

  /// Registro A380 - Lista

  { TRegistroA380List }

  TRegistroA380List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroA380;
    procedure SetItem(Index: Integer; const Value: TRegistroA380);
  public
    function New(AOwner: TRegistroA370): TRegistroA380;
    property Items[Index: Integer]: TRegistroA380 read GetItem write SetItem;
  end;

  /// Registro A990 - Encerramento do Bloco A

  { TRegistroA990 }

  TRegistroA990 = class
  private
    fQTD_LIN_A: Integer;
  public
    property QTD_LIN_A: Integer read fQTD_LIN_A write fQTD_LIN_A;
  end;

implementation

{ TRegistroA001 }

constructor TRegistroA001.Create;
begin
  FRegistroA320 := TRegistroA320List.Create;
  FRegistroA020 := TRegistroA020List.Create;
  FRegistroA300 := TRegistroA300List.Create;
  FRegistroA370 := TRegistroA370List.Create;
  FRegistroA350 := TRegistroA350List.Create;
  //
  IND_MOV := imlSemDados;
end;

destructor TRegistroA001.Destroy;
begin
  FRegistroA320.Free;
  FRegistroA020.Free;
  FRegistroA300.Free;
  FRegistroA370.Free;
  FRegistroA350.Free;
  inherited;
end;

{ TRegistroA020 }

constructor TRegistroA020.Create(AOwner: TRegistroA001);
begin
  FRegistroA025 := TRegistroA025List.Create;
  FRegistroA030 := TRegistroA030.Create(Self);
  FRegistroA035 := TRegistroA035.Create(Self);
  FRegistroA040 := TRegistroA040.Create(Self);
  FRegistroA050 := TRegistroA050List.Create;
  FRegistroA200 := TRegistroA200List.Create;
end;

destructor TRegistroA020.Destroy;
begin
  FRegistroA025.Free;
  FRegistroA030.Free;
  FRegistroA035.Free;
  FRegistroA040.Free;
  FRegistroA050.Free;
  FRegistroA200.Free;
  inherited;
end;

{ TRegistroA020List }

function TRegistroA020List.GetItem(Index: Integer): TRegistroA020;
begin
  Result := TRegistroA020(Get(Index));
end;

function TRegistroA020List.New(AOwner: TRegistroA001): TRegistroA020;
begin
  Result := TRegistroA020.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA020List.SetItem(Index: Integer; const Value: TRegistroA020);
begin
  Put(Index, Value);
end;

{ TRegistroA025 }

constructor TRegistroA025.Create(AOwner: TRegistroA020);
begin
end;

{ TRegistroA025List }

function TRegistroA025List.GetItem(Index: Integer): TRegistroA025;
begin
  Result := TRegistroA025(Get(Index));
end;

function TRegistroA025List.New(AOwner: TRegistroA020): TRegistroA025;
begin
  Result := TRegistroA025.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA025List.SetItem(Index: Integer; const Value: TRegistroA025);
begin
  Put(Index, Value);
end;

{ TRegistroA030 }

constructor TRegistroA030.Create(AOwner: TRegistroA020);
begin
end;

{ TRegistroA035 }

constructor TRegistroA035.Create(AOwner: TRegistroA020);
begin
end;

{ TRegistroA040 }

constructor TRegistroA040.Create(AOwner: TRegistroA020);
begin
  FRegistroA045 := TRegistroA045.Create(Self);
end;

destructor TRegistroA040.Destroy;
begin
  FRegistroA045.Free;
  inherited;
end;

{ TRegistroA045 }

constructor TRegistroA045.Create(AOwner: TRegistroA040);
begin
end;

{ TRegistroA050 }

constructor TRegistroA050.Create(AOwner: TRegistroA020);
begin
  FRegistroA055 := TRegistroA055List.Create;
end;

destructor TRegistroA050.Destroy;
begin
  FRegistroA055.Free;
  inherited;
end;

{ TRegistroA050List }

function TRegistroA050List.GetItem(Index: Integer): TRegistroA050;
begin
  Result := TRegistroA050(Get(Index));
end;

function TRegistroA050List.New(AOwner: TRegistroA020): TRegistroA050;
begin
  Result := TRegistroA050.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA050List.SetItem(Index: Integer; const Value: TRegistroA050);
begin
  Put(Index, Value);
end;

{ TRegistroA055 }

constructor TRegistroA055.Create(AOwner: TRegistroA050);
begin
end;

{ TRegistroA055List }

function TRegistroA055List.GetItem(Index: Integer): TRegistroA055;
begin
  Result := TRegistroA055(Get(Index));
end;

function TRegistroA055List.New(AOwner: TRegistroA050): TRegistroA055;
begin
  Result := TRegistroA055.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA055List.SetItem(Index: Integer; const Value: TRegistroA055);
begin
  Put(Index, Value);
end;

{ TRegistroA200 }

constructor TRegistroA200.Create(AOwner: TRegistroA020);
begin
end;

{ TRegistroA200List }

function TRegistroA200List.GetItem(Index: Integer): TRegistroA200;
begin
  Result := TRegistroA200(Get(Index));
end;

function TRegistroA200List.New(AOwner: TRegistroA020): TRegistroA200;
begin
  Result := TRegistroA200.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA200List.SetItem(Index: Integer; const Value: TRegistroA200);
begin
  Put(Index, Value);
end;

{ TRegistroA300 }

constructor TRegistroA300.Create(AOwner: TRegistroA001);
begin
  FRegistroA310 := TRegistroA310List.Create;
end;

destructor TRegistroA300.Destroy;
begin
  FRegistroA310.Free;
  inherited;
end;

{ TRegistroA300List }

function TRegistroA300List.GetItem(Index: Integer): TRegistroA300;
begin
  Result := TRegistroA300(Get(Index));
end;

function TRegistroA300List.New(AOwner: TRegistroA001): TRegistroA300;
begin
  Result := TRegistroA300.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA300List.SetItem(Index: Integer; const Value: TRegistroA300);
begin
  Put(Index, Value);
end;

{ TRegistroA310 }

constructor TRegistroA310.Create(AOwner: TRegistroA300);
begin
end;

{ TRegistroA310List }

function TRegistroA310List.GetItem(Index: Integer): TRegistroA310;
begin
  Result := TRegistroA310(Get(Index));
end;

function TRegistroA310List.New(AOwner: TRegistroA300): TRegistroA310;
begin
  Result := TRegistroA310.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA310List.SetItem(Index: Integer; const Value: TRegistroA310);
begin
  Put(Index, Value);
end;

{ TRegistroA320 }

constructor TRegistroA320.Create(AOwner: TRegistroA001);
begin
  FRegistroA330 := TRegistroA330List.Create;
end;

destructor TRegistroA320.Destroy;
begin
  FRegistroA330.Free;
  inherited;
end;

{ TRegistroA320List }

function TRegistroA320List.GetItem(Index: Integer): TRegistroA320;
begin
  Result := TRegistroA320(Get(Index));
end;

function TRegistroA320List.New(AOwner: TRegistroA001): TRegistroA320;
begin
  Result := TRegistroA320.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA320List.SetItem(Index: Integer; const Value: TRegistroA320);
begin
  Put(Index, Value);
end;

{ TRegistroA330 }

constructor TRegistroA330.Create(AOwner: TRegistroA320);
begin
end;

{ TRegistroA330List }

function TRegistroA330List.GetItem(Index: Integer): TRegistroA330;
begin
  Result := TRegistroA330(Get(Index));
end;

function TRegistroA330List.New(AOwner: TRegistroA320): TRegistroA330;
begin
  Result := TRegistroA330.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA330List.SetItem(Index: Integer; const Value: TRegistroA330);
begin
  Put(Index, Value);
end;

{ TRegistroA350 }

constructor TRegistroA350.Create(AOwner: TRegistroA001);
begin
  FRegistroA360 := TRegistroA360List.Create;
end;

destructor TRegistroA350.Destroy;
begin
  FRegistroA360.Free;
  inherited;
end;

{ TRegistroA350List }

function TRegistroA350List.GetItem(Index: Integer): TRegistroA350;
begin
  Result := TRegistroA350(Get(Index));
end;

function TRegistroA350List.New(AOwner: TRegistroA001): TRegistroA350;
begin
  Result := TRegistroA350.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA350List.SetItem(Index: Integer; const Value: TRegistroA350);
begin
  Put(Index, Value);
end;

{ TRegistroA355 }

constructor TRegistroA355.Create(AOwner: TRegistroA350);
begin
end;

{ TRegistroA360 }

constructor TRegistroA360.Create(AOwner: TRegistroA350);
begin
end;

{ TRegistroA360List }

function TRegistroA360List.GetItem(Index: Integer): TRegistroA360;
begin
  Result := TRegistroA360(Get(Index));
end;

function TRegistroA360List.New(AOwner: TRegistroA350): TRegistroA360;
begin
  Result := TRegistroA360.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA360List.SetItem(Index: Integer; const Value: TRegistroA360);
begin
  Put(Index, Value);
end;

{ TRegistroA365 }

constructor TRegistroA365.Create(AOwner: TRegistroA360);
begin
end;

{ TRegistroA370 }

constructor TRegistroA370.Create(AOwner: TRegistroA001);
begin
  FRegistroA380 := TRegistroA380List.Create;
end;

destructor TRegistroA370.Destroy;
begin
  FRegistroA380.Free;
  inherited;
end;

{ TRegistroA370List }

function TRegistroA370List.GetItem(Index: Integer): TRegistroA370;
begin
  Result := TRegistroA370(Get(Index));
end;

function TRegistroA370List.New(AOwner: TRegistroA001): TRegistroA370;
begin
  Result := TRegistroA370.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA370List.SetItem(Index: Integer; const Value: TRegistroA370);
begin
  Put(Index, Value);
end;

{ TRegistroA380 }

constructor TRegistroA380.Create(AOwner: TRegistroA370);
begin

end;

{ TRegistroA380List }

function TRegistroA380List.GetItem(Index: Integer): TRegistroA380;
begin
  Result := TRegistroA380(Get(Index));
end;

function TRegistroA380List.New(AOwner: TRegistroA370): TRegistroA380;
begin
  Result := TRegistroA380.Create(AOwner);
  Add(Result);
end;

procedure TRegistroA380List.SetItem(Index: Integer; const Value: TRegistroA380);
begin
  Put(Index, Value);
end;

end.
