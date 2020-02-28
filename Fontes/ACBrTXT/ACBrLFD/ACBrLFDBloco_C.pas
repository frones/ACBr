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

unit ACBrLFDBloco_C;

interface

uses
  SysUtils, Classes, DateUtils, ACBrLFDBlocos;

type

  TRegistroC020List = class;
  TRegistroC030List = class;
  TRegistroC035List = class;
  TRegistroC040 = class;
  TRegistroC050 = class;
  TRegistroC060 = class;
  TRegistroC200 = class;
  TRegistroC250 = class;
  TRegistroC255 = class;
  TRegistroC260List = class;
  TRegistroC300List = class;
  TRegistroC305 = class;
  TRegistroC310 = class;
  TRegistroC315 = class;
  TRegistroC320 = class;
  TRegistroC325 = class;
  TRegistroC500List = class;
  TRegistroC550List = class;
  TRegistroC555List = class;
  TRegistroC560List = class;
  TRegistroC570List = class;
  TRegistroC575List = class;
  TRegistroC580List = class;
  TRegistroC600List = class;
  TRegistroC605List = class;
  TRegistroC610List = class;
  TRegistroC615 = class;
  TRegistroC620List = class;
  TRegistroC625List = class;
  TRegistroC640List = class;
  TRegistroC700List = class;
  TRegistroC705 = class;
  TRegistroC720List = class;
  TRegistroC750List = class;
  TRegistroC755 = class;
  TRegistroC760List = class;
  TRegistroC770List = class;
  TRegistroC775List = class;
  TRegistroC780List = class;

  /// Registo C001 - ABERTURA DO BLOCO C

  { TRegistro C001 }

  TRegistroC001 = class(TOpenBlocos)
  private
    FRegistroC020: TRegistroC020List;
    FRegistroC550: TRegistroC550List;
    FRegistroC570: TRegistroC570List;
    FRegistroC600: TRegistroC600List;
    FRegistroC620: TRegistroC620List;
    FRegistroC700: TRegistroC700List;
    FRegistroC770: TRegistroC770List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroC020: TRegistroC020List read FRegistroC020 write FRegistroC020;
    property RegistroC550: TRegistroC550List read FRegistroC550 write FRegistroC550;
    property RegistroC570: TRegistroC570List read FRegistroC570 write FRegistroC570;
    property RegistroC600: TRegistroC600List read FRegistroC600 write FRegistroC600;
    property RegistroC620: TRegistroC620List read FRegistroC620 write FRegistroC620;
    property RegistroC700: TRegistroC700List read FRegistroC700 write FRegistroC700;
    property RegistroC770: TRegistroC770List read FRegistroC770 write FRegistroC770;
  end;

  /// Registro C020 - DOCUMENTO - NOTA FISCAL (CÓDIGO 01), NOTA FISCAL DE PRODUTOR (CÓDIGO 04)
  /// E NOTA FISCAL ELETRÔNICA (CÓDIGO 55)

  { TRegistroC020 }

  TRegistroC020 = class
  private
    FIND_FRT: TACBrTipoFrete;
    FIND_OPER: TACBrlTipoOperacao; // Indicador de operação
    FIND_EMIT: TACBrlEmitente; // Indicador do emitente
    FCOD_PART: String; // Código do participante
    FCOD_MOD: String; // Código do modelo do documento fiscal
    FCOD_SIT: TACBrlSituacaoDocto; // Código da situação do documento fiscal
    FSER: String; // Série do documento fiscal
    FNUM_DOC: String; // Número do documento fiscal
    FCHV_NFE: String; // Chave de acesso da Nota Fiscal Eletrônica
    FDT_EMIS: TDateTime; // Data da emissão do documento fiscal
    FDT_DOC: TDateTime; // Data da entrada ou da saída
    FCOD_NAT: String; // Código da natureza da operação ou prestação
    FIND_PGTO: TACBrlTipoPagamento; // Indicador do pagamento
    FVL_DOC: Currency; // Valor do documento fiscal
    FVL_DESC: Currency; // Valor dos descontos
    FVL_ACMO: Currency; // Valor dos acréscimos
    FVL_MERC: Currency; // Valor das mercadorias (valor dos itens)
    FVL_FRT: Currency; // Valor do frete
    FVL_SEG: Currency; // Valor do seguro
    FVL_OUT_DA: Currency; // Valor das outras despesas acessórias
    FVL_BC_ICMS: Currency; // Valor da base de cálculo do ICMS
    FVL_ICMS: Currency; // Valor do ICMS
    FVL_BC_ST: Currency; // Valor da base de cálculo do ICMS substituição tributária
    FVL_ICMS_ST: Currency; // Valor do ICMS da substituição tributária
    FVL_AT: Currency; // Valor da antecipação tributária, nas entradas
    FVL_IPI: Currency; // Valor do IPI
    FCOD_INF_OBS: String; // Código de referência a informação complementar

    FRegistroC030: TRegistroC030List;
    FRegistroC040: TRegistroC040;
    FRegistroC050: TRegistroC050;
    FRegistroC060: TRegistroC060;
    FRegistroC200: TRegistroC200;
    FRegistroC250: TRegistroC250;
    FRegistroC300: TRegistroC300List;
    FRegistroC500: TRegistroC500List;
  public
    constructor Create(AOwner: TRegistroC001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_OPER: TACBrlTipoOperacao read FIND_OPER write FIND_OPER;
    property IND_EMIT: TACBrlEmitente read FIND_EMIT write FIND_EMIT;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property COD_SIT: TACBrlSituacaoDocto read FCOD_SIT write FCOD_SIT;
    property SER: String read FSER write FSER;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property CHV_NFE: String read FCHV_NFE write FCHV_NFE;
    property DT_EMIS: TDateTime read FDT_EMIS write FDT_EMIS;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property COD_NAT: String read FCOD_NAT write FCOD_NAT;
    property IND_PGTO: TACBrlTipoPagamento read FIND_PGTO write FIND_PGTO;
    property VL_DOC: Currency read FVL_DOC write FVL_DOC;
    property VL_DESC: Currency read FVL_DESC write FVL_DESC;
    property VL_ACMO: Currency read FVL_ACMO write FVL_ACMO;
    property VL_MERC: Currency read FVL_MERC write FVL_MERC;
    property IND_FRT: TACBrTipoFrete read FIND_FRT write FIND_FRT;
    property VL_FRT: Currency read FVL_FRT write FVL_FRT;
    property VL_SEG: Currency read FVL_SEG write FVL_SEG;
    property VL_OUT_DA: Currency read FVL_OUT_DA write FVL_OUT_DA;
    property VL_BC_ICMS: Currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: Currency read FVL_ICMS write FVL_ICMS;
    property VL_BC_ST: Currency read FVL_BC_ST write FVL_BC_ST;
    property VL_ICMS_ST: Currency read FVL_ICMS_ST write FVL_ICMS_ST;
    property VL_AT: Currency read FVL_AT write FVL_AT;
    property VL_IPI: Currency read FVL_IPI write FVL_IPI;
    property COD_INF_OBS: String read FCOD_INF_OBS write FCOD_INF_OBS;

    property RegistroC030: TRegistroC030List read FRegistroC030 write FRegistroC030;
    property RegistroC040: TRegistroC040     read FRegistroC040 write FRegistroC040;
    property RegistroC050: TRegistroC050     read FRegistroC050 write FRegistroC050;
    property RegistroC060: TRegistroC060     read FRegistroC060 write FRegistroC060;
    property RegistroC200: TRegistroC200     read FRegistroC200 write FRegistroC200;
    property RegistroC250: TRegistroC250     read FRegistroC250 write FRegistroC250;
    property RegistroC300: TRegistroC300List read FRegistroC300 write FRegistroC300;
    property RegistroC500: TRegistroC500List read FRegistroC500 write FRegistroC500;
  end;

  /// Registro C020 - Lista

  { TRegistroC020List }

  TRegistroC020List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC020;
    procedure SetItem(Index: Integer; const Value: TRegistroC020);
  public
    function New(AOwner: TRegistroC001): TRegistroC020;
    property Items[Index: Integer]: TRegistroC020 read GetItem write SetItem;
  end;

  /// Registro C030 - COMPLEMENTO DO DOCUMENTO - FATURA

  { TRegistroC030 }

  TRegistroC030 = class
  private
    FIND_TIT: TACBrTipoTitulo;
    FDESCR_TIT: String;
    FNUM_TIT: String;
    FDT_TIT: TDateTime;
    FVL_TIT: Currency;
    FQTD_PARC: Integer;

    FRegistroC035: TRegistroC035List;
  public
    constructor Create(AOwner: TRegistroC020); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_TIT: TACBrTipoTitulo read FIND_TIT write FIND_TIT;
    property DESCR_TIT: String read FDESCR_TIT write FDESCR_TIT;
    property NUM_TIT: String read FNUM_TIT write FNUM_TIT;
    property DT_TIT: TDateTime read FDT_TIT write FDT_TIT;
    property VL_TIT: Currency read FVL_TIT write FVL_TIT;
    property QTD_PARC: Integer read FQTD_PARC write FQTD_PARC;

    property RegistroC035: TRegistroC035List read FRegistroC035 write FRegistroC035;
  end;

  /// Registro C030 - Lista

  { TRegistroC030List }

  TRegistroC030List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC030;
    procedure SetItem(Index: Integer; const Value: TRegistroC030);
  public
    function New(AOwner: TRegistroC020): TRegistroC030;
    property Items[Index: Integer]: TRegistroC030 read GetItem write SetItem;
  end;

  /// Registro C035 - VENCIMENTO DA FATURA

  { TRegistroC035 }

  TRegistroC035 = class
  private
    FNUM_PARC: Integer;
    FDT_VCTO: TDateTime;
    FVL_PARC: Currency;
  public
    constructor Create(AOwner: TRegistroC030); virtual; /// Create

    property NUM_PARC: Integer read FNUM_PARC write FNUM_PARC;
    property DT_VCTO: TDateTime read FDT_VCTO write FDT_VCTO;
    property VL_PARC: Currency read FVL_PARC write FVL_PARC;
  end;

  /// Registro C035 - Lista

  { TRegistroC035List }

  TRegistroC035List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC035;
    procedure SetItem(Index: Integer; const Value: TRegistroC035);
  public
    function New(AOwner: TRegistroC030): TRegistroC035;
    property Items[Index: Integer]: TRegistroC035 read GetItem write SetItem;
  end;

  /// Registro C040 - COMPLEMENTO DO DOCUMENTO - ISS

  { TRegistroC040 }

  TRegistroC040 = class
  private
    FCOD_MUN_SERV: Integer; // Código do município onde o serviço foi prestado
    FVL_OP_ISS: Currency; // Valor das operações tributadas pelo ISS
    FVL_BC_ISS: Currency; // Valor da base de cálculo do ISS
    FVL_ISS: Currency; // Valor do ISS
    FVL_BC_RT_ISS: Currency; // Valor da base de cálculo de retenção do ISS
    FVL_RT_ISS: Currency; // Valor do ISS retido pelo tomador
  public
    constructor Create(AOwner: TRegistroC020); virtual; /// Create

    property COD_MUN_SERV: Integer read FCOD_MUN_SERV write FCOD_MUN_SERV;
    property VL_OP_ISS: Currency read FVL_OP_ISS write FVL_OP_ISS;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property VL_BC_RT_ISS: Currency read FVL_BC_RT_ISS write FVL_BC_RT_ISS;
    property VL_RT_ISS: Currency read FVL_RT_ISS write FVL_RT_ISS;
  end;

  /// Registro C050 - COMPLEMENTO DO DOCUMENTO - PIS/COFINS

  { TRegistroC050 }

  TRegistroC050 = class
  private
    fALIQ_COFINS: Currency;
    fALIQ_PIS: Currency;
    fVL_COFINS: Currency;
    fVL_PIS: Currency;
  public
    constructor Create(AOwner: TRegistroC020); virtual; /// Create
    property ALIQ_PIS: Currency read fALIQ_PIS write fALIQ_PIS;
    property VL_PIS: Currency read fVL_PIS write fVL_PIS;
    property ALIQ_COFINS: Currency read fALIQ_COFINS write fALIQ_COFINS;
    property VL_COFINS: Currency read fVL_COFINS write fVL_COFINS;
  end;

  /// Registro C060 - COMPLEMENTO DO DOCUMENTO - DECLARAÇÃO/BASE DE CÁLCULO DE IMPORTAÇÃO

  { TRegistroC060 }

  TRegistroC060 = class
  private
    fVL_IOF: Currency;
    fVL_ADU_ICMS: Currency;
    fNUM_DI_DSI: Integer;
    fVL_PIS: Currency;
    fVL_MERC: Currency;
    fVL_COFINS: Currency;
    fVL_BC_IMP_IPI: Currency;
    fVL_ADU: Currency;
    fDT_REG: TDateTime;
    fVL_IPI: Currency;
    fDT_DES: TDateTime;
    fVL_BC_IMP_ICMS: Currency;
    fVL_II: Currency;
    fVL_ICMS: Currency;
  public
    constructor Create(AOwner: TRegistroC020); virtual; /// Create

    property NUM_DI_DSI: Integer read fNUM_DI_DSI write fNUM_DI_DSI;
    property DT_REG: TDateTime read fDT_REG write fDT_REG;
    property DT_DES: TDateTime read fDT_DES write fDT_DES;
    property VL_MERC: Currency read fVL_MERC write fVL_MERC;
    property VL_PIS: Currency read fVL_PIS write fVL_PIS;
    property VL_COFINS: Currency read fVL_COFINS write fVL_COFINS;
    property VL_ADU: Currency read fVL_ADU write fVL_ADU;
    property VL_ADU_ICMS: Currency read fVL_ADU_ICMS write fVL_ADU_ICMS;
    property VL_IOF: Currency read fVL_IOF write fVL_IOF;
    property VL_II: Currency read fVL_II write fVL_II;
    property VL_BC_IMP_IPI: Currency read fVL_BC_IMP_IPI write fVL_BC_IMP_IPI;
    property VL_IPI: Currency read fVL_IPI write fVL_IPI;
    property VL_BC_IMP_ICMS: Currency read fVL_BC_IMP_ICMS write fVL_BC_IMP_ICMS;
    property VL_ICMS: Currency read fVL_ICMS write fVL_ICMS;
  end;

  /// Registro C200 - COMPLEMENTO DO DOCUMENTO - DADOS ADICIONAIS

  { TRegistroC200 }

  TRegistroC200 = class
  private
    fVL_FCP: Currency;
    fIND_F0: Boolean;
  public
    constructor Create(AOwner: TRegistroC020); virtual; /// Create

    property VL_FCP: Currency read fVL_FCP write fVL_FCP;
    property IND_F0: Boolean read fIND_F0 write fIND_F0;
  end;

  /// Registro C250 - COMPLEMENTO DO DOCUMENTO - TRANSPORTADOR

  { TRegistroC250 }

  TRegistroC250 = class
  private
    FIND_FRT: TACBrTipoFrete; // Indicador de frete
    FCOD_PART: String; // Código do participante
    FRTC: String; // Número de registro do transportador de carga
    FID_VEIC: String; // Identificação do veículo transportador
    FUF_VEIC: String; // Sigla da unidade da Federação de registro do veículo transportador
    FVOL: Double; // Volume transportado
    FQTD_VOL: Double; // Quantidade de volumes transportados
    FESPECIE: String; // Espécie dos volumes transportados
    FMARCA: String; // Marca dos volumes transportados
    FNUM: String; // Numeração dos volumes transportados
    FPESO_BRT: Double; // Peso bruto dos volumes transportados (em kg)
    FPESO_LIQ: Double; // Peso líquido dos volumes transportados (em kg)

    FRegistroC255: TRegistroC255;
    FRegistroC260: TRegistroC260List;
  public
    constructor Create(AOwner: TRegistroC020); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_FRT: TACBrTipoFrete read FIND_FRT write FIND_FRT;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property RTC: String read FRTC write FRTC;
    property ID_VEIC: String read FID_VEIC write FID_VEIC;
    property UF_VEIC: String read FUF_VEIC write FUF_VEIC;
    property VOL: Double read FVOL write FVOL;
    property QTD_VOL: Double read FQTD_VOL write FQTD_VOL;
    property ESPECIE: String read FESPECIE write FESPECIE;
    property MARCA: String read FMARCA write FMARCA;
    property NUM: String read FNUM write FNUM;
    property PESO_BRT: Double read FPESO_BRT write FPESO_BRT;
    property PESO_LIQ: Double read FPESO_LIQ write FPESO_LIQ;

    property RegistroC255: TRegistroC255 read FRegistroC255 write FRegistroC255;
    property RegistroC260: TRegistroC260List read FRegistroC260 write FRegistroC260;
  end;

  /// Registro C255 - Coleta e Entrega

  { TRegistroC255 }

  TRegistroC255 = class
  private
    FIND_MOD: TACBrTipoTransporte; // Indicador do modal do transporte
    FCNPJ_COL: String; // CNPJ do contribuinte do local de coleta
    FUF_COL: String; // Sigla da unidade da Federação do contribuinte do local de coleta
    FIE_COL: String; // Inscrição Estadual do contribuinte do local de coleta
    FCOD_MUN_COL: Integer; // Código do município do local de coleta
    FIM_COL: String; // Inscrição Municipal do contribuinte do local de coleta
    FEND_COL: String; // Endereço completo do local de coleta
    FCNPJ_ENTG: String; // CNPJ do contribuinte do local de entrega
    FUF_ENTG: String; // Sigla da unidade da Federação do contribuinte do local de entrega
    FIE_ENTG: String; // Inscrição Estadual do contribuinte do local de entrega
    FCOD_MUN_ENTG: String; // Código do município do local de entrega
    FIM_ENTG: String; // Inscrição Municipal do contribuinte do local de entrega
    FEND_ENTG: String; // Endereço completo do local de entrega
  public
    constructor Create(AOwner: TRegistroC250); virtual;

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
    property COD_MUN_ENTG: String read FCOD_MUN_ENTG write FCOD_MUN_ENTG;
    property IM_ENTG: String read FIM_ENTG write FIM_ENTG;
    property END_ENTG: String read FEND_ENTG write FEND_ENTG;
  end;

  /// Registro C260 - Veículo Composto

  { TRegistroC260 }

  TRegistroC260 = class
  private
    FRTC: String;
    FID_VEIC: String;
    FUF_VEIC: String;
    FVEIC_DESCR: String;
  public
    constructor Create(AOwner: TRegistroC250); virtual; /// Create

    property RTC: String read FRTC write FRTC;
    property ID_VEIC: String read FID_VEIC write FID_VEIC;
    property UF_VEIC: String read FUF_VEIC write FUF_VEIC;
    property VEIC_DESCR: String read FVEIC_DESCR write FVEIC_DESCR;
  end;

  /// Registro C260 - Lista

  { TRegistroC260List }

  TRegistroC260List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC260;
    procedure SetItem(Index: Integer; const Value: TRegistroC260);
  public
    function New(AOwner: TRegistroC250): TRegistroC260;
    property Items[Index: Integer]: TRegistroC260 read GetItem write SetItem;
  end;

  /// Registro C265 - Complemento do documento - frete do transportador não-inscrito

  { TRegistroC265 }

  TRegistroC265 = class
  private
    FCOD_MUN_NI: Integer; // Código do município de ocorrência do fato gerador do ICMS do frete do transportador não-inscrito
    FVL_BC_ST_NI: Currency; // Valor da base de cálculo do ICMS da substituição tributária do frete do transportador não-inscrito
    FALIQ_ST_NI: Currency; // Alíquota do ICMS da substituição tributária do frete do transportador não-inscrito
    FVL_ICMS_ST_NI: Currency; // Valor do ICMS da substituição tributária do frete do transportador não-inscrito
  public
    constructor Create(AOwner: TRegistroC250); virtual; /// Create

    property COD_MUN_NI: Integer read FCOD_MUN_NI write FCOD_MUN_NI;
    property VL_BC_ST_NI: Currency read FVL_BC_ST_NI write FVL_BC_ST_NI;
    property ALIQ_ST_NI: Currency read FALIQ_ST_NI write FALIQ_ST_NI;
    property VL_ICMS_ST_NI: Currency read FVL_ICMS_ST_NI write FVL_ICMS_ST_NI;
  end;

  /// Registro C300 - Itens do Documento

  { TRegistroC300 }

  TRegistroC300 = class
  private
    FNUM_ITEM: Integer; /// Número seqüencial do item no documento fiscal
    FCOD_ITEM: String; /// Código do item (campo 02 do Registro 0200)
    FUNID: String; /// Unidade do item
    FVL_UNIT: Currency; /// Valor unitário
    FQTD: Double; /// Quantidade do item
    FVL_DESC_I: Currency; /// Valor do desconto
    FVL_ACMO_I: Currency; // Valor do acréscimo
    FVL_ITEM: Currency; /// Valor do item
    FNCM: String; /// Código da Nomenclatura Comum do Mercosul
    FCST: String; /// Código da Situação Tributária
    FCFOP: Integer; /// Código Fiscal de Operação e Prestação
    FVL_BC_ICMS_I: Currency; /// Valor da base de cálculo do ICMS
    FALIQ_ICMS: Currency; /// Alíquota do ICMS
    FVL_ICMS_I: Currency; /// Valor do ICMS
    FVL_BC_ST_I: Currency; /// Valor da base de cálculo referente à substituição
    FALIQ_ST: Currency; /// Alíquota do ICMS da substituição tributária na
    FVL_ICMS_ST_I: Currency; /// Valor do ICMS da substituição tributária
    FVL_BC_IPI: Currency; /// Valor da base de cálculo do IPI
    FALIQ_IPI: Currency; /// Alíquota do IPI
    FVL_IPI_I: Currency; /// Valor do IPI

    FRegistroC320: TRegistroC320;
    FRegistroC310: TRegistroC310;
    FRegistroC325: TRegistroC325;
    FRegistroC305: TRegistroC305;
    FRegistroC315: TRegistroC315;
  public
    constructor Create(AOwner: TRegistroC020); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property NUM_ITEM: Integer read FNUM_ITEM write FNUM_ITEM;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property VL_UNIT: Currency read FVL_UNIT write FVL_UNIT;
    property QTD: Double read FQTD write FQTD;
    property VL_DESC_I: Currency read FVL_DESC_I write FVL_DESC_I;
    property VL_ACMO_I: Currency read FVL_ACMO_I write FVL_ACMO_I;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property NCM: String read FNCM write FNCM;
    property CST: String read FCST write FCST;
    property CFOP: Integer read FCFOP write FCFOP;
    property VL_BC_ICMS_I: Currency read FVL_BC_ICMS_I write FVL_BC_ICMS_I;
    property ALIQ_ICMS: Currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_ICMS_I: Currency read FVL_ICMS_I write FVL_ICMS_I;
    property VL_BC_ST_I: Currency read FVL_BC_ST_I write FVL_BC_ST_I;
    property ALIQ_ST: Currency read FALIQ_ST write FALIQ_ST;
    property VL_ICMS_ST_I: Currency read FVL_ICMS_ST_I write FVL_ICMS_ST_I;
    property VL_BC_IPI: Currency read FVL_BC_IPI write FVL_BC_IPI;
    property ALIQ_IPI: Currency read FALIQ_IPI write FALIQ_IPI;
    property VL_IPI_I: Currency read FVL_IPI_I write FVL_IPI_I;

    property RegistroC305: TRegistroC305 read FRegistroC305 write FRegistroC305;
    property RegistroC310: TRegistroC310 read FRegistroC310 write FRegistroC310;
    property RegistroC315: TRegistroC315 read FRegistroC315 write FRegistroC315;
    property RegistroC320: TRegistroC320 read FRegistroC320 write FRegistroC320;
    property RegistroC325: TRegistroC325 read FRegistroC325 write FRegistroC325;
  end;

  /// Registro C300 - Lista

  { TRegistroC300List }

  TRegistroC300List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC300;
    procedure SetItem(Index: Integer; const Value: TRegistroC300);
  public
    function New(AOwner: TRegistroC020): TRegistroC300;
    property Items[Index: Integer]: TRegistroC300 read GetItem write SetItem;
  end;

  /// Registro C305 - Complemento do Item - Qualificação

  { TRegistroC305 }

  TRegistroC305 = class
  private
    fDESCR_COMPL: String; /// Descrição complementar do item
  public
    constructor Create(AOwner: TRegistroC300); virtual; /// Create

    property DESCR_COMPL: String read fDESCR_COMPL write fDESCR_COMPL;
  end;

  /// Registro C310 - Complemento do Item - Operações com ISS

  { TRegistroC310 }

  TRegistroC310 = class
  private
    FVL_BC_ISS_I: Currency; /// Valor da base de cálculo do ISS
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS_I: Currency; /// Valor do ISS
  public
    constructor Create(AOwner: TRegistroC300); virtual; /// Create

    property VL_BC_ISS_I: Currency read FVL_BC_ISS_I write FVL_BC_ISS_I;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS_I: Currency read FVL_ISS_I write FVL_ISS_I;
  end;

  /// Registro C315 - COMPLEMENTO DO ITEM - MEDICAMENTOS

  { TRegistroC315 }

  TRegistroC315 = class
  private
    FIND_MED: TACBrTipoProduto; /// Indicador de medicamento
    FLOTE: String; /// Número do lote de fabricação do medicamento
    FQTD: Double; /// Quantidade de itens do lote
    FDT_FAB: TDateTime; /// Data de fabricação do medicamento
    FDT_VAL: TDateTime; /// Data de expiração da validade do medicamento
    FIND_BC: TACBrTipoBaseMedicamento; /// Indicador de referência da base de cálculo do medicamento
    FVL_TAB_MAX: Currency; /// Valor do preço tabelado ou valor do preço máximo
  public
    constructor Create(AOwner: TRegistroC300); virtual; /// Create

    property IND_MED: TACBrTipoProduto read FIND_MED write FIND_MED;
    property LOTE: String read FLOTE write FLOTE;
    property QTD: Double read FQTD write FQTD;
    property DT_FAB: TDateTime read FDT_FAB write FDT_FAB;
    property DT_VAL: TDateTime read FDT_VAL write FDT_VAL;
    property IND_BC: TACBrTipoBaseMedicamento read FIND_BC write FIND_BC;
    property VL_TAB_MAX: Currency read FVL_TAB_MAX write FVL_TAB_MAX;
  end;

  /// Registro C320 - COMPLEMENTO DO ITEM - ARMAS DE FOGO

  { TRegistroC320 }

  TRegistroC320 = class
  private
    FIND_ARM: TACBrTipoArmaFogo; /// Indicador do tipo da arma de fogo
    FNUM_ARM: String; /// Numeração de série de fabricação da arma
    FNUM_CANO: String; /// Numeração de série de fabricação do cano
    FDESCR_COMPL: String; /// Descrição da arma
  public
    constructor Create(AOwner: TRegistroC300); virtual; /// Create

    property IND_ARM: TACBrTipoArmaFogo read FIND_ARM write FIND_ARM;
    property NUM_ARM: String read FNUM_ARM write FNUM_ARM;
    property NUM_CANO: String read FNUM_CANO write FNUM_CANO;
    property DESCR_COMPL: String read FDESCR_COMPL write FDESCR_COMPL;
  end;

  /// Registro C325 - COMPLEMENTO DO ITEM - VEÍCULOS NOVOS

  { TRegistroC325 }

  TRegistroC325 = class
  private
    FIND_VEIC_OPER: TACBrTipoOperacaoVeiculo; /// Indicador do tipo de operação com veículo
    FCNPJ: String; /// CNPJ do participante
    FCPF: String; /// CPF do participante
    FUF: String; /// Sigla da unidade da federação do participante
    FIE: String; /// Inscrição Estadual do participante
    FCHASSI: String; /// Chassi do veículo
  public
    constructor Create(AOwner: TRegistroC300); virtual; /// Create

    property IND_VEIC_OPER: TACBrTipoOperacaoVeiculo read FIND_VEIC_OPER write FIND_VEIC_OPER;
    property CNPJ: String read FCNPJ write FCNPJ;
    property CPF: String read FCPF write FCPF;
    property UF: String read FUF write FUF;
    property IE: String read FIE write FIE;
    property CHASSI: String read FCHASSI write FCHASSI;
  end;

  /// Registro C500 - Registro Analítico do Documento

  { TRegistroC500 }

  TRegistroC500 = class
  private
    fCST: String; /// Código da Situação Tributária
    fCFOP: String; /// Código Fiscal de Operação e Prestação
    fVL_CONT_P: Currency; /// Parcela correspondente ao “Valor Contábil”
    fVL_BC_ICMS_P: Currency; /// Parcela correspondente ao "Valor da base
    fALIQ_ICMS: Currency; /// Alíquota do ICMS
    fVL_ICMS_P: Currency; /// Parcela correspondente ao "Valor do ICMS"
    fVL_ICMS_ST_P: Currency; /// Parcela correspondente ao "Valor do ICMS da substituição tributária"
    fVL_IPI_P: Currency; /// Parcela correspondente ao "Valor do IPI"
  public
    constructor Create(AOwner: TRegistroC020); virtual; /// Create

    property CST: String read fCST write fCST;
    property CFOP: String read fCFOP write fCFOP;
    property VL_CONT_P: Currency read fVL_CONT_P write fVL_CONT_P;
    property VL_BC_ICMS_P: Currency read fVL_BC_ICMS_P write fVL_BC_ICMS_P;
    property ALIQ_ICMS: Currency read fALIQ_ICMS write fALIQ_ICMS;
    property VL_ICMS_P: Currency read fVL_ICMS_P write fVL_ICMS_P;
    property VL_ICMS_ST_P: Currency read fVL_ICMS_ST_P write fVL_ICMS_ST_P;
    property VL_IPI_P: Currency read fVL_IPI_P write fVL_IPI_P;
  end;

  /// Registro C500 - Lista

  { TRegistroC500List }

  TRegistroC500List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC500;
    procedure SetItem(Index: Integer; const Value: TRegistroC500);
  public
    function New(AOwner: TRegistroC020): TRegistroC500;
    property Items[Index: Integer]: TRegistroC500 read GetItem write SetItem;
  end;

  /// Registro C550 - DOCUMENTO - NOTA FISCAL DE VENDA A CONSUMIDOR

  { TRegistroC550 }

  TRegistroC550 = class
  private
    FCPF_CONS: String; // CPF do consumidor adquirente
    FCNPJ_CONS: String; // CNPJ do consumidor adquirente
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FCOD_SIT: TACBrlSituacaoDocto; /// Código da situação do documento fiscal
    FSER: String; /// Série do documento fiscal
    FSUB: String; /// Subsérie do documento fiscal
    FNUM_DOC: String; /// Número do documento fiscal
    FDT_DOC: TDateTime; /// Data da emissão do documento fiscal
    FCOP: String; // Código da classe da operação
    FVL_DOC: Currency; /// Valor total do documento fiscal
    FVL_DESC: Currency; /// Valor total do desconto
    FVL_ACMO: Currency; /// Valor total do desconto
    FVL_MERC: Currency; /// Valor das mercadorias
    FVL_BC_ICMS: Currency; /// Valor da base de cálculo do ICMS
    FVL_ICMS: Currency; /// Valor do ICMS
    FCOD_INF_OBS: String; /// Código de referência a informação

    FRegistroC555: TRegistroC555List;
  public
    constructor Create(AOwner: TRegistroC001); virtual; /// Create
    destructor Destroy; override; /// Destroy;

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
    property VL_MERC: Currency read FVL_MERC write FVL_MERC;
    property VL_BC_ICMS: Currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: Currency read FVL_ICMS write FVL_ICMS;
    property COD_INF_OBS: String read FCOD_INF_OBS write FCOD_INF_OBS;

    property RegistroC555: TRegistroC555List read FRegistroC555 write FRegistroC555;
  end;

  /// Registro C550 - Lista

  TRegistroC550List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC550;
    procedure SetItem(Index: Integer; const Value: TRegistroC550);
  public
    function New(AOwner: TRegistroC001): TRegistroC550;
    property Items[Index: Integer]: TRegistroC550 read GetItem write SetItem;
  end;



   /// Registro C555 - ITENS DO DOCUMENTO

  { TRegistroC555 }

  TRegistroC555 = class
  private
    FNUM_ITEM: Integer; ///  Número seqüencial do item
    FCOD_ITEM: String; /// Código do item
    FUNID: String; /// Unidade do item
    FVL_UNIT: Currency; /// Valor unitário
    FQTD: Double; /// Quantidade do item
    FVL_DESC_I: Currency; /// Valor do desconto
    FVL_ACMO_I: Currency; /// Valor do desconto
    FVL_ITEM: Currency; /// Valor do item
    FCST: String; /// Código da Situação Tributária
    FCFOP: String; /// Código Fiscal de Operação e Prestação
    FVL_BC_ICMS_I: Currency; /// Valor da base de cálculo do ICMS do item
    FALIQ_ICMS: Currency; /// Alíquota do ICMS
    FVL_ICMS_I: Currency; /// Valor do ICMS
  public
    constructor Create(AOwner: TRegistroC550); virtual; /// Create

    property NUM_ITEM: Integer read FNUM_ITEM write FNUM_ITEM;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property VL_UNIT: Currency read FVL_UNIT write FVL_UNIT;
    property QTD: Double read FQTD write FQTD;
    property VL_DESC_I: Currency read FVL_DESC_I write FVL_DESC_I;
    property VL_ACMO_I: Currency read FVL_ACMO_I write FVL_ACMO_I;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property CST: String read FCST write FCST;
    property CFOP: String read FCFOP write FCFOP;
    property VL_BC_ICMS_I: Currency read FVL_BC_ICMS_I write FVL_BC_ICMS_I;
    property ALIQ_ICMS: Currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_ICMS_I: Currency read FVL_ICMS_I write FVL_ICMS_I;
  end;

  /// Registro 555 - Lista

  { TRegistroC555List }

  TRegistroC555List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC555;
    procedure SetItem(Index: Integer; const Value: TRegistroC555);
  public
    function New(AOwner: TRegistroC550): TRegistroC555;
    property Items[Index: Integer]: TRegistroC555 read GetItem write SetItem;
  end;

  /// Registro C560 - ITENS DO DOCUMENTO

  { TRegistroC560 }

  TRegistroC560 = class
  private
    FNUM_ITEM: Integer; ///  Número seqüencial do item
    FCOD_ITEM: String; /// Código do item
    FUNID: String; /// Unidade do item
    FVL_UNIT: Currency; /// Valor unitário
    FQTD: Double; /// Quantidade do item
    FVL_DESC_I: Currency; /// Valor do desconto
    FVL_ACMO_I: Currency; /// Valor do desconto
    FVL_ITEM: Currency; /// Valor do item
    FCST: String; /// Código da Situação Tributária
    FCFOP: Integer; /// Código Fiscal de Operação e Prestação
    FVL_BC_ICMS_I: Currency; /// Valor da base de cálculo do ICMS do item
    FALIQ_ICMS: Currency; /// Alíquota do ICMS
    FVL_ICMS_I: Currency; /// Valor do ICMS
  public
    constructor Create(AOwner: TRegistroC550); virtual; /// Create

    property NUM_ITEM: Integer read FNUM_ITEM write FNUM_ITEM;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property VL_UNIT: Currency read FVL_UNIT write FVL_UNIT;
    property QTD: Double read FQTD write FQTD;
    property VL_DESC_I: Currency read FVL_DESC_I write FVL_DESC_I;
    property VL_ACMO_I: Currency read FVL_ACMO_I write FVL_ACMO_I;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property CST: String read FCST write FCST;
    property CFOP: Integer read FCFOP write FCFOP;
    property VL_BC_ICMS_I: Currency read FVL_BC_ICMS_I write FVL_BC_ICMS_I;
    property ALIQ_ICMS: Currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_ICMS_I: Currency read FVL_ICMS_I write FVL_ICMS_I;
  end;

  /// Registro 560 - Lista

  { TRegistroC560List }

  TRegistroC560List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC560;
    procedure SetItem(Index: Integer; const Value: TRegistroC560);
  public
    function New(AOwner: TRegistroC550): TRegistroC560;
    property Items[Index: Integer]: TRegistroC560 read GetItem write SetItem;
  end;

  /// Registro C570 - Documentos - Resumo Diário de Nota Fiscal de Venda a Consumidor (código 02)

  { TRegistroC570 }

  TRegistroC570 = class
  private
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FQTD_CANC: Integer; /// Quantidade de documentos cancelados
    FSER: String; /// Série do documento fiscal
    FSUB: String; /// Subsérie do documento fiscal
    FNUM_DOC_INI: String; /// Número do primeiro documento fiscal emitido
    FNUM_DOC_FIN: String; /// Número do último documento fiscal emitido
    FDT_DOC: TDateTime; /// Data da emissão dos documentos fiscais
    FCOP: String; /// Código da classe da operação
    FVL_DOC: Currency; /// Valor total acumulado
    FVL_MERC: Currency; /// Valor total das mercadorias
    FVL_BC_ICMS: Currency; /// Valor acumulado da base de cálculo do ICMS
    FVL_ICMS: Currency; /// Valor acumulado do ICMS

    FRegistroC575: TRegistroC575List;
  public
    constructor Create(AOwner: TRegistroC001); virtual; /// Create
    destructor Destroy; override; /// Destroy;

    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property NUM_DOC_INI: String read FNUM_DOC_INI write FNUM_DOC_INI;
    property NUM_DOC_FIN: String read FNUM_DOC_FIN write FNUM_DOC_FIN;
    property QTD_CANC: Integer read FQTD_CANC write FQTD_CANC;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property VL_DOC: Currency read FVL_DOC write FVL_DOC;
    property VL_BC_ICMS: Currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: Currency read FVL_ICMS write FVL_ICMS;

    property RegistroC575: TRegistroC575List read FRegistroC575 write FRegistroC575;
  end;

  /// Registro C570 - Lista

  { TRegistroC570List }

  TRegistroC570List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC570;
    procedure SetItem(Index: Integer; const Value: TRegistroC570);
  public
    function New(AOwner: TRegistroC001): TRegistroC570;
    property Items[Index: Integer]: TRegistroC570 read GetItem write SetItem;
  end;

  /// Registro C575 - Itens dos Documentos

  { TRegistroC575 }

  TRegistroC575 = class
  private
    FCOD_ITEM: Integer; /// Código do item
    FUNID: String; /// Unidade do item
    FQTD: Double; /// Quantidade acumulada do item
    FVL_ITEM: Currency; /// Valor acumulado do item
    FCST: String; /// Código da Situação Tributária
    FCFOP: Integer; /// Código Fiscal de Operação e Prestação
    FVL_BC_ICMS_I: Currency; /// Valor acumulado da base de cálculo do ICMS
    FALIQ_ICMS: Currency; /// Alíquota do ICMS
    FVL_ICMS_I: Currency; /// Valor do ICMS
  public
    constructor Create(AOwner: TRegistroC570); virtual; /// Create

    property COD_ITEM: Integer read FCOD_ITEM write FCOD_ITEM;
    property QTD: Double read FQTD write FQTD;
    property UNID: String read FUNID write FUNID;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property CST: String read FCST write FCST;
    property CFOP: Integer read FCFOP write FCFOP;
    property VL_BC_ICMS_I: Currency read FVL_BC_ICMS_I write FVL_BC_ICMS_I;
    property ALIQ_ICMS: Currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_ICMS_I: Currency read FVL_ICMS_I write FVL_ICMS_I;
  end;

  /// Registro C575 - Lista

  { TRegistroC575List }

  TRegistroC575List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC575;
    procedure SetItem(Index: Integer; const Value: TRegistroC575);
  public
    function New(AOwner: TRegistroC570): TRegistroC575;
    property Items[Index: Integer]: TRegistroC575 read GetItem write SetItem;
  end;

  /// Registro C580 - Resumo Mensal de Itens - Nota Fiscal de Venda a Consumidor (código 02)

  { TRegistroC580 }

  TRegistroC580 = class
  private
    fCOD_ITEM: String; /// Código do item
    fQTD: Double; /// Quantidade acumulada do item
    fQTD_CANC: Integer; /// Quantidade de documentos cancelados
    fUNID: String; /// Unidade do item
    fVL_ITEM: Currency; /// Valor acumulado do item
    fVL_DESC_I: Currency; /// Valor acumulado dos descontos
    fVL_BC_ICMS_I: Currency; /// Valor acumulado da base de cálculo do ICMS
    fALIQ_ICMS: Currency; /// Alíquota do ICMS
    fVL_ICMS_I: Currency; /// Valor acumulado do ICMS
  public
    constructor Create(AOwner: TRegistroC001); virtual; /// Create

    property COD_ITEM: String read fCOD_ITEM write fCOD_ITEM;
    property QTD: Double read fQTD write fQTD;
    property QTD_CANC: Integer read fQTD_CANC write fQTD_CANC;
    property UNID: String read fUNID write fUNID;
    property VL_ITEM: Currency read fVL_ITEM write fVL_ITEM;
    property VL_DESC_I: Currency read fVL_DESC_I write fVL_DESC_I;
    property VL_BC_ICMS_I: Currency read fVL_BC_ICMS_I write fVL_BC_ICMS_I;
    property ALIQ_ICMS: Currency read fALIQ_ICMS write fALIQ_ICMS;
    property VL_ICMS_I: Currency read fVL_ICMS_I write fVL_ICMS_I;
  end;

  /// Registro C580 - Lista

  { TRegistroC580List }

  TRegistroC580List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC580;
    procedure SetItem(Index: Integer; const Value: TRegistroC580);
  public
    function New(AOwner: TRegistroC001): TRegistroC580;
    property Items[Index: Integer]: TRegistroC580 read GetItem write SetItem;
  end;

  /// Registro C600 - Documento - Cupom Fiscal/ICMS (código 2D e código 02)

  { TRegistroC600 }

  TRegistroC600 = class
  private
    FCPF_CONS: String; /// Número de inscrição do adquirente no CPF
    FCNPJ_CONS: String; /// Número de inscrição do adquirente no CNPJ
    fCOD_MOD: String; /// Código do modelo do documento fiscal
    fCOD_SIT: TACBrlSituacaoDocto; /// Código da situação do documento fiscal
    fECF_CX: Integer; /// Número do caixa atribuído ao ECF
    fECF_FAB: String; /// Número de série de fabricação do ECF
    fCRO: Integer; /// Posição do Contador de Reinício de Operação
    fCRZ: Integer; /// Posição do Contador de Redução Z
    fNUM_DOC: String; /// Número do documento fiscal
    fDT_DOC: TDateTime; /// Data da emissão do documento fiscal
    FCOP: String; /// Código da classe da operação
    FVL_ACMO_ISS: Currency;
    FVL_CANC_ISS: Currency;
    FVL_DESC_ISS: Currency;
    fVL_DOC: Currency; /// Valor do documento fiscal
    FVL_CANC_ICMS: Currency; /// Valor dos cancelamentos referentes ao ICMS
    FVL_DESC_ICMS: Currency; /// Valor dos descontos referentes ao ICMS
    FVL_ACMO_ICMS: Currency; /// Valor dos acréscimos referentes ao ICMS
    FVL_BC_ICMS: Currency; /// Valor da base de cálculo do ICMS
    FVL_ICMS: Currency; /// Valor do ICMS
    FVL_ISN: Currency; /// Valor das operações isentas do ICMS
    FVL_ISS: Currency;
    FVL_NT: Currency; /// Valor das operações não-tributadas pelo ICMS
    FVL_ICMS_ST: Currency; /// Valor do ICMS da substituição tributária

    FRegistroC605: TRegistroC605List;
    FRegistroC610: TRegistroC610List;
    FVL_ST: Currency;
  public
    constructor Create(AOwner: TRegistroC001); virtual; /// Create
    destructor Destroy; override; /// Destroy;

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
    property VL_CANC_ICMS: Currency read FVL_CANC_ICMS write FVL_CANC_ICMS;
    property VL_DESC_ICMS: Currency read FVL_DESC_ICMS write FVL_DESC_ICMS;
    property VL_ACMO_ICMS: Currency read FVL_ACMO_ICMS write FVL_ACMO_ICMS;
    property VL_CANC_ISS: Currency read FVL_CANC_ISS write FVL_CANC_ISS;
    property VL_DESC_ISS: Currency read FVL_DESC_ISS write FVL_DESC_ISS;
    property VL_ACMO_ISS: Currency read FVL_ACMO_ISS write FVL_ACMO_ISS;
    property VL_BC_ICMS: Currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: Currency read FVL_ICMS write FVL_ICMS;
    property VL_ISN: Currency read FVL_ISN write FVL_ISN;
    property VL_NT: Currency read FVL_NT write FVL_NT;
    property VL_ST: Currency read FVL_ST write FVL_ST;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property VL_ICMS_ST: Currency read FVL_ICMS_ST write FVL_ICMS_ST;

    property RegistroC605: TRegistroC605List read FRegistroC605 write FRegistroC605;
    property RegistroC610: TRegistroC610List read FRegistroC610 write FRegistroC610;
  end;

  /// Registro C600 - Lista

  { TRegistroC600List }

  TRegistroC600List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC600;
    procedure SetItem(Index: Integer; const Value: TRegistroC600);
  public
    function New(AOwner: TRegistroC001): TRegistroC600;
    property Items[Index: Integer]: TRegistroC600 read GetItem write SetItem;
  end;

  /// Registro C605 - COMPLEMENTO DO DOCUMENTO - ISS

  { TRegistroC605 }

  TRegistroC605 = class
  private
    FALIQ_ICMS: Currency;
    FCFOP: String;
    FCOD_ITEM: String;
    FCST: String;
    FNUM_ITEM: Integer;
    FQTD: Double;
    FQTD_CANC_I: Double;
    FUNID: String;
    FVL_ACMO_I: Currency;
    FVL_BC_ICMS_I: Currency;
    FVL_CANC_I: Currency;
    FVL_DESC_I: Currency;
    FVL_ICMS_I: Currency;
    FVL_ICMS_ST_I: Currency;
    FVL_ISN_I: Currency;
    FVL_ISS: Currency;
    FVL_ITEM: Currency;
    FVL_NT_I: Currency;
    FVL_ST_I: Currency;
    FVL_UNIT: Currency;
  public
    constructor Create(AOwner: TRegistroC600); virtual; /// Create

    property NUM_ITEM: Integer read FNUM_ITEM write FNUM_ITEM;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property VL_UNIT: Currency read FVL_UNIT write FVL_UNIT;
    property QTD: Double read FQTD write FQTD;
    property QTD_CANC_I: Double read FQTD_CANC_I write FQTD_CANC_I;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property VL_DESC_I: Currency read FVL_DESC_I write FVL_DESC_I;
    property VL_CANC_I: Currency read FVL_CANC_I write FVL_CANC_I;
    property VL_ACMO_I: Currency read FVL_ACMO_I write FVL_ACMO_I;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property CST: String read FCST write FCST;
    property CFOP: String read FCFOP write FCFOP;
    property VL_BC_ICMS_I: Currency read FVL_BC_ICMS_I write FVL_BC_ICMS_I;
    property ALIQ_ICMS: Currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_ICMS_I: Currency read FVL_ICMS_I write FVL_ICMS_I;
    property VL_ISN_I: Currency read FVL_ISN_I write FVL_ISN_I;
    property VL_NT_I: Currency read FVL_NT_I write FVL_NT_I;
    property VL_ST_I: Currency read FVL_ST_I write FVL_ST_I;
    property VL_ICMS_ST_I: Currency read FVL_ICMS_ST_I write FVL_ICMS_ST_I;
  end;

  /// Registro C605 - Lista

  { TRegistroC605List }

  TRegistroC605List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC605;
    procedure SetItem(Index: Integer; const Value: TRegistroC605);
  public
    function New(AOwner: TRegistroC600): TRegistroC605;
    property Items[Index: Integer]: TRegistroC605 read GetItem write SetItem;
  end;

  /// Registro C610 - Itens do documento

  { TRegistroC610 }

  TRegistroC610 = class
  private
    FNUM_ITEM: Integer; /// Número seqüencial do item no documento fiscal
    FCOD_ITEM: String; /// Código do item (campo 02 da Linha 0200)
    FQTD_CANC_I: Double;
    FUNID: String; /// Unidade do item
    FVL_CANC_I: Currency;
    FVL_ISS: Currency;
    FVL_UNIT: Currency; /// Valor unitário
    FQTD: Double; /// Quantidade líquida do item, já computado o cancelamento parcial
    FVL_DESC_I: Currency; // Valor do desconto
    FVL_ACMO_I: Currency; /// Valor do acréscimo
    FVL_ITEM: Currency; /// Valor líquido do item, já computados o desconto ou acréscimo e o cancelamento parcial
    FCST: String; /// Código da Situação Tributária do ICMS
    FCFOP: Integer; /// Código Fiscal de Operações e Prestações
    FVL_BC_ICMS_I: Currency; /// Valor da base de cálculo do ICMS
    FALIQ_ICMS: Currency; /// Alíquota do ICMS
    FVL_ICMS_I: Currency; /// Valor do ICMS
    FVL_ISN_I: Currency; /// Valor da operação isenta do ICMS
    FVL_NT_I: Currency; /// Valor da operação não-tributada pelo ICMS
    FVL_ICMS_ST_I: Currency; /// Valor do ICMS da substituição tributária

    FRegistroC615: TRegistroC615;
  public
    constructor Create(AOwner: TRegistroC600); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property NUM_ITEM: Integer read FNUM_ITEM write FNUM_ITEM;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property VL_UNIT: Currency read FVL_UNIT write FVL_UNIT;
    property QTD: Double read FQTD write FQTD;
    property QTD_CANC_I: Double read FQTD_CANC_I write FQTD_CANC_I;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property VL_DESC_I: Currency read FVL_DESC_I write FVL_DESC_I;
    property VL_CANC_I: Currency read FVL_CANC_I write FVL_CANC_I;
    property VL_ACMO_I: Currency read FVL_ACMO_I write FVL_ACMO_I;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property CST: String read FCST write FCST;
    property CFOP: Integer read FCFOP write FCFOP;
    property VL_BC_ICMS_I: Currency read FVL_BC_ICMS_I write FVL_BC_ICMS_I;
    property ALIQ_ICMS: Currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_ICMS_I: Currency read FVL_ICMS_I write FVL_ICMS_I;
    property VL_ISN_I: Currency read FVL_ISN_I write FVL_ISN_I;
    property VL_NT_I: Currency read FVL_NT_I write FVL_NT_I;
    property VL_ICMS_ST_I: Currency read FVL_ICMS_ST_I write FVL_ICMS_ST_I;

    property RegistroC615: TRegistroC615 read FRegistroC615 write FRegistroC615;
  end;

  /// Registro C610 - Lista

  { TRegistroC610List }

  TRegistroC610List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC610;
    procedure SetItem(Index: Integer; const Value: TRegistroC610);
  public
    function New(AOwner: TRegistroC600): TRegistroC610;
    property Items[Index: Integer]: TRegistroC610 read GetItem write SetItem;
  end;

  /// Registro C615 - COMPLEMENTO DO ITEM - ISS

  { TRegistroC615 }

  TRegistroC615 = class
  private
    FVL_BC_ISS_I: Currency; /// Valor da base de cálculo do ISS
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS_I: Currency; /// Valor do ISS
    FVL_ISN_ISS_I: Currency; /// Valor da operação isenta do ISS
    FVL_NT_ISS_I: Currency; /// Valor da operação não-tributada pelo ISS
  public
    constructor Create(AOwner: TRegistroC610); virtual; /// Create

    property VL_BC_ISS_I: Currency read FVL_BC_ISS_I write FVL_BC_ISS_I;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS_I: Currency read FVL_ISS_I write FVL_ISS_I;
    property VL_ISN_ISS_I: Currency read FVL_ISN_ISS_I write FVL_ISN_ISS_I;
    property VL_NT_ISS_I: Currency read FVL_NT_ISS_I write FVL_NT_ISS_I;
  end;

  /// Registro C620 - DOCUMENTOS - RESUMO DE CUPOM FISCAL/ICMS (CÓDIGO 2D E CÓDIGO 02)

  { TRegistroC620 }

  TRegistroC620 = class
  private
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FQTD_CANC: Integer; /// Quantidade de documentos cancelados
    FECF_CX: Integer; /// Número do caixa atribuído ao ECF
    FECF_FAB: String; /// Número de série de fabricação do ECF
    FCRO: Integer; /// Posição do Contador de Reinício de Operação
    FCRZ: Integer; /// Posição do Contador de Redução Z
    FNUM_DOC_INI: String; /// Número do primeiro documento fiscal
    FNUM_DOC_FIN: String; /// Número do último documento fiscal
    FDT_DOC: TDateTime; /// Data da emissão dos documentos fiscais
    FCOP: String; /// Código da classe da operação
    FVL_DOC: Currency; /// Valor acumulado dos documentos fiscais
    FVL_CANC_ISS: Currency; /// Valor total dos cancelamentos referentes ao ISS
    FVL_CANC_ICMS: Currency; /// Valor acumulado dos cancelamentos referentes ao ICMS
    FVL_DESC_ISS: Currency; /// Valor total dos descontos referentes ao ISS
    FVL_DESC_ICMS: Currency; /// Valor acumulado dos descontos referentes ao ICMS
    FVL_ACMO_ISS: Currency; /// Valor total dos acréscimos referentes ao ISS
    FVL_ACMO_ICMS: Currency; /// Valor acumulado dos acréscimos referentes ao ICMS
    FVL_OP_ISS: Currency; /// Valor total das operações tributadas pelo ISS
    FVL_BC_ICMS: Currency; /// Valor acumulado da base de cálculo do ICMS
    FVL_ICMS: Currency; /// Valor acumulado do ICMS
    FVL_ISN: Currency; /// Valor total das operações isentas do ICMS
    FVL_NT: Currency; /// Valor acumulado das saídas sob não-incidência ou não-tributadas pelo ICMS
    FVL_ICMS_ST: Currency; /// Valor total do ICMS da substituição tributária
    FVL_BC_ISS: Currency; /// Valor total da base de cálculo do ISS
    FVL_ISS: Currency; /// Valor total do ISS
    FVL_ISN_ISS: Currency; /// Valor total da operação isenta do ISS
    FVL_NT_ISS: Currency; /// Valor total da operação não-tributada pelo ISS

    FRegistroC625: TRegistroC625List;
  public
    constructor Create(AOwner: TRegistroC001); virtual; /// Create
    destructor Destroy; override; /// Destroy;

    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property QTD_CANC: Integer read FQTD_CANC write FQTD_CANC;
    property ECF_CX: Integer read FECF_CX write FECF_CX;
    property ECF_FAB: String read FECF_FAB write FECF_FAB;
    property CRO: Integer read FCRO write FCRO;
    property CRZ: Integer read FCRZ write FCRZ;
    property NUM_DOC_INI: String read FNUM_DOC_INI write FNUM_DOC_INI;
    property NUM_DOC_FIN: String read FNUM_DOC_FIN write FNUM_DOC_FIN;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property COP: String read FCOP write FCOP;
    property VL_DOC: Currency read FVL_DOC write FVL_DOC;
    property VL_CANC_ISS: Currency read FVL_CANC_ISS write FVL_CANC_ISS;
    property VL_CANC_ICMS: Currency read FVL_CANC_ICMS write FVL_CANC_ICMS;
    property VL_DESC_ISS: Currency read FVL_DESC_ISS write FVL_DESC_ISS;
    property VL_DESC_ICMS: Currency read FVL_DESC_ICMS write FVL_DESC_ICMS;
    property VL_ACMO_ISS: Currency read FVL_ACMO_ISS write FVL_ACMO_ISS;
    property VL_ACMO_ICMS: Currency read FVL_ACMO_ICMS write FVL_ACMO_ICMS;
    property VL_OP_ISS: Currency read FVL_OP_ISS write FVL_OP_ISS;
    property VL_BC_ICMS: Currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: Currency read FVL_ICMS write FVL_ICMS;
    property VL_ISN: Currency read FVL_ISN write FVL_ISN;
    property VL_NT: Currency read FVL_NT write FVL_NT;
    property VL_ICMS_ST: Currency read FVL_ICMS_ST write FVL_ICMS_ST;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property VL_ISN_ISS: Currency read FVL_ISN_ISS write FVL_ISN_ISS;
    property VL_NT_ISS: Currency read FVL_NT_ISS write FVL_NT_ISS;

    property RegistroC625: TRegistroC625List read FRegistroC625 write FRegistroC625;
  end;

  /// Registro C620 - Lista

  { TRegistroC620List }

  TRegistroC620List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC620;
    procedure SetItem(Index: Integer; const Value: TRegistroC620);
  public
    function New(AOwner: TRegistroC001): TRegistroC620;
    property Items[Index: Integer]: TRegistroC620 read GetItem write SetItem;
  end;

  /// Registro C625 - Itens dos Documentos

  { TRegistroC625 }

  TRegistroC625 = class
  private
    FCOD_ITEM: String; /// Código do item
    FUNID: String; /// Unidade do item
    FQTD: Double; /// Quantidade acumulada do item
    FVL_DESC_I: Currency; /// Valor acumulado dos descontos
    FVL_ACMO_I: Currency; /// Valor acumulado dos acréscimos
    FVL_ITEM: Currency; /// Valor acumulado do item
    FCFOP: Integer; /// Código Fiscal de Operações e Prestações preponderante no dia
    FVL_BC_ICMS_I: Currency; /// Valor acumulado da base de cálculo do ICMS
    FALIQ_ICMS: Currency; /// Alíquota do ICMS
    FVL_ICMS_I: Currency; /// Valor acumulado do ICMS
    FVL_ISN_I: Currency; /// Valor acumulado das saídas isentas do ICMS
    FVL_NT_I: Currency; /// Valor acumulado das saídas sob não-incidência ou não-tributadas pelo ICMS
    FVL_ICMS_ST_I: Currency; /// Valor acumulado das saídas de mercadorias adquiridas
    FVL_BC_ISS_I: Currency; /// Valor total da base de cálculo do ISS
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS_I: Currency; /// Valor total do ISS
    FVL_ISN_ISS_I: Currency; /// Valor total da operação isenta do ISS
    FVL_NT_ISS_I: Currency; /// Valor total da operação não-tributada pelo ISS
  public
    constructor Create(AOwner: TRegistroC620); virtual; /// Create

    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property QTD: Double read FQTD write FQTD;
    property VL_DESC_I: Currency read FVL_DESC_I write FVL_DESC_I;
    property VL_ACMO_I: Currency read FVL_ACMO_I write FVL_ACMO_I;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property CFOP: Integer read FCFOP write FCFOP;
    property VL_BC_ICMS_I: Currency read FVL_BC_ICMS_I write FVL_BC_ICMS_I;
    property ALIQ_ICMS: Currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_ICMS_I: Currency read FVL_ICMS_I write FVL_ICMS_I;
    property VL_ISN_I: Currency read FVL_ISN_I write FVL_ISN_I;
    property VL_NT_I: Currency read FVL_NT_I write FVL_NT_I;
    property VL_ICMS_ST_I: Currency read FVL_ICMS_ST_I write FVL_ICMS_ST_I;
    property VL_BC_ISS_I: Currency read FVL_BC_ISS_I write FVL_BC_ISS_I;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS_I: Currency read FVL_ISS_I write FVL_ISS_I;
    property VL_ISN_ISS_I: Currency read FVL_ISN_ISS_I write FVL_ISN_ISS_I;
    property VL_NT_ISS_I: Currency read FVL_NT_ISS_I write FVL_NT_ISS_I;
  end;

  /// Registro C625 - Lista

  { TRegistroC625List }

  TRegistroC625List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC625;
    procedure SetItem(Index: Integer; const Value: TRegistroC625);
  public
    function New(AOwner: TRegistroC620): TRegistroC625;
    property Items[Index: Integer]: TRegistroC625 read GetItem write SetItem;
  end;

  /// Registro C640 - Resumo Mensal de Itens - Cupom Fiscal/ICMS (código 2D e código 02)

  { TRegistroC640 }

  TRegistroC640 = class
  private
    fCOD_ITEM: String; /// Código do item
    fQTD: Double; /// Quantidade acumulada do item
    fQTD_CANC_I: Double; /// Quantidade cancelada acumulada, no caso de cancelamento parcial de item
    fUNID: String; /// Unidade do item
    fVL_ITEM: Currency; /// Valor acumulado do item
    fVL_DESC_I: Currency; /// Valor acumulado dos descontos
    fVL_CANC_I: Currency; /// Valor acumulado dos cancelamentos
    fVL_ACMO_I: Currency; /// Valor acumulado dos acréscimos
    fVL_BC_ICMS_I: Currency; /// Valor acumulado da base de cálculo do ICMS
    fALIQ_ICMS: Currency; /// Alíquota do ICMS
    fVL_ICMS_I: Currency; /// Valor acumulado do ICMS
    fVL_ISEN_I: Currency; /// Valor acumulado das saídas isentas do ICMS
    fVL_NT_I: Currency; /// Valor acumulado das saídas sob não-incidência ou não-tributadas pelo ICMS
    fVL_ST_I: Currency; /// Valor acumulado das saídas de mercadorias adquiridas com substituição tributária do ICMS
  public
    constructor Create(AOwner: TRegistroC001); virtual; /// Create

    property COD_ITEM: String read fCOD_ITEM write fCOD_ITEM;
    property QTD: Double read fQTD write fQTD;
    property QTD_CANC_I: Double read fQTD_CANC_I write fQTD_CANC_I;
    property UNID: String read fUNID write fUNID;
    property VL_ITEM: Currency read fVL_ITEM write fVL_ITEM;
    property VL_DESC_I: Currency read fVL_DESC_I write fVL_DESC_I;
    property VL_CANC_I: Currency read fVL_CANC_I write fVL_CANC_I;
    property VL_ACMO_I: Currency read fVL_ACMO_I write fVL_ACMO_I;
    property VL_BC_ICMS_I: Currency read fVL_BC_ICMS_I write fVL_BC_ICMS_I;
    property ALIQ_ICMS: Currency read fALIQ_ICMS write fALIQ_ICMS;
    property VL_ICMS_I: Currency read fVL_ICMS_I write fVL_ICMS_I;
    property VL_ISEN_I: Currency read fVL_ISEN_I write fVL_ISEN_I;
    property VL_NT_I: Currency read fVL_NT_I write fVL_NT_I;
    property VL_ST_I: Currency read fVL_ST_I write fVL_ST_I;
  end;

  /// Registro C640 - Lista

  { TRegistroC640List }

  TRegistroC640List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC640;
    procedure SetItem(Index: Integer; const Value: TRegistroC640);
  public
    function New(AOwner: TRegistroC001): TRegistroC640;
    property Items[Index: Integer]: TRegistroC640 read GetItem write SetItem;
  end;

  /// Registro C700 - Documento - Nota Fiscal/Conta de Energia Elétrica (código 06) e Nota Fiscal/Conta de Fornecimento de Gás (código 28)

  { TRegistroC700 }

  TRegistroC700 = class
  private
    FIND_OPER: TACBrlTipoOperacao; /// Indicador do tipo de operação
    FIND_EMIT: TACBrlEmitente; /// Indicador do emitente do documento fiscal
    FCOD_PART: String; /// Código do participante (campo 02 do Registro 0150)
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FCOD_SIT: TACBrlSituacaoDocto; /// Código da situação do documento fiscal
    FSER: String; /// Série do documento fiscal
    FSUB: String; /// Subsérie do documento fiscal
    FCOD_ASS: Integer; /// Código do tipo de consumidor ou assinante
    FNUM_DOC: String; /// Número do documento fiscal
    FDT_EMIS: TDateTime; /// Data da emissão do documento fiscal
    FDT_DOC: TDateTime; /// Data da entrada ou da saída
    FCOD_NAT: String; /// Código da natureza da operação
    FVL_DOC: Currency; /// Valor total do documento fiscal
    FVL_TERC: Currency; /// Valor cobrado em nome de terceiros
    FVL_DESC: Currency; /// Valor total do desconto
    FVL_ACMO: Currency; /// Valor dos acréscimos
    FVL_OP_ISS: Currency; /// Valor das operações tributadas pelo ISS
    FVL_MERC: Currency; /// Valor da mercadoria fornecida/consumida
    FVL_DA: Currency; /// Valor de despesas acessórias indicadas no documento fiscal
    FVL_BC_ICMS: Currency; /// Valor da base de cálculo do ICMS
    FVL_ICMS: Currency; /// Valor do ICMS
    FCOD_INF_OBS: String; /// Código de referência a informação complementar

    FRegistroC705: TRegistroC705;
    FRegistroC750: TRegistroC750List;
    FRegistroC760: TRegistroC760List;
  public
    constructor Create(AOwner: TRegistroC001); virtual; /// Create
    destructor Destroy; override; /// Destroy;

    property IND_OPER: TACBrlTipoOperacao read FIND_OPER write FIND_OPER;
    property IND_EMIT: TACBrlEmitente read FIND_EMIT write FIND_EMIT;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property COD_SIT: TACBrlSituacaoDocto read FCOD_SIT write FCOD_SIT;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property COD_ASS: Integer read FCOD_ASS write FCOD_ASS;
    property NUM_DOC: String read FNUM_DOC write FNUM_DOC;
    property DT_EMIS: TDateTime read FDT_EMIS write FDT_EMIS;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property COD_NAT: String read FCOD_NAT write FCOD_NAT;
    property VL_DOC: Currency read FVL_DOC write FVL_DOC;
    property VL_TERC: Currency read FVL_TERC write FVL_TERC;
    property VL_DESC: Currency read FVL_DESC write FVL_DESC;
    property VL_ACMO: Currency read FVL_ACMO write FVL_ACMO;
    property VL_OP_ISS: Currency read FVL_OP_ISS write FVL_OP_ISS;
    property VL_MERC: Currency read FVL_MERC write FVL_MERC;
    property VL_DA: Currency read FVL_DA write FVL_DA;
    property VL_BC_ICMS: Currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: Currency read FVL_ICMS write FVL_ICMS;
    property COD_INF_OBS: String read FCOD_INF_OBS write FCOD_INF_OBS;

    property RegistroC705: TRegistroC705     read FRegistroC705 write FRegistroC705;
    property RegistroC750: TRegistroC750List read FRegistroC750 write FRegistroC750;
    property RegistroC760: TRegistroC760List read FRegistroC760 write FRegistroC760;
  end;

  /// Registro C700 - Lista

  { TRegistroC700List }

  TRegistroC700List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC700;
    procedure SetItem(Index: Integer; const Value: TRegistroC700);
  public
    function New(AOwner: TRegistroC001): TRegistroC700;
    property Items[Index: Integer]: TRegistroC700 read GetItem write SetItem;
  end;

  /// Registro C705 - COMPLEMENTO DO DOCUMENTO - PONTO DE CONSUMO (CÓDIGO 06)

  { TRegistroC705 }

  TRegistroC705 = class
  private
    FID_ASS: String; /// Identificação do consumidor ou assinante
    FCONTA: String; /// Número da conta de consumo
    FIND_LIG: TACBrTipoLigacao; /// Indicador do tipo de ligação
    FIND_KV: TACBrGrupoTensao; /// Indicador de grupo de tensão
    FDT_INI: TDateTime; /// Data em que iniciou o consumo de energia
    FDT_FIN: TDateTime; /// Data em que encerrou o consumo de energia
    FDEM: Double; /// Demanda, em kW
    FCONS: Double; /// Consumo total em kWh
    FHASH_DOC: Double; /// Código de autenticação do documento fiscal
  public
    constructor Create(AOwner: TRegistroC700); virtual; /// Create

    property ID_ASS: String read FID_ASS write FID_ASS;
    property CONTA: String read FCONTA write FCONTA;
    property IND_LIG: TACBrTipoLigacao read FIND_LIG write FIND_LIG;
    property IND_KV: TACBrGrupoTensao read FIND_KV write FIND_KV;
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDatetime read FDT_FIN write FDT_FIN;
    property DEM: Double read FDEM write FDEM;
    property CONS: Double read FCONS write FCONS;
    property HASH_DOC: Double read FHASH_DOC write FHASH_DOC;
  end;

  /// Registro C710 - COMPLEMENTO DO DOCUMENTO - TÍTULO DE CRÉDITO (CÓDIGO 28)

  { TRegistroC710 }

  TRegistroC710 = class
  private
    FID_ASS: String; /// Identificação do consumidor ou assinante
    FCONTA: String; /// Número da conta de consumo/fatura
    FMED: String; /// Número de identificação do medidor
    FDT_INI: TDateTime; /// Data inicial do período de fornecimento
    FDT_FIN: TDateTime; /// Data final do período de fornecimento
    FCONS: Double; /// Consumo, em metros cúbicos
    FVL_BC_ST: Currency; /// Valor da base de cálculo do ICMS substituição tributária
    FVL_ICMS_ST: Currency; /// Valor do ICMS da substituição tributáia
  public
    constructor Create(AOwner: TRegistroC700); virtual; /// Create

    property ID_ASS: String read FID_ASS write FID_ASS;
    property CONTA: String read FCONTA write FCONTA;
    property MED: String read FMED write FMED;
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
    property CONS: Double read FCONS write FCONS;
    property VL_BC_ST: Currency read FVL_BC_ST write FVL_BC_ST;
    property VL_ICMS_ST: Currency read FVL_ICMS_ST write FVL_ICMS_ST;
  end;

  /// Registro C715 - COMPLEMENTO DO DOCUMENTO - TÍTULO DE CRÉDITO (CÓDIGO 28)

  { TRegistroC715 }

  TRegistroC715 = class
  private
    FIND_TIT: TACBrTipoTitulo; /// Indicador de título de crédito
    FDESCR_TIT: String; /// Descrição complementar do título de crédito
    FNUM_TIT: String; /// Número ou código identificador do título de crédito
    FDT_TIT: TDateTime; /// Data de emissão do título de crédito
    FVL_TIT: Currency; /// Valor original do título de crédito
    FQTD_PARC: Integer; /// Quantidade de parcelas a pagar

    FRegistroC720: TRegistroC720List;
  public
    constructor Create(AOwner: TRegistroC700); virtual; /// Create

    property IND_TIT: TACBrTipoTitulo read FIND_TIT write FIND_TIT;
    property DESCR_TIT: String read FDESCR_TIT write FDESCR_TIT;
    property NUM_TIT: String read FNUM_TIT write FNUM_TIT;
    property DT_TIT: TDateTime read FDT_TIT write FDT_TIT;
    property VL_TIT: Currency read FVL_TIT write FVL_TIT;
    property QTD_PARC: Integer read FQTD_PARC write FQTD_PARC;

    property RegistroC720: TRegistroC720List read FRegistroC720 write FRegistroC720;
  end;

  /// Registro C720

  { TRegistroC720 }

  TRegistroC720 = class
  private
    FNUM_PARC: Integer; /// Número da parcela
    FDT_VCTO: TDateTime; /// Data de vencimento da parcela
    FVL_PARC: Currency; /// Valor da parcela
  public
    constructor Create(AOwner: TRegistroC715); virtual; /// Create

    property NUM_PARC: Integer read FNUM_PARC write FNUM_PARC;
    property DT_VCTO: TDateTime read FDT_VCTO write FDT_VCTO;
    property VL_PARC: Currency read FVL_PARC write FVL_PARC;
  end;

  /// Registro C720 - Lista

  { TRegistroC720List }

  TRegistroC720List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC720;
    procedure SetItem(Index: Integer; const Value: TRegistroC720);
  public
    function New(AOwner: TRegistroC715): TRegistroC720;
    property Items[Index: Integer]: TRegistroC720 read GetItem write SetItem;
  end;

  /// Registro C750 - ITENS DO DOCUMENTO

  { TRegistroC750 }

  TRegistroC750 = class
  private
    FNUM_ITEM: Integer; /// Número seqüencial do item no documento fiscal
    FCOD_CLASS: TACBrServicoPrestado; /// Código de classificação do item
    FCOD_ITEM: String; /// Código do item
    FUNID: String; /// Unidade do item
    FVL_UNIT: Currency; /// Valor unitário
    FQTD: Double; /// Quantidade do item
    FVL_DESC_I: Currency; /// Valor do desconto
    FVL_ACMO_I: Currency; /// Valor do acréscimo
    FVL_ITEM: Currency; /// Valor do item
    FCST: String; /// Código da Situação Tributária
    FCFOP: Integer; /// Código Fiscal de Operação e Prestação
    FVL_BC_ICMS_I: Currency; /// Valor da base de cálculo do ICMS
    FALIQ_ICMS: Currency; /// Alíquota do ICMS
    FVL_ICMS_I: Currency; /// Valor do ICMS

    FRegistroC755: TRegistroC755;
  public
    constructor Create(AOwner: TRegistroC700); virtual; /// Create

    property NUM_ITEM: Integer read FNUM_ITEM write FNUM_ITEM;
    property COD_CLASS: TACBrServicoPrestado read FCOD_CLASS write FCOD_CLASS;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property VL_UNIT: Currency read FVL_UNIT write FVL_UNIT;
    property QTD: Double read FQTD write FQTD;
    property VL_DESC_I: Currency read FVL_DESC_I write FVL_DESC_I;
    property VL_ACMO_I: Currency read FVL_ACMO_I write FVL_ACMO_I;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property CST: String read FCST write FCST;
    property CFOP: Integer read FCFOP write FCFOP;
    property VL_BC_ICMS_I: Currency read FVL_BC_ICMS_I write FVL_BC_ICMS_I;
    property ALIQ_ICMS: Currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_ICMS_I: Currency read FVL_ICMS_I write FVL_ICMS_I;

    property RegistroC755: TRegistroC755 read FRegistroC755 write FRegistroC755;
  end;

  /// Registro C750 - Lista

  { TRegistroC750List }

  TRegistroC750List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC750;
    procedure SetItem(Index: Integer; const Value: TRegistroC750);
  public
    function New(AOwner: TRegistroC700): TRegistroC750;
    property Items[Index: Integer]: TRegistroC750 read GetItem write SetItem;
  end;

  /// Registro C755 - COMPLEMENTO DO ITEM - ICMS-ST (CÓDIGO 28)

  { TRegistroC755 }

  TRegistroC755 = class
  private
    FVL_BC_ST_I: Currency; /// Valor da base de cálculo referente à substituição tributária
    FALIQ_ST: Currency; /// Alíquota do ICMS da substituição tributária na unidade da Federação de destino
    FVL_ICMS_ST_I: Currency; /// Valor do ICMS da substituição tributária
  public
    constructor Create(AOwner: TRegistroC750); virtual; /// Create

    property VL_BC_ST_I: Currency read FVL_BC_ST_I write FVL_BC_ST_I;
    property ALIQ_ST: Currency read FALIQ_ST write FALIQ_ST;
    property VL_ICMS_ST_I: Currency read FVL_ICMS_ST_I write FVL_ICMS_ST_I;
  end;

  /// Registro C760 - Registro Analítico do Documento

  { TRegistroC760 }

  TRegistroC760 = class
  private
    fCST: String; /// Código da Situação Tributária
    fCFOP: Integer; /// Código Fiscal de Operação e Prestação
    fVL_CONT_P: Currency; /// Parcela correspondente ao “Valor Contábil” referente à combinação CFOP, CST e alíquota do ICMS
    fVL_BC_ICMS_P: Currency; /// Parcela correspondente ao "Valor da base de cálculo do ICMS" referente à combinação CFOP, CST e alíquota do ICMS
    fALIQ_ICMS: Currency; /// Alíquota do ICMS
    fVL_ICMS_P: Currency; /// Parcela correspondente ao "Valor do ICMS" referente à combinação CFOP, CST e alíquota do ICMS
  public
    constructor Create(AOwner: TRegistroC700); virtual; /// Create

    property CST: String read fCST write fCST;
    property CFOP: Integer read fCFOP write fCFOP;
    property VL_CONT_P: Currency read fVL_CONT_P write fVL_CONT_P;
    property VL_BC_ICMS_P: Currency read fVL_BC_ICMS_P write fVL_BC_ICMS_P;
    property ALIQ_ICMS: Currency read fALIQ_ICMS write fALIQ_ICMS;
    property VL_ICMS_P: Currency read fALIQ_ICMS write fALIQ_ICMS;
  end;

  /// Registro C760 - Lista

  { TRegistroC760List }

  TRegistroC760List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC760;
    procedure SetItem(Index: Integer; const Value: TRegistroC760);
  public
    function New(AOwner: TRegistroC700): TRegistroC760;
    property Items[Index: Integer]: TRegistroC760 read GetItem write SetItem;
  end;

  /// Registro C770 - Documento Consolidado - Nota Fiscal/Conta de Energia
  /// Elétrica (código 06), Nota Fiscal/Conta de Fornecimento de
  /// Gás (código 28) e Nota Fiscal/Conta de Fornecimento d'Água (código 29)

  { TRegistroC770 }

  TRegistroC770 = class
  private
    FCOD_MUN: Integer; /// Código do município dos pontos de consumo
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FQTD_CANC: Integer; /// Quantidade de documentos cancelados
    FSER: String; /// Série dos documentos consolidados
    FSUB: String; /// Subsérie dos documentos consolidados
    FCOD_CONS: TACBrClasseConsumo; /// Código de classe de consumo
    FQTD_DOC: Double; /// Quantidade de documentos consolidados neste registro
    FDT_DOC: TDateTime; /// Data dos documentos consolidados
    FCOD_NAT: String; /// Código da natureza da operação
    FVL_DOC: Currency; /// Valor acumulado dos documentos consolidados
    FVL_OP_ISS: Currency; /// Valor total das operações tributadas pelo ISS
    FVL_TERC: Currency; /// Valor acumulado cobrado em nome de terceiros
    FVL_FORN: Currency; /// Valor acumulado do fornecimento
    FVL_DA: Currency; /// Valor acumulado das despesas acessórias
    FVL_BC_ICMS: Currency; /// Valor acumulado da base de cálculo do ICMS
    FVL_ICMS: Currency; /// Valor acumulado do ICMS
    FVL_BC_ST: Currency; // Valor da base de cálculo do ICMS substituição tributária
    FVL_ICMS_ST: Currency; /// Valor do ICMS da substituição tributária
    FCONS: Double; /// Consumo total, em kWh

    FRegistroC775: TRegistroC775List;
    FRegistroC780: TRegistroC780List;
  public
    constructor Create(AOwner: TRegistroC001); virtual; /// Create
    destructor Destroy; override; /// Destroy;

    property COD_MUN: Integer read FCOD_MUN write FCOD_MUN;
    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property QTD_CANC: Integer read FQTD_CANC write FQTD_CANC;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property COD_CONS: TACBrClasseConsumo read FCOD_CONS write FCOD_CONS;
    property QTD_DOC: Double read FQTD_DOC write FQTD_DOC;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property COD_NAT: String read FCOD_NAT write FCOD_NAT;
    property VL_DOC: Currency read FVL_DOC write FVL_DOC;
    property VL_OP_ISS: Currency read FVL_OP_ISS write FVL_OP_ISS;
    property VL_TERC: Currency read FVL_TERC write FVL_TERC;
    property VL_FORN: Currency read FVL_FORN write FVL_FORN;
    property VL_DA: Currency read FVL_DA write FVL_DA;
    property VL_BC_ICMS: Currency read FVL_BC_ICMS write FVL_BC_ICMS;
    property VL_ICMS: Currency read FVL_ICMS write FVL_ICMS;
    property VL_BC_ST: Currency read FVL_BC_ST write FVL_BC_ST;
    property VL_ICMS_ST: Currency read FVL_ICMS_ST write FVL_ICMS_ST;
    property CONS: Double read FCONS write FCONS;

    property RegistroC775: TRegistroC775List read FRegistroC775 write FRegistroC775;
    property RegistroC780: TRegistroC780List read FRegistroC780 write FRegistroC780;
  end;

  /// Registro C770 - Lista

  { TRegistroC770List }

  TRegistroC770List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC770;
    procedure SetItem(Index: Integer; const Value: TRegistroC770);
  public
    function New(AOwner: TRegistroC001): TRegistroC770;
    property Items[Index: Integer]: TRegistroC770 read GetItem write SetItem;
  end;

  /// Registro C775 - Itens dos Documentos

  { TRegistroC775 }

  TRegistroC775 = class
  private
    FCOD_CLASS: TACBrClasseConsumo; /// Código de classificação do item
    FCOD_ITEM: String; /// Código do item (campo 02 do Registro 0200)
    FUNID: String; /// Unidade do item
    FQTD: Double; /// Quantidade acumulada do item
    FVL_ITEM: Currency; /// Valor acumulado do item
    FCST: String; /// Código da Situação Tributária, conforme a tabela indicada no item
    FCFOP: Integer; /// Código Fiscal de Operação e Prestação preponderante, conforme a tabela indicada no item
    FALIQ_ICMS: Currency; /// Alíquota do ICMS
    FVL_BC_ICMS_I: Currency; /// Valor acumulado da base de cálculo do ICMS
    FVL_ICMS_I: Currency; /// Valor acumulado do ICMS
    FVL_BC_ST_I: Currency; /// Valor da base de cálculo referente à substituição tributária
    FALIQ_ST: Currency; /// Alíquota do ICMS da substituição tributária na unidade da Federação de destino
    FVL_ICMS_ST_I: Currency; /// Valor do ICMS da substituição tributária
  public
    constructor Create(AOwner: TRegistroC770); virtual; /// Create

    property COD_CLASS: TACBrClasseConsumo read FCOD_CLASS write FCOD_CLASS;
    property COD_ITEM: String read FCOD_ITEM write FCOD_ITEM;
    property UNID: String read FUNID write FUNID;
    property QTD: Double read FQTD write FQTD;
    property VL_ITEM: Currency read FVL_ITEM write FVL_ITEM;
    property CST: String read FCST write FCST;
    property CFOP: Integer read FCFOP write FCFOP;
    property ALIQ_ICMS: Currency read FALIQ_ICMS write FALIQ_ICMS;
    property VL_BC_ICMS_I: Currency read FVL_BC_ICMS_I write FVL_BC_ICMS_I;
    property VL_ICMS_I: Currency read FVL_ICMS_I write FVL_ICMS_I;
    property VL_BC_ST_I: Currency read FVL_BC_ST_I write FVL_BC_ST_I;
    property ALIQ_ST: Currency read FALIQ_ST write FALIQ_ST;
    property VL_ICMS_ST_I: Currency read FVL_ICMS_ST_I write FVL_ICMS_ST_I;
  end;

  /// Registro C775 - Lista

  { TRegistroC775List }

  TRegistroC775List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC775;
    procedure SetItem(Index: Integer; const Value: TRegistroC775);
  public
    function New(AOwner: TRegistroC770): TRegistroC775;
    property Items[Index: Integer]: TRegistroC775 read GetItem write SetItem;
  end;

  /// Registro C780 - Registro Analítico dos Documentos

  { TRegistroC780 }

  TRegistroC780 = class
  private
    fCST: String; /// Código da Situação Tributária
    fCFOP: Integer; /// Código Fiscal de Operação e Prestação
    fVL_CONT_P: Currency; /// Parcela correspondente ao “Valor Contábil” referente à combinação CFOP, CST e ICMS
    fVL_BC_ICMS_P: Currency; /// Parcela correspondente ao "Valor da base de cálculo do ICMS"
    fALIQ_ICMS: Currency; /// Alíquota do ICMS
    fVL_ICMS_P: Currency; /// Parcela correspondente ao "Valor do ICMS"
  public
    constructor Create(AOwner: TRegistroC770); virtual; /// Create

    property CST: String read fCST write fCST;
    property CFOP: Integer read fCFOP write fCFOP;
    property VL_CONT_P: Currency read fVL_CONT_P write fVL_CONT_P;
    property VL_BC_ICMS_P: Currency read fVL_BC_ICMS_P write fVL_BC_ICMS_P;
    property ALIQ_ICMS: Currency read fALIQ_ICMS write fALIQ_ICMS;
    property VL_ICMS_P: Currency read fVL_ICMS_P write fVL_ICMS_P;
  end;

  /// Registro C780 - Lista

  { TRegistroC780List }

  TRegistroC780List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroC780;
    procedure SetItem(Index: Integer; const Value: TRegistroC780);
  public
    function New(AOwner: TRegistroC770): TRegistroC780;
    property Items[Index: Integer]: TRegistroC780 read GetItem write SetItem;
  end;

  /// Registro C990 - Encerramento do Bloco C

  { TRegistroC990 }

  TRegistroC990 = class
  private
    fQTD_LIN_C: Integer; /// Quantidade total de linhas do Bloco C
  public
    property QTD_LIN_C: Integer read fQTD_LIN_C write fQTD_LIN_C;
  end;

implementation

{ TRegistroC605 }

constructor TRegistroC605.Create(AOwner: TRegistroC600);
begin

end;

{ TRegistroC555 }

constructor TRegistroC555.Create(AOwner: TRegistroC550);
begin

end;

{ TRegistroC001 }

constructor TRegistroC001.Create;
begin
  FRegistroC020 := TRegistroC020List.Create;
  FRegistroC550 := TRegistroC550List.Create;
  FRegistroC570 := TRegistroC570List.Create;
  FRegistroC600 := TRegistroC600List.Create;
  FRegistroC620 := TRegistroC620List.Create;
  FRegistroC700 := TRegistroC700List.Create;
  FRegistroC770 := TRegistroC770List.Create;
  //
  IND_MOV := imlSemDados;
end;

destructor TRegistroC001.Destroy;
begin
  FRegistroC020.Free;
  FRegistroC550.Free;
  FRegistroC570.Free;
  FRegistroC600.Free;
  FRegistroC620.Free;
  FRegistroC700.Free;
  FRegistroC770.Free;
  inherited;
end;

{ TRegistroC020 }

constructor TRegistroC020.Create(AOwner: TRegistroC001);
begin
  FRegistroC030 := TRegistroC030List.Create;
  FRegistroC040 := TRegistroC040.Create(Self);
  FRegistroC050 := TRegistroC050.Create(Self);
  FRegistroC060 := TRegistroC060.Create(Self);
  FRegistroC200 := TRegistroC200.Create(Self);
  FRegistroC250 := TRegistroC250.Create(Self);
  FRegistroC300 := TRegistroC300List.Create;
  FRegistroC500 := TRegistroC500List.Create;
end;

destructor TRegistroC020.Destroy;
begin
  FRegistroC030.Free;
  FRegistroC040.Free;
  FRegistroC050.Free;
  FRegistroC060.Free;
  FRegistroC200.Free;
  FRegistroC250.Free;
  FRegistroC300.Free;
  FRegistroC500.Free;
  inherited;
end;

{ TRegistroC020List }

function TRegistroC020List.GetItem(Index: Integer): TRegistroC020;
begin
  Result := TRegistroC020(Get(Index));
end;

function TRegistroC020List.New(AOwner: TRegistroC001): TRegistroC020;
begin
  Result := TRegistroC020.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC020List.SetItem(Index: Integer; const Value: TRegistroC020);
begin
  Put(Index, Value);
end;

{ TRegistroC030 }

constructor TRegistroC030.Create(AOwner: TRegistroC020);
begin
  FRegistroC035 := TRegistroC035List.Create;
end;

destructor TRegistroC030.Destroy;
begin
  FRegistroC035.Free;
  inherited;
end;

{ TRegistroC030List }

function TRegistroC030List.GetItem(Index: Integer): TRegistroC030;
begin
  Result := TRegistroC030(Get(Index));
end;

function TRegistroC030List.New(AOwner: TRegistroC020): TRegistroC030;
begin
  Result := TRegistroC030.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC030List.SetItem(Index: Integer; const Value: TRegistroC030);
begin
  Put(Index, Value);
end;

{ TRegistroC035 }

constructor TRegistroC035.Create(AOwner: TRegistroC030);
begin
end;

{ TRegistroC035List }

function TRegistroC035List.GetItem(Index: Integer): TRegistroC035;
begin
  Result := TRegistroC035(Get(Index));
end;

function TRegistroC035List.New(AOwner: TRegistroC030): TRegistroC035;
begin
  Result := TRegistroC035.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC035List.SetItem(Index: Integer; const Value: TRegistroC035);
begin
  Put(Index, Value);
end;

{ TRegistroC040 }

constructor TRegistroC040.Create(AOwner: TRegistroC020);
begin
end;

{ TRegistroC050 }

constructor TRegistroC050.Create(AOwner: TRegistroC020);
begin
end;

{ TRegistroC060 }

constructor TRegistroC060.Create(AOwner: TRegistroC020);
begin
end;

{ TRegistroC200 }

constructor TRegistroC200.Create(AOwner: TRegistroC020);
begin
end;

{ TRegistroC250 }

constructor TRegistroC250.Create(AOwner: TRegistroC020);
begin
  FRegistroC255 := TRegistroC255.Create(Self);
  FRegistroC260 := TRegistroC260List.Create;
end;

destructor TRegistroC250.Destroy;
begin
  FRegistroC255.Free;
  FRegistroC260.Free;
  inherited;
end;

{ TRegistroC255 }

constructor TRegistroC255.Create(AOwner: TRegistroC250);
begin
end;

{ TRegistroC260 }

constructor TRegistroC260.Create(AOwner: TRegistroC250);
begin
end;

{ TRegistroC260List }

function TRegistroC260List.GetItem(Index: Integer): TRegistroC260;
begin
  Result := TRegistroC260(Get(Index));
end;

function TRegistroC260List.New(AOwner: TRegistroC250): TRegistroC260;
begin
  Result := TRegistroC260.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC260List.SetItem(Index: Integer; const Value: TRegistroC260);
begin
  Put(Index, Value);
end;

{ TRegistroC265 }

constructor TRegistroC265.Create(AOwner: TRegistroC250);
begin
end;

{ TRegistroC300 }

constructor TRegistroC300.Create(AOwner: TRegistroC020);
begin
  FRegistroC320 := TRegistroC320.Create(Self);
  FRegistroC310 := TRegistroC310.Create(Self);
  FRegistroC325 := TRegistroC325.Create(Self);
  FRegistroC305 := TRegistroC305.Create(Self);
  FRegistroC315 := TRegistroC315.Create(Self);
end;

destructor TRegistroC300.Destroy;
begin
  FRegistroC320.Free;
  FRegistroC310.Free;
  FRegistroC325.Free;
  FRegistroC305.Free;
  FRegistroC315.Free;
  inherited;
end;

{ TRegistroC300List }

function TRegistroC300List.GetItem(Index: Integer): TRegistroC300;
begin
  Result := TRegistroC300(Get(Index));
end;

function TRegistroC300List.New(AOwner: TRegistroC020): TRegistroC300;
begin
  Result := TRegistroC300.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC300List.SetItem(Index: Integer; const Value: TRegistroC300);
begin
  Put(Index, Value);
end;

{ TRegistroC305 }

constructor TRegistroC305.Create(AOwner: TRegistroC300);
begin
end;

{ TRegistroC310 }

constructor TRegistroC310.Create(AOwner: TRegistroC300);
begin
end;

{ TRegistroC315 }

constructor TRegistroC315.Create(AOwner: TRegistroC300);
begin
end;

{ TRegistroC320 }

constructor TRegistroC320.Create(AOwner: TRegistroC300);
begin
end;

{ TRegistroC325 }

constructor TRegistroC325.Create(AOwner: TRegistroC300);
begin
end;

{ TRegistroC500 }

constructor TRegistroC500.Create(AOwner: TRegistroC020);
begin
end;

{ TRegistroC500List }

function TRegistroC500List.GetItem(Index: Integer): TRegistroC500;
begin
  Result := TRegistroC500(Get(Index));
end;

function TRegistroC500List.New(AOwner: TRegistroC020): TRegistroC500;
begin
  Result := TRegistroC500.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC500List.SetItem(Index: Integer; const Value: TRegistroC500);
begin
  Put(Index, Value);
end;

{ TRegistroC550 }

constructor TRegistroC550.Create(AOwner: TRegistroC001);
begin
  FRegistroC555 := TRegistroC555List.Create;
end;

destructor TRegistroC550.Destroy;
begin
  FRegistroC555.Free;
  inherited;
end;

{ TRegistroC550List }

function TRegistroC550List.GetItem(Index: Integer): TRegistroC550;
begin
  Result := TRegistroC550(Get(Index));
end;

function TRegistroC550List.New(AOwner: TRegistroC001): TRegistroC550;
begin
  Result := TRegistroC550.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC550List.SetItem(Index: Integer; const Value: TRegistroC550);
begin
  Put(Index, Value);
end;


{ TRegistroC560 }

constructor TRegistroC560.Create(AOwner: TRegistroC550);
begin
end;

{ TRegistroC555List }

function TRegistroC555List.GetItem(Index: Integer): TRegistroC555;
begin
  Result := TRegistroC555(Get(Index));
end;

procedure TRegistroC555List.SetItem(Index: Integer; const Value: TRegistroC555);
begin
   Put(Index, Value);
end;

function TRegistroC555List.New(AOwner: TRegistroC550): TRegistroC555;
begin
  Result := TRegistroC555.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC560List.SetItem(Index: Integer; const Value: TRegistroC560);
begin
  Put(Index, Value);
end;

{ TRegistroC560 }

{constructor TRegistroC560.Create(AOwner: TRegistroC550);
begin
end;   }

{ TRegistroC560List }

function TRegistroC560List.GetItem(Index: Integer): TRegistroC560;
begin
  Result := TRegistroC560(Get(Index));
end;

function TRegistroC560List.New(AOwner: TRegistroC550): TRegistroC560;
begin
  Result := TRegistroC560.Create(AOwner);
  Add(Result);
end;

{procedure TRegistroC560List.SetItem(Index: Integer; const Value: TRegistroC560);
begin
  Put(Index, Value);
end;}

{ TRegistroC570 }

constructor TRegistroC570.Create(AOwner: TRegistroC001);
begin
  FRegistroC575 := TRegistroC575List.Create;
end;

destructor TRegistroC570.Destroy;
begin
  FRegistroC575.Free;
  inherited;
end;

{ TRegistroC570List }

function TRegistroC570List.GetItem(Index: Integer): TRegistroC570;
begin
  Result := TRegistroC570(Get(Index));
end;

function TRegistroC570List.New(AOwner: TRegistroC001): TRegistroC570;
begin
  Result := TRegistroC570.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC570List.SetItem(Index: Integer; const Value: TRegistroC570);
begin
  Put(Index, Value);
end;

{ TRegistroC575 }

constructor TRegistroC575.Create(AOwner: TRegistroC570);
begin
end;

{ TRegistroC575List }

function TRegistroC575List.GetItem(Index: Integer): TRegistroC575;
begin
  Result := TRegistroC575(Get(Index));
end;

function TRegistroC575List.New(AOwner: TRegistroC570): TRegistroC575;
begin
  Result := TRegistroC575.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC575List.SetItem(Index: Integer; const Value: TRegistroC575);
begin
  Put(Index, Value);
end;

{ TRegistroC580 }

constructor TRegistroC580.Create(AOwner: TRegistroC001);
begin
end;

{ TRegistroC580List }

function TRegistroC580List.GetItem(Index: Integer): TRegistroC580;
begin
  Result := TRegistroC580(Get(Index));
end;

function TRegistroC580List.New(AOwner: TRegistroC001): TRegistroC580;
begin
  Result := TRegistroC580.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC580List.SetItem(Index: Integer; const Value: TRegistroC580);
begin
  Put(Index, Value);
end;

{ TRegistroC600 }

constructor TRegistroC600.Create(AOwner: TRegistroC001);
begin
  FRegistroC605 := TRegistroC605List.Create;
  FRegistroC610 := TRegistroC610List.Create;
end;

destructor TRegistroC600.Destroy;
begin
  FRegistroC605.Free;
  FRegistroC610.Free;
  inherited;
end;

{ TRegistroC600List }

function TRegistroC600List.GetItem(Index: Integer): TRegistroC600;
begin
  Result := TRegistroC600(Get(Index));
end;

function TRegistroC600List.New(AOwner: TRegistroC001): TRegistroC600;
begin
  Result := TRegistroC600.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC600List.SetItem(Index: Integer; const Value: TRegistroC600);
begin
  Put(Index, Value);
end;

{ TRegistroC605List }

function TRegistroC605List.GetItem(Index: Integer): TRegistroC605;
begin
  Result := TRegistroC605(Get(Index));
end;

function TRegistroC605List.New(AOwner: TRegistroC600): TRegistroC605;
begin
  Result := TRegistroC605.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC605List.SetItem(Index: Integer; const Value: TRegistroC605);
begin
  Put(Index, Value);
end;

{ TRegistroC610 }

constructor TRegistroC610.Create(AOwner: TRegistroC600);
begin
  FRegistroC615 :=  TRegistroC615.Create(Self);
end;

destructor TRegistroC610.Destroy;
begin
  FRegistroC615.Free;
  inherited;
end;

{ TRegistroC610List }

function TRegistroC610List.GetItem(Index: Integer): TRegistroC610;
begin
  Result := TRegistroC610(Get(Index));
end;

function TRegistroC610List.New(AOwner: TRegistroC600): TRegistroC610;
begin
  Result := TRegistroC610.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC610List.SetItem(Index: Integer; const Value: TRegistroC610);
begin
  Put(Index, Value);
end;

{ TRegistroC615 }

constructor TRegistroC615.Create(AOwner: TRegistroC610);
begin
end;

{ TRegistroC620 }

constructor TRegistroC620.Create(AOwner: TRegistroC001);
begin
  FRegistroC625 := TRegistroC625List.Create;
end;

destructor TRegistroC620.Destroy;
begin
  FRegistroC625.Free;
  inherited;
end;

{ TRegistroC620List }

function TRegistroC620List.GetItem(Index: Integer): TRegistroC620;
begin
  Result := TRegistroC620(Get(Index));
end;

function TRegistroC620List.New(AOwner: TRegistroC001): TRegistroC620;
begin
  Result := TRegistroC620.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC620List.SetItem(Index: Integer; const Value: TRegistroC620);
begin
  Put(Index, Value)
end;

{ TRegistroC625 }

constructor TRegistroC625.Create(AOwner: TRegistroC620);
begin
end;

{ TRegistroC625List }

function TRegistroC625List.GetItem(Index: Integer): TRegistroC625;
begin
  Result := TRegistroC625(Get(Index));
end;

function TRegistroC625List.New(AOwner: TRegistroC620): TRegistroC625;
begin
  Result := TRegistroC625.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC625List.SetItem(Index: Integer; const Value: TRegistroC625);
begin
  Put(Index, Value);
end;

{ TRegistroC640 }

constructor TRegistroC640.Create(AOwner: TRegistroC001);
begin
end;

{ TRegistroC640List }

function TRegistroC640List.GetItem(Index: Integer): TRegistroC640;
begin
  Result := TRegistroC640(Get(Index));
end;

function TRegistroC640List.New(AOwner: TRegistroC001): TRegistroC640;
begin
  Result := TRegistroC640.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC640List.SetItem(Index: Integer; const Value: TRegistroC640);
begin
  Put(Index, Value);
end;

{ TRegistroC700 }

constructor TRegistroC700.Create(AOwner: TRegistroC001);
begin
  FRegistroC705 := TRegistroC705.Create(Self);
  FRegistroC750 := TRegistroC750List.Create;
  FRegistroC760 := TRegistroC760List.Create;
end;

destructor TRegistroC700.Destroy;
begin
  FRegistroC705.Free;
  FRegistroC750.Free;
  FRegistroC760.Free;
  inherited;
end;

{ TRegistroC700List }

function TRegistroC700List.GetItem(Index: Integer): TRegistroC700;
begin
  Result := TRegistroC700(Get(Index));
end;

function TRegistroC700List.New(AOwner: TRegistroC001): TRegistroC700;
begin
  Result := TRegistroC700.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC700List.SetItem(Index: Integer; const Value: TRegistroC700);
begin
  Put(Index, Value);
end;

{ TRegistroC705 }

constructor TRegistroC705.Create(AOwner: TRegistroC700);
begin
end;

{ TRegistroC710 }

constructor TRegistroC710.Create(AOwner: TRegistroC700);
begin
end;

{ TRegistroC715 }

constructor TRegistroC715.Create(AOwner: TRegistroC700);
begin
end;

{ TRegistroC720 }

constructor TRegistroC720.Create(AOwner: TRegistroC715);
begin
end;

{ TRegistroC720List }

function TRegistroC720List.GetItem(Index: Integer): TRegistroC720;
begin
  Result := TRegistroC720(Get(Index));
end;

function TRegistroC720List.New(AOwner: TRegistroC715): TRegistroC720;
begin
  Result := TRegistroC720.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC720List.SetItem(Index: Integer; const Value: TRegistroC720);
begin
  Put(Index, Value);
end;

{ TRegistroC750 }

constructor TRegistroC750.Create(AOwner: TRegistroC700);
begin
end;

{ TRegistroC750List }

function TRegistroC750List.GetItem(Index: Integer): TRegistroC750;
begin
  Result := TRegistroC750(Get(Index));
end;

function TRegistroC750List.New(AOwner: TRegistroC700): TRegistroC750;
begin
  Result := TRegistroC750.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC750List.SetItem(Index: Integer; const Value: TRegistroC750);
begin
  Put(Index, Value);
end;

{ TRegistroC755 }

constructor TRegistroC755.Create(AOwner: TRegistroC750);
begin
end;

{ TRegistroC760 }

constructor TRegistroC760.Create(AOwner: TRegistroC700);
begin
end;

{ TRegistroC760List }

function TRegistroC760List.GetItem(Index: Integer): TRegistroC760;
begin
  Result := TRegistroC760(Get(Index));
end;

function TRegistroC760List.New(AOwner: TRegistroC700): TRegistroC760;
begin
  Result := TRegistroC760.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC760List.SetItem(Index: Integer; const Value: TRegistroC760);
begin
  Put(Index, Value);
end;

{ TRegistroC770 }

constructor TRegistroC770.Create(AOwner: TRegistroC001);
begin
  FRegistroC775 := TRegistroC775List.Create;
  FRegistroC780 := TRegistroC780List.Create;
end;

destructor TRegistroC770.Destroy;
begin
  FRegistroC775.Free;
  FRegistroC780.Free;
  inherited;
end;

{ TRegistroC770List }

function TRegistroC770List.GetItem(Index: Integer): TRegistroC770;
begin
  Result := TRegistroC770(Get(Index));
end;

function TRegistroC770List.New(AOwner: TRegistroC001): TRegistroC770;
begin
  Result := TRegistroC770.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC770List.SetItem(Index: Integer; const Value: TRegistroC770);
begin
  Put(Index, Value);
end;

{ TRegistroC775 }

constructor TRegistroC775.Create(AOwner: TRegistroC770);
begin
end;

{ TRegistroC775List }

function TRegistroC775List.GetItem(Index: Integer): TRegistroC775;
begin
  Result := TRegistroC775(Get(Index));
end;

function TRegistroC775List.New(AOwner: TRegistroC770): TRegistroC775;
begin
  Result := TRegistroC775.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC775List.SetItem(Index: Integer; const Value: TRegistroC775);
begin
  Put(Index, Value);
end;

{ TRegistroC780 }

constructor TRegistroC780.Create(AOwner: TRegistroC770);
begin
end;

{ TRegistroC780List }

function TRegistroC780List.GetItem(Index: Integer): TRegistroC780;
begin
  Result := TRegistroC780(Get(Index));
end;

function TRegistroC780List.New(AOwner: TRegistroC770): TRegistroC780;
begin
  Result := TRegistroC780.Create(AOwner);
  Add(Result);
end;

procedure TRegistroC780List.SetItem(Index: Integer; const Value: TRegistroC780);
begin
  Put(Index, Value);
end;

end.
