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

unit ACBrLFDBloco_B;

interface

uses
  SysUtils, Classes, DateUtils, ACBrLFDBlocos;

type

  TRegistroB020List = class;
  TRegistroB025List = class;
  TRegistroB030List = class;
  TRegistroB035List = class;
  TRegistroB040List = class;
  TRegistroB045List = class;
  TRegistroB050List = class;
  TRegistroB055List = class;
  TRegistroB350List = class;
  TRegistroB400List = class;
  TRegistroB410List = class;
  TRegistroB420List = class;
  TRegistroB430List = class;
  TRegistroB440List = class;
  TRegistroB450List = class;
  TRegistroB460List = class;
  TRegistroB465List = class;
  TRegistroB470List = class;
  TRegistroB475List = class;
  TRegistroB480List = class;
  TRegistroB490List = class;
  TRegistroB500List = class;
  TRegistroB510List = class;
  TRegistroB600List = class;
  TRegistroB700List = class;

  /// Registro B001 - Abertura do Bloco B

  { TRegistroB001 }

  TRegistroB001 = class(TOpenBlocos)
  private
    fCOD_MUN: Integer;  /// Código do município do domicílio fiscal do contribuinte

    FRegistroB020: TRegistroB020List;
    FRegistroB030: TRegistroB030List;
    FRegistroB040: TRegistroB040List;
    FRegistroB050: TRegistroB050List;
    FRegistroB350: TRegistroB350List;
    FRegistroB400: TRegistroB400List;
    FRegistroB700: TRegistroB700List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_MUN: Integer read fCOD_MUN write fCOD_MUN;

    property RegistroB020: TRegistroB020List read FRegistroB020 write FRegistroB020;
    property RegistroB030: TRegistroB030List read FRegistroB030 write FRegistroB030;
    property RegistroB040: TRegistroB040List read FRegistroB040 write FRegistroB040;
    property RegistroB050: TRegistroB050List read FRegistroB050 write FRegistroB050;
    property RegistroB350: TRegistroB350List read FRegistroB350 write FRegistroB350;
    property RegistroB400: TRegistroB400List read FRegistroB400 write FRegistroB400;
    property RegistroB700: TRegistroB700List read FRegistroB700 write FRegistroB700;
  end;

  /// Registro B020 - Nota Fiscal de Serviços

  { TRegistroB020 }

  TRegistroB020 = class
  private
    FIND_OPER: TACBrlTipoOperacao; /// Indicador do tipo de operação
    FIND_EMIT: TACBrlEmitente; /// Indicador do emitente do documento fiscal
    FCOD_PART: String; /// Código do participante
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FCOD_SIT: TACBrlSituacaoDocto; /// Código do modelo do documento fiscal
    FSER: String; /// Série do documento fiscal
    FSUB: String; /// Subsérie do documento fiscal
    FNUM_DOC: String; /// Número do documento fiscal
    FDT_DOC: TDateTime; /// Data de emissão do documento fiscal ou data de pagamento no caso de substituição tributária
    FCFPS: String; /// Código Fiscal de Prestações de Serviços
    FNUM_LCTO: String; /// Número ou código de identificação do lançamento contábil
    FCOD_MUN_SERV: Integer; /// Código do município onde o serviço foi prestado
    FVL_CONT: Currency; /// Valor contábil (valor total do documento)
    FVL_MAT_TERC: Currency; /// Valor do material fornecido por terceiros na prestação do serviço
    FVL_SUB: Currency; /// Valor da subempreitada
    FVL_ISNT_ISS: Currency; /// Valor das operações isentas ou não-tributadas pelo ISS
    FVL_DED_BC: Currency; /// Valor da dedução da base de cálculo
    FVL_BC_ISS: Currency; /// Valor da base de cálculo do ISS
    FVL_BC_ISS_RT: CUrrency; /// Valor da base de cálculo de retenção do ISS
    FVL_ISS_RT: Currency; /// Valor do ISS retido pelo tomador
    FVL_ISS: Currency; /// Valor do ISS destacado
    FCOD_INF_OBS: String; /// Código de referência à observação

    FRegistroB025: TRegistroB025List;
  public
    constructor Create(AOwner: TRegistroB001); virtual; /// Create
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
    property CFPS: String read FCFPS write FCFPS;
    property NUM_LCTO: String read FNUM_LCTO write FNUM_LCTO;
    property COD_MUN_SERV: Integer read FCOD_MUN_SERV write FCOD_MUN_SERV;
    property VL_CONT: Currency read FVL_CONT write FVL_CONT;
    property VL_MAT_TERC: Currency read FVL_MAT_TERC write FVL_MAT_TERC;
    property VL_SUB: Currency read FVL_SUB write FVL_SUB;
    property VL_ISNT_ISS: Currency read FVL_ISNT_ISS write FVL_ISNT_ISS;
    property VL_DED_BC: Currency read FVL_DED_BC write FVL_DED_BC;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_BC_ISS_RT: CUrrency read FVL_BC_ISS_RT write FVL_BC_ISS_RT;
    property VL_ISS_RT: Currency read FVL_ISS_RT write FVL_ISS_RT;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property COD_INF_OBS: String read FCOD_INF_OBS write FCOD_INF_OBS;

    property RegistroB025: TRegistroB025List read FRegistroB025 write FRegistroB025;
  end;

  /// Registro B020 - Lista

  { TRegistroB020List }

  TRegistroB020List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB020;
    procedure SetItem(Index: Integer; const Value: TRegistroB020);
  public
    function New(AOwner: TRegistroB001): TRegistroB020;
    property Items[Index: Integer]: TRegistroB020 read GetItem write SetItem;
  end;

  /// Registro B025 - Detalhe - valores parciais

  { TRegistroB025 }

  TRegistroB025 = class
  private
    FVL_CONT_P: Currency; /// Parcela correspondente ao "Valor Contábil" referente à alíquota do ISS
    FVL_BC_ISS_P: Currency; /// Parcela correspondente ao "Valor da base de cálculo do ISS" referente à alíquota do ISS
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS_P: Currency; /// Parcela correspondente ao "Valor do ISS" referente à alíquota do ISS
    FVL_ISNT_ISS_P: Currency; /// Parcela correspondente ao "Valor das operações isentas ou não-tributadas pelo ISS" referente à alíquota do ISS
  public
    constructor Create(AOwner: TRegistroB020); virtual; /// Create

    property VL_CONT_P: Currency read FVL_CONT_P write FVL_CONT_P;
    property VL_BC_ISS_P: Currency read FVL_BC_ISS_P write FVL_BC_ISS_P;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS_P: Currency read FVL_ISS_P write FVL_ISS_P;
    property VL_ISNT_ISS_P: Currency read FVL_ISNT_ISS_P write FVL_ISNT_ISS_P;
  end;

  /// Registro B025 - Lista

  { TRegistroB025List }

  TRegistroB025List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB025;
    procedure SetItem(Index: Integer; const Value: TRegistroB025);
  public
    function New(AOwner: TRegistroB020): TRegistroB025;
    property Items[Index: Integer]: TRegistroB025 read GetItem write SetItem;
  end;

  /// Registro B030 - Lançamento - Nota Fiscal de Serviços Simplificada

  { TRegistroB030 }

  TRegistroB030 = class
  private
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FSER: String; /// Série do documento fiscal
    FSUB: String; /// Subsérie do documento fiscal
    FNUM_DOC_INI: Integer; /// Número do primeiro documento fiscal emitido no dia
    FNUM_DOC_FIN: Integer; /// Número do último documento fiscal emitido no dia
    FDT_DOC: TDateTime; /// Data da emissão dos documentos fiscais
    FCFPS: String; /// Código Fiscal de Prestações de Serviços
    FNUM_LCTO: Integer; /// Número ou código de identificação do lançamento contábil
    FQTD_CANC: Double; /// Quantidade de documentos cancelados
    FVL_CONT: Currency; /// Valor contábil (valor total acumulado dos documentos)
    FVL_ISNT_ISS: Currency; /// Valor acumulado das operações isentas ou não-tributadas pelo ISS
    FVL_BC_ISS: Currency; /// Valor acumulado da base de cálculo do ISS
    FVL_ISS: Currency; /// Valor acumulado do ISS destacado
    FCOD_INF_OBS: String; /// Código de referência à observação

    FRegistroB035: TRegistroB035List;
  public
    constructor Create(AOwner: TRegistroB001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property SER: String read FSER write FSER;
    property SUB: String read FSUB write FSUB;
    property NUM_DOC_INI: Integer read FNUM_DOC_INI write FNUM_DOC_INI;
    property NUM_DOC_FIN: Integer read FNUM_DOC_FIN write FNUM_DOC_FIN;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property CFPS: String read FCFPS write FCFPS;
    property NUM_LCTO: Integer read FNUM_LCTO write FNUM_LCTO;
    property QTD_CANC: Double read FQTD_CANC write FQTD_CANC;
    property VL_CONT: Currency read FVL_CONT write FVL_CONT;
    property VL_ISNT_ISS: Currency read FVL_ISNT_ISS write FVL_ISNT_ISS;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property COD_INF_OBS: String read FCOD_INF_OBS write FCOD_INF_OBS;

    property RegistroB035: TRegistroB035List read FRegistroB035 write FRegistroB035;
  end;

  /// Registro B030 - Lista

  { TRegistroB030List }

  TRegistroB030List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB030;
    procedure SetItem(Index: Integer; const Value: TRegistroB030);
  public
    function New(AOwner: TRegistroB001): TRegistroB030;
    property Items[Index: Integer]: TRegistroB030 read GetItem write SetItem;
  end;

  /// Registro B035 - Detalhe - valores parciais

  { TRegistroB035 }

  TRegistroB035 = class
  private
    FVL_CONT_P: Currency; /// Parcela correspondente ao “Valor Contábil” referente à alíquota do ISS
    FVL_ISNT_ISS_P: Currency; /// Parcela correspondente ao “Valor das operações isentas ou não-tributadas pelo ISS” referente à alíquota do ISS
    FVL_BC_ISS_P: Currency; /// Parcela correspondente ao “Valor da base de cálculo do ISS” referente à alíquota do ISS
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS_P: Currency; /// Parcela correspondente ao “Valor do ISS” referente à alíquota do ISS
  public
    constructor Create(AOwner: TRegistroB030); virtual; /// Create

    property VL_CONT_P: Currency read FVL_CONT_P write FVL_CONT_P;
    property VL_ISNT_ISS_P: Currency read FVL_ISNT_ISS_P write FVL_ISNT_ISS_P;
    property VL_BC_ISS_P: Currency read FVL_BC_ISS_P write FVL_BC_ISS_P;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS_P: Currency read FVL_ISS_P write FVL_ISS_P;
  end;

  /// Registro B035 - Lista

  { TRegistroB035List }

  TRegistroB035List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB035;
    procedure SetItem(Index: Integer; const Value: TRegistroB035);
  public
    function New(AOwner: TRegistroB030): TRegistroB035;
    property Items[Index: Integer]: TRegistroB035 read GetItem write SetItem;
  end;

  /// Registro B040 - Lançamento - Redução Z/ISS

  { TRegistroB040 }

  TRegistroB040 = class
  private
    FCOD_MOD: String; /// Código do modelo do documento fiscal
    FECF_CX: Integer; /// Número do caixa atribuído ao ECF
    FECF_FAB: String; /// Número de série de fabricação do ECF
    FCRO: Integer; /// Posição do Contador de Reinício de Operação
    FCRZ: Integer; /// Posição do Contador de Redução Z
    FNUM_DOC_INI: Integer; /// Número do primeiro documento fiscal emitido no dia
    FNUM_DOC_FIN: Integer; /// Número do último documento fiscal emitido no dia
    FDT_DOC: TDateTime; /// Data da Redução Z
    FGT_INI: Currency; /// Valor do Grande Total inicial
    FGT_FIN: Currency; /// Valor do Grande Total final
    FVL_BRT: Currency; /// Valor da venda bruta
    FVL_CANC_ISS: Currency; /// Valor dos cancelamentos referentes ao ISS
    FVL_CANC_ICMS: Currency; /// Valor dos cancelamentos referentes ao ICMS
    FVL_CANC: Currency; /// Valor dos cancelamentos registrados
    FVL_DESC_ISS: Currency; /// Valor dos descontos registrados nas operações sujeitas ao ISS
    FVL_DESC_ICMS: Currency; /// Valor dos descontos registrados nas operações sujeitas ao ICMS
    FVL_DESC: Currency; /// Valor dos descontos registrados
    FVL_ACMO_ISS: Currency; /// Valor dos acréscimos referentes ao ISS
    FVL_ACMO_ICMS: Currency; /// Valor dos acréscimos referentes ao ICMS
    FVL_ACMO: Currency; /// Valor dos acréscimos registrados
    FVL_BC_ISS: Currency; /// Valor da base de cálculo do ISS
    FVL_ISS: Currency; /// Valor do ISS
    FVL_ISEN_ISS: Currency; /// Valor das prestações de serviço isentas do ISS
    FVL_NT_ISS: Currency; /// Valor das prestações de serviço sob não-incidência ou não-tributadas pelo ISS
    FVL_RT_ISS: Currency; /// Valor das prestações de serviço com ISS retido pelo tomador
    FVL_LIQ: Currency; /// Valor da venda líquida
    FCOD_INF_OBS: String; /// Código de referência à observação

    FRegistroB045: TRegistroB045List;
  public
    constructor Create(AOwner: TRegistroB001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_MOD: String read FCOD_MOD write FCOD_MOD;
    property ECF_CX: Integer read FECF_CX write FECF_CX;
    property ECF_FAB: String read FECF_FAB write FECF_FAB;
    property CRO: Integer read FCRO write FCRO;
    property CRZ: Integer read FCRZ write FCRZ;
    property NUM_DOC_INI: Integer read FNUM_DOC_INI write FNUM_DOC_INI;
    property NUM_DOC_FIN: Integer read FNUM_DOC_FIN write FNUM_DOC_FIN;
    property DT_DOC: TDateTime read FDT_DOC write FDT_DOC;
    property GT_INI: Currency read FGT_INI write FGT_INI;
    property GT_FIN: Currency read FGT_FIN write FGT_FIN;
    property VL_BRT: Currency read FVL_BRT write FVL_BRT;
    property VL_CANC_ISS: Currency read FVL_CANC_ISS write FVL_CANC_ISS;
    property VL_CANC_ICMS: Currency read FVL_CANC_ICMS write FVL_CANC_ICMS;
    property VL_CANC: Currency read FVL_CANC write FVL_CANC;
    property VL_DESC_ISS: Currency read FVL_DESC_ISS write FVL_DESC_ISS;
    property VL_DESC_ICMS: Currency read FVL_DESC_ICMS write FVL_DESC_ICMS;
    property VL_DESC: Currency read FVL_DESC write FVL_DESC;
    property VL_ACMO_ISS: Currency read FVL_ACMO_ISS write FVL_ACMO_ISS;
    property VL_ACMO_ICMS: Currency read FVL_ACMO_ICMS write FVL_ACMO_ICMS;
    property VL_ACMO: Currency read FVL_ACMO write FVL_ACMO;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property VL_ISEN_ISS: Currency read FVL_ISEN_ISS write FVL_ISEN_ISS;
    property VL_NT_ISS: Currency read FVL_NT_ISS write FVL_NT_ISS;
    property VL_RT_ISS: Currency read FVL_RT_ISS write FVL_RT_ISS;
    property VL_LIQ: Currency read FVL_LIQ write FVL_LIQ;
    property COD_INF_OBS: String read FCOD_INF_OBS write FCOD_INF_OBS;

    property RegistroB045: TRegistroB045List read FRegistroB045 write FRegistroB045;
  end;

  /// Registro B040 - Lista

  { TRegistroB040List }

  TRegistroB040List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB040;
    procedure SetItem(Index: Integer; const Value: TRegistroB040);
  public
    function New(AOwner: TRegistroB001): TRegistroB040;
    property Items[Index: Integer]: TRegistroB040 read GetItem write SetItem;
  end;

  /// Registro B045 - Detalhe - valores parciais

  { TRegistroB045 }

  TRegistroB045 = class
  private
    FVL_CANC_ISS_P: Currency; /// Valor dos cancelamentos registrados nas operações sujeitas ao ISS
    FVL_CANC_ICMS_P: Currency; /// Valor dos cancelamentos registrados nas operações sujeitas ao ICMS
    FVL_DESC_ISS_P: Currency; /// Valor dos descontos registrados nas operações sujeitas ao ISS
    FVL_DESC_ICMS_P: Currency; /// Valor dos descontos registrados nas operações sujeitas ao ICMS
    FVL_ACMO_ISS_P: Currency; /// Valor dos acréscimos registrados nas operações sujeitas ao ISS
    FVL_ACMO_ICMS_P: Currency; /// Valor dos acréscimos registrados nas operações sujeitas ao ICMS
    FVL_BC_ISS_P: Currency; /// Valor da base de cálculo do ISS acumulada relativa à alíquota
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS_P: Currency; /// Valor do ISS
    FVL_ISEN_ISS_P: Currency; /// Valor das prestações de serviço isentas do ISS
    FVL_NT_ISS_P: Currency; /// Valor das prestações de serviço sob não-incidência ou não-tributadas pelo ISS
    FVL_ISS_RT_P: Currency; /// Valor das prestações de serviço com ISS retido pelo tomador
  public
    constructor Create(AOwner: TRegistroB040); virtual; /// Create

    property VL_CANC_ISS_P: Currency read FVL_CANC_ISS_P write FVL_CANC_ISS_P;
    property VL_CANC_ICMS_P: Currency read FVL_CANC_ICMS_P write FVL_CANC_ICMS_P;
    property VL_DESC_ISS_P: Currency read FVL_DESC_ISS_P write FVL_DESC_ISS_P;
    property VL_DESC_ICMS_P: Currency read FVL_DESC_ICMS_P write FVL_DESC_ICMS_P;
    property VL_ACMO_ISS_P: Currency read FVL_ACMO_ISS_P write FVL_ACMO_ISS_P;
    property VL_ACMO_ICMS_P: Currency read FVL_ACMO_ICMS_P write FVL_ACMO_ICMS_P;
    property VL_BC_ISS_P: Currency read FVL_BC_ISS_P write FVL_BC_ISS_P;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS_P: Currency read FVL_ISS_P write FVL_ISS_P;
    property VL_ISEN_ISS_P: Currency read FVL_ISEN_ISS_P write FVL_ISEN_ISS_P;
    property VL_NT_ISS_P: Currency read FVL_NT_ISS_P write FVL_NT_ISS_P;
    property VL_ISS_RT_P: Currency read FVL_ISS_RT_P write FVL_ISS_RT_P;
  end;

  /// Registro B045 - Lista

  { TRegistroB045List }

  TRegistroB045List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB045;
    procedure SetItem(Index: Integer; const Value: TRegistroB045);
  public
    function New(AOwner: TRegistroB040): TRegistroB045;
    property Items[Index: Integer]: TRegistroB045 read GetItem write SetItem;
  end;

  /// Registro B050 - Lançamento - Mapa-Resumo de ECF/ISS

  { TRegistroB050 }

  TRegistroB050 = class
  private
    FIND_MR: TACBrTipoTotalECF; /// Indicador do tipo de totalização
    FNUM_MR_ECF: Integer; /// Número do Mapa-Resumo ECF utilizado no dia
    FDT_MR_ECF: TDateTime; /// Data do movimento do Mapa-Resumo ECF
    FCFPS: Integer; /// Código Fiscal de Prestações de Serviços preponderante, conforme a tabela indicada no item
    FNUM_LCTO: Integer; /// Número ou código de identificação do lançamento contábil
    FVL_BRT: Currency; /// Valor da venda bruta
    FVL_CANC_ISS: Currency; /// Valor dos cancelamentos referentes ao ISS
    FVL_CANC_ICMS: Currency; /// Valor dos cancelamentos referentes ao ICMS
    FVL_CANC: Currency; /// Valor dos cancelamentos registrados
    FVL_DESC_ISS: Currency; /// Valor dos descontos registrados nas operações sujeitas ao ISS
    FVL_DESC_ICMS: Currency; /// Valor dos descontos registrados nas operações sujeitas ao ICMS
    FVL_DESC: Currency; /// Valor dos descontos registrados
    FVL_ACMO_ISS: Currency; /// Valor dos acréscimos referentes ao ISS
    FVL_ACMO_ICMS: Currency; /// Valor dos acréscimos referentes ao ICMS
    FVL_ACMO: Currency; /// Valor dos acréscimos registrados
    FVL_BC_ISS: Currency; /// Valor da base de cálculo do ISS
    FVL_ISS: Currency; /// Valor do ISS
    FVL_ISEN_ISS: Currency; /// Valor das prestações de serviço isentas do ISS
    FVL_NT_ISS: Currency; /// Valor das prestações de serviço sob não-incidência ou não-tributadas pelo ISS
    FVL_RT_ISS: Currency; /// Valor das prestações de serviço com ISS retido pelo tomador
    FVL_CONT: Currency; /// Valor da venda líquida (valor contábil)
    FIND_OBS: TACBrObsMapaResumo; /// Indicador de observações do Mapa-Resumo ECF

    FRegistroB055: TRegistroB055List;
  public
    constructor Create(AOwner: TRegistroB001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_MR: TACBrTipoTotalECF read FIND_MR write FIND_MR;
    property NUM_MR_ECF: Integer read FNUM_MR_ECF write FNUM_MR_ECF;
    property DT_MR_ECF: TDateTime read FDT_MR_ECF write FDT_MR_ECF;
    property CFPS: Integer read FCFPS write FCFPS;
    property NUM_LCTO: Integer read FNUM_LCTO write FNUM_LCTO;
    property VL_BRT: Currency read FVL_BRT write FVL_BRT;
    property VL_CANC_ISS: Currency read FVL_CANC_ISS write FVL_CANC_ISS;
    property VL_CANC_ICMS: Currency read FVL_CANC_ICMS write FVL_CANC_ICMS;
    property VL_CANC: Currency read FVL_CANC write FVL_CANC;
    property VL_DESC_ISS: Currency read FVL_DESC_ISS write FVL_DESC_ISS;
    property VL_DESC_ICMS: Currency read FVL_DESC_ICMS write FVL_DESC_ICMS;
    property VL_DESC: Currency read FVL_DESC write FVL_DESC;
    property VL_ACMO_ISS: Currency read FVL_ACMO_ISS write FVL_ACMO_ISS;
    property VL_ACMO_ICMS: Currency read FVL_ACMO_ICMS write FVL_ACMO_ICMS;
    property VL_ACMO: Currency read FVL_ACMO write FVL_ACMO;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property VL_ISEN_ISS: Currency read FVL_ISEN_ISS write FVL_ISEN_ISS;
    property VL_NT_ISS: Currency read FVL_NT_ISS write FVL_NT_ISS;
    property VL_RT_ISS: Currency read FVL_RT_ISS write FVL_RT_ISS;
    property VL_CONT: Currency read FVL_CONT write FVL_CONT;
    property IND_OBS: TACBrObsMapaResumo read FIND_OBS write FIND_OBS;

    property RegistroB055: TRegistroB055List read FRegistroB055 write FRegistroB055;
  end;

  /// Registro B050 - Lista

  { TRegistroB050List }

  TRegistroB050List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB050;
    procedure SetItem(Index: Integer; const Value: TRegistroB050);
  public
    function New(AOwner: TRegistroB001): TRegistroB050;
    property Items[Index: Integer]: TRegistroB050 read GetItem write SetItem;
  end;

  /// Registro B055 - Detalhe - valores parciais

  { TRegistroB055 }

  TRegistroB055 = class
  private
    FVL_CANC_ISS_P: Currency; /// Parcela correspondente ao valor dos cancelamentos registrados nas operações sujeitas ao ISS
    FVL_CANC_ICMS_P: Currency; /// Parcela correspondente ao valor dos cancelamentos registrados nas operações sujeitas ao ICMS
    FVL_DESC_ISS_P: Currency; /// Parcela correspondente ao valor dos descontos registrados nas operações sujeitas ao ISS
    FVL_DESC_ICMS_P: Currency; /// Parcela correspondente ao valor dos descontos registrados nas operações sujeitas ao ICMS
    FVL_ACMO_ISS_P: Currency; /// Parcela correspondente ao valor dos acréscimos registrados nas operações sujeitas ao ISS
    FVL_ACMO_ICMS_P: Currency; /// Parcela correspondente ao valor dos acréscimos registrados nas operações sujeitas ao ICMS
    FVL_CONT_P: Currency; /// Parcela correspondente ao “Valor Contábil” referente à alíquota do ISS
    FVL_BC_ISS_P: Currency; /// Parcela correspondente ao “Valor da base de cálculo do ISS” referente à alíquota do ISS
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS_P: Currency; /// Parcela correspondente ao “Valor do ISS” referente à alíquota do ISS
    FVL_ISEN_ISS_P: Currency; /// Parcela correspondente ao valor das prestações de serviço isentas do ISS
    FVL_NT_ISS_P: Currency; /// Parcela correspondente ao valor das prestações de serviço sob não-incidência ou não-tributadas pelo ISS
    FVL_RT_ISS_P: Currency; /// Parcela correspondente das prestações de serviço com ISS retido pelo tomador
  public
    constructor Create(AOwner: TRegistroB050); virtual; /// Create

    property VL_CANC_ISS_P: Currency read FVL_CANC_ISS_P write FVL_CANC_ISS_P;
    property VL_CANC_ICMS_P: Currency read FVL_CANC_ICMS_P write FVL_CANC_ICMS_P;
    property VL_DESC_ISS_P: Currency read FVL_DESC_ISS_P write FVL_DESC_ISS_P;
    property VL_DESC_ICMS_P: Currency read FVL_DESC_ICMS_P write FVL_DESC_ICMS_P;
    property VL_ACMO_ISS_P: Currency read FVL_ACMO_ISS_P write FVL_ACMO_ISS_P;
    property VL_ACMO_ICMS_P: Currency read FVL_ACMO_ICMS_P write FVL_ACMO_ICMS_P;
    property VL_CONT_P: Currency read FVL_CONT_P write FVL_CONT_P;
    property VL_BC_ISS_P: Currency read FVL_BC_ISS_P write FVL_BC_ISS_P;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS_P: Currency read FVL_ISS_P write FVL_ISS_P;
    property VL_ISEN_ISS_P: Currency read FVL_ISEN_ISS_P write FVL_ISEN_ISS_P;
    property VL_NT_ISS_P: Currency read FVL_NT_ISS_P write FVL_NT_ISS_P;
    property VL_RT_ISS_P: Currency read FVL_RT_ISS_P write FVL_RT_ISS_P;
  end;

  /// Registro B055 - Lista

  { TRegistroB055List }

  TRegistroB055List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB055;
    procedure SetItem(Index: Integer; const Value: TRegistroB055);
  public
    function New(AOwner: TRegistroB050): TRegistroB055;
    property Items[Index: Integer]: TRegistroB055 read GetItem write SetItem;
  end;

  /// Registro B350 - Lançamento - serviços prestados por instituições financeiras

  { TRegistroB350 }

  TRegistroB350 = class
  private
    FPERIODO: TDateTime; /// Período dos lançamentos relativos aos serviços prestados
    FCOD_CTA: String; /// Código da conta do plano de contas
    FCTA_ISS: String; /// Nome da conta analítica que recepciona os lançamentos do ISS
    FCOD_CTA_COSIF: String; ///Código da conta de acordo com o plano de contas referencial da COSIF, a qual está subordinada a conta informada
    FQTD_LCTO: Integer; /// Quantidade de lançamentos na conta
    FCOD_LST: String; /// Código do serviço conforme lista anexa à Lei Complementar Federal nº
    FVL_CONT: Currency; /// Valor contábil (valor do serviço)
    FVL_BC_ISS: Currency; /// Valor da base de cálculo do ISS
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS: Currency; /// Valor do ISS
    FCOD_INF_OBS: String; /// Código de referência à observação (campo 02 da Linha 0450)
  public
    constructor Create(AOwner: TRegistroB001); virtual; /// Create

    property PERIODO: TDateTime read FPERIODO write FPERIODO;
    property COD_CTA: String read FCOD_CTA write FCOD_CTA;
    property CTA_ISS: String read FCTA_ISS write FCTA_ISS;
    property COD_CTA_COSIF: String read FCOD_CTA_COSIF write FCOD_CTA_COSIF;
    property QTD_LCTO: Integer read FQTD_LCTO write FQTD_LCTO;
    property COD_LST: String read FCOD_LST write FCOD_LST;
    property VL_CONT: Currency read FVL_CONT write FVL_CONT;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property COD_INF_OBS: String read FCOD_INF_OBS write FCOD_INF_OBS;
  end;

  /// Registro B350 - Lista

  { TRegistroB350List }

  TRegistroB350List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB350;
    procedure SetItem(Index: Integer; const Value: TRegistroB350);
  public
    function New(AOwner: TRegistroB001): TRegistroB350;
    property Items[Index: Integer]: TRegistroB350 read GetItem write SetItem;
  end;

  /// Registro B400 - Apuração do ISS

  { TRegistroB400 }

  TRegistroB400 = class
  private
    FDT_INI: TDateTime; /// Data inicial a que a apuração se refere
    FDT_FIN: TDateTime; /// Data final a que a apuração se refere
    FIND_DAD: TACBrLIndicadorMovimento;  /// Indicador de conteúdo

    FRegistroB410: TRegistroB410List;
    FRegistroB420: TRegistroB420List;
    FRegistroB430: TRegistroB430List;
    FRegistroB440: TRegistroB440List;
    FRegistroB450: TRegistroB450List;
    FRegistroB460: TRegistroB460List;
    FRegistroB470: TRegistroB470List;
    FRegistroB480: TRegistroB480List;
    FRegistroB490: TRegistroB490List;
    FRegistroB500: TRegistroB500List;
    FRegistroB600: TRegistroB600List;
  public
    constructor Create(AOwner: TRegistroB001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
    property IND_DAD: TACBrLIndicadorMovimento read FIND_DAD write FIND_DAD;

    property RegistroB410: TRegistroB410List read FRegistroB410 write FRegistroB410;
    property RegistroB420: TRegistroB420List read FRegistroB420 write FRegistroB420;
    property RegistroB430: TRegistroB430List read FRegistroB430 write FRegistroB430;
    property RegistroB440: TRegistroB440List read FRegistroB440 write FRegistroB440;
    property RegistroB450: TRegistroB450List read FRegistroB450 write FRegistroB450;
    property RegistroB460: TRegistroB460List read FRegistroB460 write FRegistroB460;
    property RegistroB470: TRegistroB470List read FRegistroB470 write FRegistroB470;
    property RegistroB480: TRegistroB480List read FRegistroB480 write FRegistroB480;
    property RegistroB490: TRegistroB490List read FRegistroB490 write FRegistroB490;
    property RegistroB500: TRegistroB500List read FRegistroB500 write FRegistroB500;
    property RegistroB600: TRegistroB600List read FRegistroB600 write FRegistroB600;
  end;

  /// Registro B400 - Lista

  { TRegistroB400List }

  TRegistroB400List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB400;
    procedure SetItem(Index: Integer; const Value: TRegistroB400);
  public
    function New(AOwner: TRegistroB001): TRegistroB400;
    property Items[Index: Integer]: TRegistroB400 read GetItem write SetItem;
  end;

  /// Registro B410 - Totalização dos Valores de Aquisições e Prestações

  { TRegistroB410 }

  TRegistroB410 = class
  private
    FIND_TOT: TACBrTotalAquisicaoPrestacao; /// Indicador do tipo de totalização
    FVL_CONT: Currency; /// Totalização dos valores contábeis
    FVL_BC_ISS: Currency; /// Totalização dos valores das bases de cálculo do ISS
    FVL_DED_BC: Currency; /// Totalização dos valores das deduções da base de cálculo
    FVL_RT: Currency; /// Totalização dos valores do ISS retido
    FVL_ISNT_ISS: Currency; /// Totalização dos valores das operações isentas ou não-tributadas pelo ISS
    FVL_ISS: Currency; /// Totalização dos valores do ISS destacado
  public
    constructor Create(AOwner: TRegistroB400); virtual; /// Create

    property IND_TOT: TACBrTotalAquisicaoPrestacao read FIND_TOT write FIND_TOT;
    property VL_CONT: Currency read FVL_CONT write FVL_CONT;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_DED_BC: Currency read FVL_DED_BC write FVL_DED_BC;
    property VL_RT: Currency read FVL_RT write FVL_RT;
    property VL_ISNT_ISS: Currency read FVL_ISNT_ISS write FVL_ISNT_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
  end;

  /// Registro B410 - Lista

  { TRegistroB410List }

  TRegistroB410List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB410;
    procedure SetItem(Index: Integer; const Value: TRegistroB410);
  public
    function New(AOwner: TRegistroB400): TRegistroB410;
    property Items[Index: Integer]: TRegistroB410 read GetItem write SetItem;
  end;

  /// Registro B420 - Relatório dos Valores dos Serviços Prestados por Alíquota

  { TRegistroB420 }

  TRegistroB420 = class
  private
    FVL_CONT: Currency; /// Totalização do "Valor Contábil" referente à alíquota do ISS
    FVL_BC_ISS: Currency; /// Totalização do "Valor da base de cálculo do ISS" referente à alíquota do ISS
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_BC_ISS_RT: Currency; /// Totalização do "Valor da base de cálculo do ISS retido pelo tomador" referente à alíquota do ISS
    FVL_RT: Currency; /// Totalização dos valores do ISS retido pelo tomador
    FVL_ISNT_ISS: Currency; /// Totalização dos valores das operações isentas ou não-tributadas pelo ISS referente à alíquota do ISS
    FVL_ISS: Currency; /// Totalização do "Valor do ISS" referente à alíquota do ISS
  public
    constructor Create(AOwner: TRegistroB400); virtual; /// Create

    property VL_CONT: Currency read FVL_CONT write FVL_CONT;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_BC_ISS_RT: Currency read FVL_BC_ISS_RT write FVL_BC_ISS_RT;
    property VL_RT: Currency read FVL_RT write FVL_RT;
    property VL_ISNT_ISS: Currency read FVL_ISNT_ISS write FVL_ISNT_ISS;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
  end;

  /// Registro B420 - Lista

  { TRegistroB420List }

  TRegistroB420List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB420;
    procedure SetItem(Index: Integer; const Value: TRegistroB420);
  public
    function New(AOwner: TRegistroB400): TRegistroB420;
    property Items[Index: Integer]: TRegistroB420 read GetItem write SetItem;
  end;

  /// Registro B430 - Relatório dos Valores por CFPS

  { TRegistroB430 }

  TRegistroB430 = class
  private
    FCFPS: String; /// Código Fiscal de Prestações de Serviços
    FVL_CONT: Currency; /// Totalização do “Valor Contábil” por CFPS
    FVL_BC_ISS: Currency; /// Totalização do “Valor da base de cálculo do ISS” por CFPS
    FVL_MAT_TERC: Currency; /// Totalização do “Valor dos materiais fornecidos por terceiros na prestação do serviço” por CFPS
    FVL_SUB: Currency; /// Totalização do “Valor da subempreitada” por CFPS
    FVL_ISNT_ISS: Currency; /// Totalização do “Valor das operações isentas ou nãotributadas pelo ISS” por CFPS
    FVL_DED_BC: Currency; /// Totalização do “Valor da dedução da base de cálculo” por CFPS
    FVL_BC_ISS_RT: Currency; /// Totalização do “Valor da base de cálculo de retenção do ISS” por CFPS
    FVL_ISS_RT: Currency; /// Totalização do “Valor do ISS retido pelo tomador” por CFPS
    FVL_ISS: Currency; /// Totalização do “Valor do ISS destacado” por CFPS
  public
    constructor Create(AOwner: TRegistroB400); virtual; /// Create

    property CFPS: String read FCFPS write FCFPS;
    property VL_CONT: Currency read FVL_CONT write FVL_CONT;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_MAT_TERC: Currency read FVL_MAT_TERC write FVL_MAT_TERC;
    property VL_SUB: Currency read FVL_SUB write FVL_SUB;
    property VL_ISNT_ISS: Currency read FVL_ISNT_ISS write FVL_ISNT_ISS;
    property VL_DED_BC: Currency read FVL_DED_BC write FVL_DED_BC;
    property VL_BC_ISS_RT: Currency read FVL_BC_ISS_RT write FVL_BC_ISS_RT;
    property VL_ISS_RT: Currency read FVL_ISS_RT write FVL_ISS_RT;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
  end;

  /// Registro B430 - Lista

  { TRegistroB430List }

  TRegistroB430List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB430;
    procedure SetItem(Index: Integer; const Value: TRegistroB430);
  public
    function New(AOwner: TRegistroB400): TRegistroB430;
    property Items[Index: Integer]: TRegistroB430 read GetItem write SetItem;
  end;

  /// Registro B440 - Relatório dos Valores Retidos

  { TRegistroB440 }

  TRegistroB440 = class
  private
    FIND_OPER: TACBrlTipoOperacao; /// Indicador do tipo de operação
    FCOD_PART: String; /// Código do participante (campo 02 do Registro 0150)
    FVL_CONT_RT: Currency; /// Totalização do "Valor Contábil" referente ao ISS retido pelo tomador
    FVL_BC_ISS_RT: Currency; /// Totalização do "Valor da base de cálculo de retenção do ISS retido pelo tomador"
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_ISS_RT: Currency; /// Totalização do "Valor do ISS retido pelo tomador"
  public
    constructor Create(AOwner: TRegistroB400); virtual; /// Create

    property IND_OPER: TACBrlTipoOperacao read FIND_OPER write FIND_OPER;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property VL_CONT_RT: Currency read FVL_CONT_RT write FVL_CONT_RT;
    property VL_BC_ISS_RT: Currency read FVL_BC_ISS_RT write FVL_BC_ISS_RT;
    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_ISS_RT: Currency read FVL_ISS_RT write FVL_ISS_RT;
  end;

  /// Registro B440 - Lista

  { TRegistroB440List }

  TRegistroB440List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB440;
    procedure SetItem(Index: Integer; const Value: TRegistroB440);
  public
    function New(AOwner: TRegistroB400): TRegistroB440;
    property Items[Index: Integer]: TRegistroB440 read GetItem write SetItem;
  end;

  /// Registro B450 - Relatório dos Valores por Município

  { TRegistroB450 }

  TRegistroB450 = class
  private
    FIND_OPER: TACBrlTipoOperacao; /// Indicador do tipo de totalização
    FCOD_MUN_SERV: Integer; /// Código do município onde o serviço foi prestado
    FVL_CONT: Currency; /// Totalização do "Valor Contábil" por município
    FVL_BC_ISS: Currency; /// Totalização do "Valor da base de cálculo do ISS" por município
    FVL_ISNT_ISS: Currency; /// Totalização do "Valor das operações isentas ou não-tributadas pelo ISS" por município
    FVL_DED_BC: Currency; /// Totalização do "Valor da dedução da base de cálculo" por município
    FVL_ISS_RT: Currency; /// Totalização do "Valor do ISS retido pelo tomador" por município
    FVL_ISS: Currency; /// Totalização do "Valor do ISS destacado" por município
  public
    constructor Create(AOwner: TRegistroB400); virtual; /// Create

    property IND_OPER: TACBrlTipoOperacao read FIND_OPER write FIND_OPER;
    property COD_MUN_SERV: Integer read FCOD_MUN_SERV write FCOD_MUN_SERV;
    property VL_CONT: Currency read FVL_CONT write FVL_CONT;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_ISNT_ISS: Currency read FVL_ISNT_ISS write FVL_ISNT_ISS;
    property VL_DED_BC: Currency read FVL_DED_BC write FVL_DED_BC;
    property VL_ISS_RT: Currency read FVL_ISS_RT write FVL_ISS_RT;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
  end;

  /// Registro B450 - Lista

  { TRegistroB450List }

  TRegistroB450List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB450;
    procedure SetItem(Index: Integer; const Value: TRegistroB450);
  public
    function New(AOwner: TRegistroB400): TRegistroB450;
    property Items[Index: Integer]: TRegistroB450 read GetItem write SetItem;
  end;

  /// Registro B460 - Deduções do ISS

  { TRegistroB460 }

  TRegistroB460 = class
  private
    FIND_DED: TACBrTipoDeducaoISS; /// Indicador do tipo de dedução
    FVL_DED: Currency; /// Valor da dedução
    FNUM_PROC: String; /// Número do processo ao qual o ajuste está vinculado
    FIND_PROC: TACBrlOrigemProcesso; /// Indicador da origem do processo
    FPROC: String; /// Descrição do processo que embasou o lançamento
    FCOD_INF_OBS: String; /// Código de referência à observação

    FRegistroB465: TRegistroB465List;
  public
    constructor Create(AOwner: TRegistroB400); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_DED: TACBrTipoDeducaoISS read FIND_DED write FIND_DED;
    property VL_DED: Currency read FVL_DED write FVL_DED;
    property NUM_PROC: String read FNUM_PROC write FNUM_PROC;
    property IND_PROC: TACBrlOrigemProcesso read FIND_PROC write FIND_PROC;
    property PROC: String read FPROC write FPROC;
    property COD_INF_OBS: String read FCOD_INF_OBS write FCOD_INF_OBS;

    property RegistroB465: TRegistroB465List read FRegistroB465 write FRegistroB465;
  end;

  /// Registro B460 - Lista

  { TRegistroB460List }

  TRegistroB460List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB460;
    procedure SetItem(Index: Integer; const Value: TRegistroB460);
  public
    function New(AOwner: TRegistroB400): TRegistroB460;
    property Items[Index: Integer]: TRegistroB460 read GetItem write SetItem;
  end;

  /// Registro B465 - Compensações

  { TRegistroB465 }

  TRegistroB465 = class
  private
    FIND_COMP: TACBrTipoCompensacaoISS; /// Indicador do tipo de compensação do ISS
    FVL_CRED: Currency; /// Valor do crédito (passível de compensação)
    FVL_COMP: Currency; /// Valor da compensação (compensado)
    FPER_FISCAL: TDateTime; /// Período fiscal da prestação do serviço
    FVL_RES: Currency; /// Valor da resultante (não compensado)
    FCOMENT: String; /// Comentários e observações
  public
    constructor Create(AOwner: TRegistroB460); virtual; /// Create

    property IND_COMP: TACBrTipoCompensacaoISS read FIND_COMP write FIND_COMP;
    property VL_CRED: Currency read FVL_CRED write FVL_CRED;
    property VL_COMP: Currency read FVL_COMP write FVL_COMP;
    property PER_FISCAL: TDateTime read FPER_FISCAL write FPER_FISCAL;
    property VL_RES: Currency read FVL_RES write FVL_RES;
    property COMENT: String read FCOMENT write FCOMENT;
  end;

  /// Registro B465 - Lista

  { TRegistroB465List }

  TRegistroB465List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB465;
    procedure SetItem(Index: Integer; const Value: TRegistroB465);
  public
    function New(AOwner: TRegistroB460): TRegistroB465;
    property Items[Index: Integer]: TRegistroB465 read GetItem write SetItem;
  end;

  /// Registro B470 - Saldos apurados do ISS

  { TRegistroB470 }

  TRegistroB470 = class
  private
    FVL_CONT: Currency; /// A - Valor total referente às prestações de serviço do período
    FVL_MAT_TERC: Currency; /// B - Valor total do material fornecido por terceiros na prestação do serviço
    FVL_MAT_PROP: Currency; /// C - Valor do material próprio utilizado na prestação do serviço
    FVL_SUB: Currency; /// D - Valor total das subempreitadas
    FVL_ISNT: Currency; /// E - Valor total das operações isentas ou não-tributadas pelo ISS
    FVL_DED_BC: Currency; /// F - Valor total das deduções da base de cálculo (B + C + D + E)
    FVL_BC_ISS: Currency; /// G - Valor total da base de cálculo do ISS (A - F)
    FVL_BC_ISS_RT: Currency; /// H - Valor total da base de cálculo de retenção do ISS
    FVL_ISS: Currency; /// I - Valor total do ISS destacado
    FVL_ISS_RT: Currency; /// J - Valor total do ISS retido pelo tomador
    FVL_DED: Currency; /// K - Valor total das deduções do ISS
    FVL_ISS_REC: Currency; /// L - Valor total apurado do ISS a recolher (I - J - K)
    FVL_ISS_ST: Currency; /// M - Valor total do ISS substituto devido pelo tomador
    FVL_ISS_FIL: Currency; /// N - Valor total apurado do ISS das filiais
    FVL_ISS_RT_REC: Currency; /// O - Valor total apurado do ISS a recolher para outros municípios

    FRegistroB475: TRegistroB475List;
    FRegistroB480: TRegistroB480List;
  public
    constructor Create(AOwner: TRegistroB400); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property VL_CONT: Currency read FVL_CONT write FVL_CONT;
    property VL_MAT_TERC: Currency read FVL_MAT_TERC write FVL_MAT_TERC;
    property VL_MAT_PROP: Currency read FVL_MAT_PROP write FVL_MAT_PROP;
    property VL_SUB: Currency read FVL_SUB write FVL_SUB;
    property VL_ISNT: Currency read FVL_ISNT write FVL_ISNT;
    property VL_DED_BC: Currency read FVL_DED_BC write FVL_DED_BC;
    property VL_BC_ISS: Currency read FVL_BC_ISS write FVL_BC_ISS;
    property VL_BC_ISS_RT: Currency read FVL_BC_ISS_RT write FVL_BC_ISS_RT;
    property VL_ISS: Currency read FVL_ISS write FVL_ISS;
    property VL_ISS_RT: Currency read FVL_ISS_RT write FVL_ISS_RT;
    property VL_DED: Currency read FVL_DED write FVL_DED;
    property VL_ISS_REC: Currency read FVL_ISS_REC write FVL_ISS_REC;
    property VL_ISS_ST: Currency read FVL_ISS_ST write FVL_ISS_ST;
    property VL_ISS_FIL: Currency read FVL_ISS_FIL write FVL_ISS_FIL;
    property VL_ISS_RT_REC: Currency read FVL_ISS_RT_REC write FVL_ISS_RT_REC;

    property RegistroB475: TRegistroB475List read FRegistroB475 write FRegistroB475;
    property RegistroB480: TRegistroB480List read FRegistroB480 write FRegistroB480;
  end;

  /// Registro B470 - Lista

  { TRegistroB470List }

  TRegistroB470List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB470;
    procedure SetItem(Index: Integer; const Value: TRegistroB470);
  public
    function New(AOwner: TRegistroB400): TRegistroB470;
    property Items[Index: Integer]: TRegistroB470 read GetItem write SetItem;
  end;

  /// Registro B475 - Obrigações do ISS a recolher por filial

  { TRegistroB475 }

  TRegistroB475 = class
  private
    FCOD_PART: String; /// Código do participante filial (campo 02 do Registro 0150)
    FVL_FIL: Currency; /// Valor total apurado do ISS a recolher pela filial
    FVL_FIL_RT: Currency; /// Totalização do valor apurado do ISS substituto devido pela filial
  public
    constructor Create(AOwner: TRegistroB470); virtual; /// Create

    property COD_PART: String read FCOD_PART write FCOD_PART;
    property VL_FIL: Currency read FVL_FIL write FVL_FIL;
    property VL_FIL_RT: Currency read FVL_FIL_RT write FVL_FIL_RT;
  end;

  /// Registro B475 - Lista

  { TRegistroB475List }

  TRegistroB475List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB475;
    procedure SetItem(Index: Integer; const Value: TRegistroB475);
  public
    function New(AOwner: TRegistroB470): TRegistroB475;
    property Items[Index: Integer]: TRegistroB475 read GetItem write SetItem;
  end;

  /// Registro B480 - Obrigações a Recolher por Município

  { TRegistroB480 }

  TRegistroB480 = class
  private
    FALIQ_ISS: Currency; /// Alíquota do ISS
    FVL_MUN: Currency; /// Valor total do ISS retido a recolher
    FCOD_MUN: Integer; /// Código do município de destino
  public
    constructor Create(AOwner: TRegistroB470); virtual; /// Create

    property ALIQ_ISS: Currency read FALIQ_ISS write FALIQ_ISS;
    property VL_MUN: Currency read FVL_MUN write FVL_MUN;
    property COD_MUN: Integer read FCOD_MUN write FCOD_MUN;
  end;

  /// Registro B480 - Lista

  { TRegistroB480List }

  TRegistroB480List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB480;
    procedure SetItem(Index: Integer; const Value: TRegistroB480);
  public
    function New(AOwner: TRegistroB470): TRegistroB480;
    property Items[Index: Integer]: TRegistroB480 read GetItem write SetItem;
  end;

  /// Registro B490 - Obrigações do ISS a recolher

  { TRegistroB490 }

  TRegistroB490 = class
  private
    FCOD_OR: Integer; /// Código da obrigação a recolher
    FVL_OR: Currency; /// Valor da obrigação a recolher
    FDT_VCTO: TDateTime; /// Data de vencimento da obrigação
    FCOD_REC: String; /// Código de receita referente à obrigação
    FCOD_MUN_SERV: Integer; /// Código do município a que se destina a obrigação, conforme a tabela indicada no item 3.3.1
    FNUM_PROC: String; /// Número do processo ao qual a obrigação está vinculada
    FIND_PROC: TACBrlOrigemProcesso; /// Indicador da origem do processo
    FPROC: String; /// Descrição do processo que embasou o lançamento
    FCOD_INF_OBS: String; /// Código de referência à observação
  public
    constructor Create(AOwner: TRegistroB400); virtual; /// Create

    property COD_OR: Integer read FCOD_OR write FCOD_OR;
    property VL_OR: Currency read FVL_OR write FVL_OR;
    property DT_VCTO: TDateTime read FDT_VCTO write FDT_VCTO;
    property COD_REC: String read FCOD_REC write FCOD_REC;
    property COD_MUN_SERV: Integer read FCOD_MUN_SERV write FCOD_MUN_SERV;
    property NUM_PROC: String read FNUM_PROC write FNUM_PROC;
    property IND_PROC: TACBrlOrigemProcesso read FIND_PROC write FIND_PROC;
    property PROC: String read FPROC write FPROC;
    property COD_INF_OBS: String read FCOD_INF_OBS write FCOD_INF_OBS;
  end;

  /// Registro B490 - Lista

  { TRegistroB490List }

  TRegistroB490List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB490;
    procedure SetItem(Index: Integer; const Value: TRegistroB490);
  public
    function New(AOwner: TRegistroB400): TRegistroB490;
    property Items[Index: Integer]: TRegistroB490 read GetItem write SetItem;
  end;

  /// Registro B500 - Sociedade uniprofissional

  { TRegistroB500 }

  TRegistroB500 = class
  private
    FVL_REC: Currency; /// Valor mensal das receitas auferidas pela sociedade uniprofissional
    FQTD_PROF: Integer; /// Quantidade de profissionais habilitados
    FVL_OR: Currency; /// Valor do ISS a recolher

    FRegistroB510: TRegistroB510List;
  public
    constructor Create(AOwner: TRegistroB400); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property VL_REC: Currency read FVL_REC write FVL_REC;
    property QTD_PROF: Integer read FQTD_PROF write FQTD_PROF;
    property VL_OR: Currency read FVL_OR write FVL_OR;

    property RegistroB510: TRegistroB510List read FRegistroB510 write FRegistroB510;
  end;

  /// Registro B500 - Lista

  { TRegistroB500List }

  TRegistroB500List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB500;
    procedure SetItem(Index: Integer; const Value: TRegistroB500);
  public
    function New(AOwner: TRegistroB400): TRegistroB500;
    property Items[Index: Integer]: TRegistroB500 read GetItem write SetItem;
  end;

  /// Registro B510 - Profissionais habilitados

  { TRegistroB510 }

  TRegistroB510 = class
  private
    FIND_PROF: TACBrHabilitacaoProfissional; /// Indicador de habilitação
    FIND_ESC: TACBrEscolaridade; /// Indicador de escolaridade
    FIND_SOC: TACBrParticipacaoSocietaria; /// Indicador de participação societária
    FCPF: String; /// CPF do profissional
    FNOME: String; /// Nome do profissional
  public
    constructor Create(AOwner: TRegistroB500); virtual; /// Create

    property IND_PROF: TACBrHabilitacaoProfissional read FIND_PROF write FIND_PROF;
    property IND_ESC: TACBrEscolaridade read FIND_ESC write FIND_ESC;
    property IND_SOC: TACBrParticipacaoSocietaria read FIND_SOC write FIND_SOC;
    property CPF: String read FCPF write FCPF;
    property NOME: String read FNOME write FNOME;
  end;

  /// Registro B510 - Lista

  { TRegistroB510List }

  TRegistroB510List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB510;
    procedure SetItem(Index: Integer; const Value: TRegistroB510);
  public
    function New(AOwner: TRegistroB500): TRegistroB510;
    property Items[Index: Integer]: TRegistroB510 read GetItem write SetItem;
  end;

  /// Registro B600 - Informações Complementares

  { TRegistroB600 }

  TRegistroB600 = class
  private
    FVL_PGTOS: Currency; /// Valor dos pagamentos não sujeitos a retenção do ISS
    FVL_ALUG: Currency; /// Valor pago a título de aluguel de imóvel
    FVL_AGUA: Currency; /// Valor pago a título de consumo de água canalizada
    FVL_EEL: Currency; /// Valor pago a título de consumo de energia elétrica
    FVL_REM: Currency; /// Valor pago a título de remunerações a funcionários, honorários, prólabores e retiradas
    FVL_OUT_DESP: Currency; /// Valor pago a título de outras despesas do período
    FVL_RPA: Currency; /// Valores retidos por prestadores de serviço autônomo
  public
    constructor Create(AOwner: TRegistroB400); virtual; /// Create

    property VL_PGTOS: Currency read FVL_PGTOS write FVL_PGTOS;
    property VL_ALUG: Currency read FVL_ALUG write FVL_ALUG;
    property VL_AGUA: Currency read FVL_AGUA write FVL_AGUA;
    property VL_EEL: Currency read FVL_EEL write FVL_EEL;
    property VL_REM: Currency read FVL_REM write FVL_REM;
    property VL_OUT_DESP: Currency read FVL_OUT_DESP write FVL_OUT_DESP;
    property VL_RPA: Currency read FVL_RPA write FVL_RPA;
  end;

  /// Registro B600 - Lista

  { TRegistroB600List }

  TRegistroB600List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB600;
    procedure SetItem(Index: Integer; const Value: TRegistroB600);
  public
    function New(AOwner: TRegistroB400): TRegistroB600;
    property Items[Index: Integer]: TRegistroB600 read GetItem write SetItem;
  end;

  /// Registro B700 - Registro de contratos

  { TRegistroB700 }

  TRegistroB700 = class
  private
    FNAT_OBRA: String; /// Natureza da obra contratada
    FCOD_PART: String; /// Código do participante (campo 02 do Registro 0150)
    FUF: String; /// Sigla da unidade da federação onde está localizada a obra
    FCEP: String; /// Código de Endereçamento Postal
    FNUM: String; /// Número do imóvel
    FCOMPL: String; /// Dados complementares do endereço
    FBAIRRO: String; /// Bairro em que a obra está situada
    FCTRT: String; /// Espécie do contrato
    FDT_CTRT: TDateTime; /// Data da contratação
    FCART_REG: String; /// Registro da obra no cartório (nome do cartório, livro e folha)
    FDT_OBR_INI: TDateTime; /// Data de início da obra
    FDT_ENC_OB: TDateTime; /// Data de encerramento da obra
    FVL_OR_MUN: Currency; /// Valor total da obra
    FCOMENT: String; /// Comentários e observações
  public
    constructor Create(AOwner: TRegistroB001); virtual; /// Create

    property NAT_OBRA: String read FNAT_OBRA write FNAT_OBRA;
    property COD_PART: String read FCOD_PART write FCOD_PART;
    property UF: String read FUF write FUF;
    property CEP: String read FCEP write FCEP;
    property NUM: String read FNUM write FNUM;
    property COMPL: String read FCOMPL write FCOMPL;
    property BAIRRO: String read FBAIRRO write FBAIRRO;
    property CTRT: String read FCTRT write FCTRT;
    property DT_CTRT: TDateTime read FDT_CTRT write FDT_CTRT;
    property CART_REG: String read FCART_REG write FCART_REG;
    property DT_OBR_INI: TDateTime read FDT_OBR_INI write FDT_OBR_INI;
    property DT_ENC_OB: TDateTime read FDT_ENC_OB write FDT_ENC_OB;
    property VL_OR_MUN: Currency read FVL_OR_MUN write FVL_OR_MUN;
    property COMENT: String read FCOMENT write FCOMENT;
  end;

  /// Registro B700 - Lista

  { TRegistroB700List }

  TRegistroB700List = class(TACBrLFDRegistros)
  private
    function GetItem(Index: Integer): TRegistroB700;
    procedure SetItem(Index: Integer; const Value: TRegistroB700);
  public
    function New(AOwner: TRegistroB001): TRegistroB700;
    property Items[Index: Integer]: TRegistroB700 read GetItem write SetItem;
  end;

  /// Registro B990 - Encerramento do Bloco B

  { TRegistroB990 }

  TRegistroB990 = class
  private
    fQTD_LIN_B: Integer;
  public
    property QTD_LIN_B: Integer read fQTD_LIN_B write fQTD_LIN_B;
  end;

implementation

{ TRegistroB001 }

constructor TRegistroB001.Create;
begin
  FRegistroB020 := TRegistroB020List.Create;
  FRegistroB030 := TRegistroB030List.Create;
  FRegistroB700 := TRegistroB700List.Create;
  FRegistroB400 := TRegistroB400List.Create;
  FRegistroB350 := TRegistroB350List.Create;
  FRegistroB040 := TRegistroB040List.Create;
  FRegistroB050 := TRegistroB050List.Create;
  //
  IND_MOV := imlSemDados;
end;

destructor TRegistroB001.Destroy;
begin
  FRegistroB020.Free;
  FRegistroB030.Free;
  FRegistroB700.Free;
  FRegistroB400.Free;
  FRegistroB350.Free;
  FRegistroB040.Free;
  FRegistroB050.Free;
  inherited;
end;

{ TRegistroB020 }

constructor TRegistroB020.Create(AOwner: TRegistroB001);
begin
  FRegistroB025 := TRegistroB025List.Create;
end;

destructor TRegistroB020.Destroy;
begin
  FRegistroB025.Free;
  inherited;
end;

{ TRegistroB020List }

function TRegistroB020List.GetItem(Index: Integer): TRegistroB020;
begin
  Result := TRegistroB020(Get(Index));
end;

function TRegistroB020List.New(AOwner: TRegistroB001): TRegistroB020;
begin
  Result := TRegistroB020.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB020List.SetItem(Index: Integer; const Value: TRegistroB020);
begin
  Put(Index, Value);
end;

{ TRegistroB025 }

constructor TRegistroB025.Create(AOwner: TRegistroB020);
begin
end;

{ TRegistroB025List }

function TRegistroB025List.GetItem(Index: Integer): TRegistroB025;
begin
  Result := TRegistroB025(Get(Index));
end;

function TRegistroB025List.New(AOwner: TRegistroB020): TRegistroB025;
begin
  Result := TRegistroB025.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB025List.SetItem(Index: Integer; const Value: TRegistroB025);
begin
  Put(Index, Value);
end;

{ TRegistroB030 }

constructor TRegistroB030.Create(AOwner: TRegistroB001);
begin
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
  Result := TRegistroB030(Get(Index));
end;

function TRegistroB030List.New(AOwner: TRegistroB001): TRegistroB030;
begin
  Result := TRegistroB030.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB030List.SetItem(Index: Integer; const Value: TRegistroB030);
begin
  Put(Index, Value);
end;

{ TRegistroB035 }

constructor TRegistroB035.Create(AOwner: TRegistroB030);
begin
end;

{ TRegistroB035List }

function TRegistroB035List.GetItem(Index: Integer): TRegistroB035;
begin
  Result := TRegistroB035(Get(Index));
end;

function TRegistroB035List.New(AOwner: TRegistroB030): TRegistroB035;
begin
  Result := TRegistroB035.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB035List.SetItem(Index: Integer; const Value: TRegistroB035);
begin
  Put(Index, Value);
end;

{ TRegistroB040 }

constructor TRegistroB040.Create(AOwner: TRegistroB001);
begin
  FRegistroB045 := TRegistroB045List.Create;
end;

destructor TRegistroB040.Destroy;
begin
  FRegistroB045.Free;
  inherited;
end;

{ TRegistroB040List }

function TRegistroB040List.GetItem(Index: Integer): TRegistroB040;
begin
  Result := TRegistroB040(Get(Index));
end;

function TRegistroB040List.New(AOwner: TRegistroB001): TRegistroB040;
begin
  Result := TRegistroB040.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB040List.SetItem(Index: Integer; const Value: TRegistroB040);
begin
  Put(Index, Value);
end;

{ TRegistroB045 }

constructor TRegistroB045.Create(AOwner: TRegistroB040);
begin
end;

{ TRegistroB045List }

function TRegistroB045List.GetItem(Index: Integer): TRegistroB045;
begin
  Result := TRegistroB045(Get(Index));
end;

function TRegistroB045List.New(AOwner: TRegistroB040): TRegistroB045;
begin
  Result := TRegistroB045.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB045List.SetItem(Index: Integer; const Value: TRegistroB045);
begin
  Put(Index, Value);
end;

{ TRegistroB050 }

constructor TRegistroB050.Create(AOwner: TRegistroB001);
begin
  FRegistroB055 := TRegistroB055List.Create;
end;

destructor TRegistroB050.Destroy;
begin
  FRegistroB055.Free;
  inherited;
end;

{ TRegistroB050List }

function TRegistroB050List.GetItem(Index: Integer): TRegistroB050;
begin
  Result := TRegistroB050(Get(Index));
end;

function TRegistroB050List.New(AOwner: TRegistroB001): TRegistroB050;
begin
  Result := TRegistroB050.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB050List.SetItem(Index: Integer; const Value: TRegistroB050);
begin
  Put(Index, Value);
end;

{ TRegistroB055 }

constructor TRegistroB055.Create(AOwner: TRegistroB050);
begin
end;

{ TRegistroB055List }

function TRegistroB055List.GetItem(Index: Integer): TRegistroB055;
begin
  Result := TRegistroB055(Get(Index));
end;

function TRegistroB055List.New(AOwner: TRegistroB050): TRegistroB055;
begin
  Result := TRegistroB055.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB055List.SetItem(Index: Integer; const Value: TRegistroB055);
begin
  Put(Index, Value);
end;

{ TRegistroB350 }

constructor TRegistroB350.Create(AOwner: TRegistroB001);
begin
end;

{ TRegistroB350List }

function TRegistroB350List.GetItem(Index: Integer): TRegistroB350;
begin
  Result := TRegistroB350(Get(Index));
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

{ TRegistroB400 }

constructor TRegistroB400.Create(AOwner: TRegistroB001);
begin
  FRegistroB410 := TRegistroB410List.Create;
  FRegistroB420 := TRegistroB420List.Create;
  FRegistroB430 := TRegistroB430List.Create;
  FRegistroB440 := TRegistroB440List.Create;
  FRegistroB450 := TRegistroB450List.Create;
  FRegistroB460 := TRegistroB460List.Create;
  FRegistroB470 := TRegistroB470List.Create;
  FRegistroB480 := TRegistroB480List.Create;
  FRegistroB490 := TRegistroB490List.Create;
  FRegistroB500 := TRegistroB500List.Create;
  FRegistroB600 := TRegistroB600List.Create;
end;

destructor TRegistroB400.Destroy;
begin
  FRegistroB410.Free;
  FRegistroB420.Free;
  FRegistroB430.Free;
  FRegistroB440.Free;
  FRegistroB450.Free;
  FRegistroB460.Free;
  FRegistroB470.Free;
  FRegistroB480.Free;
  FRegistroB490.Free;
  FRegistroB500.Free;
  FRegistroB600.Free;
  inherited;
end;

{ TRegistroB400List }

function TRegistroB400List.GetItem(Index: Integer): TRegistroB400;
begin
  Result := TRegistroB400(Get(Index));
end;

function TRegistroB400List.New(AOwner: TRegistroB001): TRegistroB400;
begin
  Result := TRegistroB400.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB400List.SetItem(Index: Integer; const Value: TRegistroB400);
begin
  Put(Index, Value);
end;

{ TRegistroB410 }

constructor TRegistroB410.Create(AOwner: TRegistroB400);
begin
end;

{ TRegistroB410List }

function TRegistroB410List.GetItem(Index: Integer): TRegistroB410;
begin
  Result := TRegistroB410(Get(Index));
end;

function TRegistroB410List.New(AOwner: TRegistroB400): TRegistroB410;
begin
  Result := TRegistroB410.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB410List.SetItem(Index: Integer; const Value: TRegistroB410);
begin
  Put(Index, Value);
end;

{ TRegistroB420 }

constructor TRegistroB420.Create(AOwner: TRegistroB400);
begin
end;

{ TRegistroB420List }

function TRegistroB420List.GetItem(Index: Integer): TRegistroB420;
begin
  Result := TRegistroB420(Get(Index));
end;

function TRegistroB420List.New(AOwner: TRegistroB400): TRegistroB420;
begin
  Result := TRegistroB420.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB420List.SetItem(Index: Integer; const Value: TRegistroB420);
begin
  Put(Index, Value);
end;

{ TRegistroB430 }

constructor TRegistroB430.Create(AOwner: TRegistroB400);
begin
end;

{ TRegistroB430List }

function TRegistroB430List.GetItem(Index: Integer): TRegistroB430;
begin
  Result := TRegistroB430(Get(Index));
end;

function TRegistroB430List.New(AOwner: TRegistroB400): TRegistroB430;
begin
  Result := TRegistroB430.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB430List.SetItem(Index: Integer; const Value: TRegistroB430);
begin
  Put(Index, Value);
end;

{ TRegistroB440 }

constructor TRegistroB440.Create(AOwner: TRegistroB400);
begin
end;

{ TRegistroB440List }

function TRegistroB440List.GetItem(Index: Integer): TRegistroB440;
begin
  Result := TRegistroB440(Get(Index));
end;

function TRegistroB440List.New(AOwner: TRegistroB400): TRegistroB440;
begin
  Result := TRegistroB440.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB440List.SetItem(Index: Integer; const Value: TRegistroB440);
begin
  Put(Index, Value);
end;

{ TRegistroB450 }

constructor TRegistroB450.Create(AOwner: TRegistroB400);
begin
end;

{ TRegistroB450List }

function TRegistroB450List.GetItem(Index: Integer): TRegistroB450;
begin
  Result := TRegistroB450(Get(Index));
end;

function TRegistroB450List.New(AOwner: TRegistroB400): TRegistroB450;
begin
  Result := TRegistroB450.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB450List.SetItem(Index: Integer; const Value: TRegistroB450);
begin
  Put(Index, Value);
end;

{ TRegistroB460 }

constructor TRegistroB460.Create(AOwner: TRegistroB400);
begin
  FRegistroB465 := TRegistroB465List.Create;
end;

destructor TRegistroB460.Destroy;
begin
  FRegistroB465.Free;
  inherited;
end;

{ TRegistroB460List }

function TRegistroB460List.GetItem(Index: Integer): TRegistroB460;
begin
  Result := TRegistroB460(Get(Index));
end;

function TRegistroB460List.New(AOwner: TRegistroB400): TRegistroB460;
begin
  Result := TRegistroB460.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB460List.SetItem(Index: Integer; const Value: TRegistroB460);
begin
  Put(Index, Value);
end;

{ TRegistroB465 }

constructor TRegistroB465.Create(AOwner: TRegistroB460);
begin
end;

{ TRegistroB465List }

function TRegistroB465List.GetItem(Index: Integer): TRegistroB465;
begin
  Result := TRegistroB465(Get(Index));
end;

function TRegistroB465List.New(AOwner: TRegistroB460): TRegistroB465;
begin
  Result := TRegistroB465.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB465List.SetItem(Index: Integer; const Value: TRegistroB465);
begin
  Put(Index, Value);
end;

{ TRegistroB470 }

constructor TRegistroB470.Create(AOwner: TRegistroB400);
begin
  FRegistroB475 := TRegistroB475List.Create;
  FRegistroB480 := TRegistroB480List.Create;
end;

destructor TRegistroB470.Destroy;
begin
  FRegistroB475.Free;
  FRegistroB480.Free;
  inherited;
end;

{ TRegistroB470List }

function TRegistroB470List.GetItem(Index: Integer): TRegistroB470;
begin
  Result := TRegistroB470(Get(Index));
end;

function TRegistroB470List.New(AOwner: TRegistroB400): TRegistroB470;
begin
  Result := TRegistroB470.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB470List.SetItem(Index: Integer; const Value: TRegistroB470);
begin
  Put(Index, Value);
end;

{ TRegistroB475 }

constructor TRegistroB475.Create(AOwner: TRegistroB470);
begin
end;

{ TRegistroB475List }

function TRegistroB475List.GetItem(Index: Integer): TRegistroB475;
begin
  Result := TRegistroB475(Get(Index));
end;

function TRegistroB475List.New(AOwner: TRegistroB470): TRegistroB475;
begin
  Result := TRegistroB475.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB475List.SetItem(Index: Integer; const Value: TRegistroB475);
begin
  Put(Index, Value);
end;

{ TRegistroB480 }

constructor TRegistroB480.Create(AOwner: TRegistroB470);
begin
end;

{ TRegistroB480List }

function TRegistroB480List.GetItem(Index: Integer): TRegistroB480;
begin
  Result := TRegistroB480(Get(Index));
end;

function TRegistroB480List.New(AOwner: TRegistroB470): TRegistroB480;
begin
  Result := TRegistroB480.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB480List.SetItem(Index: Integer; const Value: TRegistroB480);
begin
  Put(Index, Value);
end;

{ TRegistroB490 }

constructor TRegistroB490.Create(AOwner: TRegistroB400);
begin
end;

{ TRegistroB490List }

function TRegistroB490List.GetItem(Index: Integer): TRegistroB490;
begin
  Result := TRegistroB490(Get(Index));
end;

function TRegistroB490List.New(AOwner: TRegistroB400): TRegistroB490;
begin
  Result := TRegistroB490.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB490List.SetItem(Index: Integer; const Value: TRegistroB490);
begin
  Put(Index, Value);
end;

{ TRegistroB500 }

constructor TRegistroB500.Create(AOwner: TRegistroB400);
begin
  FRegistroB510 := TRegistroB510List.Create;
end;

destructor TRegistroB500.Destroy;
begin
  FRegistroB510.Free;
  inherited;
end;

{ TRegistroB500List }

function TRegistroB500List.GetItem(Index: Integer): TRegistroB500;
begin
  Result := TRegistroB500(Get(Index));
end;

function TRegistroB500List.New(AOwner: TRegistroB400): TRegistroB500;
begin
  Result := TRegistroB500.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB500List.SetItem(Index: Integer; const Value: TRegistroB500);
begin
  Put(Index, Value);
end;

{ TRegistroB510 }

constructor TRegistroB510.Create(AOwner: TRegistroB500);
begin
end;

{ TRegistroB510List }

function TRegistroB510List.GetItem(Index: Integer): TRegistroB510;
begin
  Result := TRegistroB510(Get(Index));
end;

function TRegistroB510List.New(AOwner: TRegistroB500): TRegistroB510;
begin
  Result := TRegistroB510.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB510List.SetItem(Index: Integer; const Value: TRegistroB510);
begin
  Put(Index, Value);
end;

{ TRegistroB600 }

constructor TRegistroB600.Create(AOwner: TRegistroB400);
begin
end;

{ TRegistroB600List }

function TRegistroB600List.GetItem(Index: Integer): TRegistroB600;
begin
  Result := TRegistroB600(Get(Index));
end;

function TRegistroB600List.New(AOwner: TRegistroB400): TRegistroB600;
begin
  Result := TRegistroB600.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB600List.SetItem(Index: Integer; const Value: TRegistroB600);
begin
  Put(Index, Value);
end;

{ TRegistroB700 }

constructor TRegistroB700.Create(AOwner: TRegistroB001);
begin
end;

{ TRegistroB700List }

function TRegistroB700List.GetItem(Index: Integer): TRegistroB700;
begin
  Result := TRegistroB700(Get(Index));
end;

function TRegistroB700List.New(AOwner: TRegistroB001): TRegistroB700;
begin
  Result := TRegistroB700.Create(AOwner);
  Add(Result);
end;

procedure TRegistroB700List.SetItem(Index: Integer; const Value: TRegistroB700);
begin
  Put(Index, Value);
end;

end.
