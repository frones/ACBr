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
*******************************************************************************}

unit ACBrFContBloco_M;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrFContBlocos;

type
  /// Registro M001 - ABERTURA DO BLOCO M

  TRegistroM001 = class(TOpenBlocos)
  private
  public
  end;

  TRegistroM155List = class;
  TRegistroM355List = class;


  ///Registro M020 - QUALIFICAÇÃO DA PESSOA JURÍDICA E RETIFICAÇÃO

  TRegistroM020 = class
  private
    fQUALI_PJ        : String;    /// Qualificação da Pessoa Jurídica.
    fTIPO_ESCRIT     : String;    /// Tipo de Escrituração: 0-Original, 1-Retificadora
    fNRO_REC_ANTERIOR: String;    /// Número do recibo da escrituração anterior a ser retificada.
    
    fID_ESCR_PER_ANT: String; //  Campo calculado pelo sistema. Identificação da escrituração do período anterior utilizada para a recuperação de saldos
    fSIT_SLD_PER_ANT : String; //Situação do saldo da escrituração do período anterior: "R" - Recuperado; "N" - Não recuperado; "E" - Editado; "I" - Importado; (Campo preenchido pelo sistema.)
    fIND_LCTO_INI_SLD:  String;// Indicativo de permissão de lançamentos do tipo  inicialização  (IS / IF) para ajuste dos saldos iniciais: 0  (Zero) -  Saldos iniciais não podem ser 1 (Um)- Saldos podem ser ajustadosCampo preenchido pelo sistema.)
    fFORM_APUR : String; //Forma de apuração
    fFORM_TRIBUT : String; //Forma de Tributação
    fTRIM_LUC_ARB: String; //Trimestre de Lucro Arbitrado
    fFORM_TRIB_TRI:  String; //Apuração do Trimestre
  public
    property QUALI_PJ: String read fQUALI_PJ write fQUALI_PJ;
    property TIPO_ESCRIT: String read fTIPO_ESCRIT write fTIPO_ESCRIT;
    property NRO_REC_ANTERIOR: String read fNRO_REC_ANTERIOR write fNRO_REC_ANTERIOR;

    property ID_ESCR_PER_ANT: String read fID_ESCR_PER_ANT write fID_ESCR_PER_ANT;
    property SIT_SLD_PER_ANT: String read fSIT_SLD_PER_ANT write fSIT_SLD_PER_ANT;
    property IND_LCTO_INI_SLD: String read fIND_LCTO_INI_SLD write fIND_LCTO_INI_SLD;
    property FORM_APUR: String read fFORM_APUR write fFORM_APUR;
    property FORM_TRIBUT: String read fFORM_TRIBUT write fFORM_TRIBUT;
    property TRIM_LUC_ARB: String read fTRIM_LUC_ARB write fTRIM_LUC_ARB;
    property FORM_TRIB_TRI: String read fFORM_TRIB_TRI write fFORM_TRIB_TRI;
  end;


  ///Registro M025 - Saldos Iniciais das Contas Patrimoniais Recuperados/Preenchidos

  TRegistroM025 = class
  private
    fCOD_CTA        : String;    /// Código da conta analítica.
    fCOD_CCUS     : String;    /// Código do centro de custos.
    fCOD_CTA_REF: String;    /// Código da conta no plano de contas referencial, conforme tabela publicada pelos órgãos indicados no campo COD_ENT_REF

    fVL_SLD_FIN_FC: Currency; //  Valor do saldo final fiscal recuperado do período anterior.
    fIND_DC_FIN_FC : String; //Indicador da situação do saldo fiscal final: D - Devedor; C - Credor.
    fVL_SLD_FIN_SOC:  Currency;// Valor do saldo final societário recuperado do período anterior.
    fIND_DC_FIN_SOC : String; //Indicador da situação do saldo Societário final: D - Devedor; C - Credor.
  public
    property COD_CTA: String read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: String read fCOD_CCUS write fCOD_CCUS;
    property COD_CTA_REF: String read fCOD_CTA_REF write fCOD_CTA_REF;
    property VL_SLD_FIN_FC: Currency read fVL_SLD_FIN_FC write fVL_SLD_FIN_FC;
    property IND_DC_FIN_FC: String read fIND_DC_FIN_FC write fIND_DC_FIN_FC;
    property VL_SLD_FIN_SOC: Currency read fVL_SLD_FIN_SOC write fVL_SLD_FIN_SOC;
    property IND_DC_FIN_SOC: String read fIND_DC_FIN_SOC write fIND_DC_FIN_SOC;
  end;

  /// Registro M025 - Lista

  TRegistroM025List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM025;
    procedure SetItem(Index: Integer; const Value: TRegistroM025);
  public
    function New: TRegistroM025;
    property Items[Index: Integer]: TRegistroM025 read GetItem write SetItem;
  end;



  /// Registro M030 – IDENTIFICAÇÃO DO PERÍODO DE APURAÇÃO do Lucro real

  TRegistroM030 = class
  private
    fIND_PER        : String;    /// Período Apuração
    fVL_LUC_LIQ     : Currency;  /// Resultado do Período - Valor do lucro líquido (ou do prejuízo) contábil do período
    fIND_LUC_LIQ    : String;    /// Situação do Resultado do Período

    FRegistroM155: TRegistroM155List;  /// BLOCO M - Lista de RegistroM155 (FILHO)
    FRegistroM355: TRegistroM355List;  /// BLOCO M - Lista de RegistroM355 (FILHO)
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_PER        : String read fIND_PER        write fIND_PER;
    property VL_LUC_LIQ     : Currency read fVL_LUC_LIQ     write fVL_LUC_LIQ;
    property IND_LUC_LIQ    : String read fIND_LUC_LIQ    write fIND_LUC_LIQ;

    /// Registros FILHOS
    property RegistroM155: TRegistroM155List read FRegistroM155 write FRegistroM155;
    property RegistroM355: TRegistroM355List read FRegistroM355 write FRegistroM355;
  end;

  /// Registro M030 - Lista

  TRegistroM030List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM030;
    procedure SetItem(Index: Integer; const Value: TRegistroM030);
  public
    function New: TRegistroM030;
    property Items[Index: Integer]: TRegistroM030 read GetItem write SetItem;
  end;

  /// Registro M155 - Detalhes dos Saldos Referenciais das Contas Patrimoniais

  TRegistroM155 = class
  private
    fCOD_CTA: String;    /// Código da conta analítica. Recuperado do Registro I155 e M025
    fCOD_CCUS: String;       /// Código do centro de custos. Recuperado do Registro I155 e M025.
    fCOD_CTA_REF: String;    /// Código da conta no plano de contas referencial, conforme tabela publicada pelos órgãos indicados no campo COD_ENT_REF
    fVL_SLD_INI_SOC_ANT: Currency; //Valor do saldo inicial societário. Calculado conforme -Cálculo dos Saldos Iniciais Antes dos Lançamentos de Inicialização
    fIND_DC_INI_SOC_ANT: String; //Indicador da situação do saldo inicial societário: D - Devedor; C - Credor.
    fVL_IS_DEB: Currency; //Valor dos lançamentos de débito do Tipo "IS" Calculado conforme - Cálculo dos Lançamentos. public
    fVL_IS_CRED: Currency; //Valor dos lançamentos de crédito do Tipo "IS" Calculado conforme - Cálculo dos Lançamentos. property COD_ENT_REF: String read fCOD_ENT_REF write fCOD_ENT_REF;
    fVL_SLD_INI_SOC: Currency; //Valor do saldo inicial societário. Calculado conforme  -Cálculo dos Saldos Iniciais Ajustados
    fIND_DC_INI_SOC: String; //Indicador da situação do saldo inicial societário: D - Devedor; C - Credor.
    fVL_SLD_INI_FC_ANT: Currency; //Valor do saldo fiscal inicial. Calculado conforme  -Cálculo dos Saldos Iniciais Antes dos Lançamentos de Inicialização
    fIND_DC_INI_FC_ANT: String; //Indicador da situação do saldo inicial: D - Devedor; C - Credor.
    fVL_IF_DEB: Currency; //Valor dos lançamentos de débito do Tipo "IF"  Calculado conforme - Cálculo dos Lançamentos.
    fVL_IF_CRED: Currency; //Valor dos lançamentos de crédito do Tipo "IF" Calculado conforme - Cálculo dos Lançamentos.
    fVL_SLD_INI_FC: Currency; //Valor do saldo fiscal inicial. Calculado conforme - Cálculo dos Saldos Iniciais Ajustados.
    fIND_DC_INI_FC: String; //Indicador da situação do saldo inicial: D - Devedor; C - Credor.
    fVL_DEB_CONTABIL: Currency; //Lançamentos contábeis de débito. Calculado conforme - Cálculo dos Débitos e Créditos Societários.
    fVL_CRED_CONTABIL: Currency; //Lançamentos contábeis de crédito. Calculado conforme - Cálculo dos Débitos e Créditos Societários.
    fVL_DEB_FCONT_E: Currency; //Valor dos lançamentos de expurgos devedores. Calculado conforme - Cálculo dos Lançamentos.
    fVL_CRED_FCONT_E: Currency; //Valor dos lançamentos de expurgos credores. Calculado conforme - Cálculo dos Lançamentos.
    fVL_DEB_FCONT_I: Currency; //Valor dos lançamentos de inclusão devedores. Calculado conforme - Cálculo dos Lançamentos.
    fVL_CRED_FCONT_I: Currency; //Valor dos lançamentos de inclusão credores. Calculado conforme - Cálculo dos Lançamentos.
    fVL_TR_DEB: Currency; //Valor dos lançamentos de débito do Tipo "TR" Calculado conforme - Cálculo dos Lançamentos.
    fVL_TR_CRED: Currency; //Valor dos lançamentos de crédito do Tipo "TR" Calculado conforme - Cálculo dos Lançamentos.
    fVL_TF_DEB: Currency; //Valor dos lançamentos de débito do Tipo "TF" Calculado conforme - Cálculo dos Lançamentos.
    fVL_TF_CRED: Currency; //Valor dos lançamentos de crédito do Tipo "TF" Calculado conforme - Cálculo dos Lançamentos.
    fVL_TS_DEB: Currency; //Valor dos lançamentos de débito do Tipo "TS" Calculado conforme - Cálculo dos Lançamentos.
    fVL_TS_CRED: Currency; //Valor dos lançamentos de crédito do Tipo "TS" Calculado conforme - Cálculo dos Lançamentos.
    fVL_EF_DEB: Currency; //Valor dos lançamentos de débito do Tipo "EF" Calculado conforme - Cálculo dos Lançamentos.
    fVL_EF_CRED: Currency; //Valor dos lançamentos de crédito do Tipo "EF" Calculado conforme - Cálculo dos Lançamentos.
    fVL_SLD_FIN_FC: Currency; //Valor do saldo fiscal final do período. Calculado conforme - Cálculo do Saldo Final Fiscal.
    fIND_DC_FIN_FC: String; //Indicador da situação do saldo final: D - Devedor; C - Credor.
    fVL_SLD_FIN_SOC: Currency; //Valor do saldo final societário. Calculado conforme - Cálculo do Saldo Final Societário.
    fIND_DC_FIN_SOC: String; //Indicador da situação do saldo Final societário: D - Devedor; C - Credor.

    public
    property COD_CTA: String read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: String read fCOD_CCUS write fCOD_CCUS;
    property COD_CTA_REF: String read fCOD_CTA_REF write fCOD_CTA_REF;
    property VL_SLD_INI_SOC_ANT: Currency read fVL_SLD_INI_SOC_ANT write fVL_SLD_INI_SOC_ANT;
    property IND_DC_INI_SOC_ANT: String read fIND_DC_INI_SOC_ANT write fIND_DC_INI_SOC_ANT;
    property VL_IS_DEB: Currency read fVL_IS_DEB write fVL_IS_DEB;
    property VL_IS_CRED: Currency read fVL_IS_CRED write fVL_IS_CRED;
    property VL_SLD_INI_SOC: Currency read fVL_SLD_INI_SOC write fVL_SLD_INI_SOC;
    property IND_DC_INI_SOC: String read fIND_DC_INI_SOC write fIND_DC_INI_SOC;
    property VL_SLD_INI_FC_ANT: Currency read fVL_SLD_INI_FC_ANT write fVL_SLD_INI_FC_ANT;
    property IND_DC_INI_FC_ANT: String read fIND_DC_INI_FC_ANT write fIND_DC_INI_FC_ANT;
    property VL_IF_DEB: Currency read fVL_IF_DEB write fVL_IF_DEB;
    property VL_IF_CRED: Currency read fVL_IF_CRED write fVL_IF_CRED;
    property VL_SLD_INI_FC: Currency read fVL_SLD_INI_FC write fVL_SLD_INI_FC;
    property IND_DC_INI_FC: String read fIND_DC_INI_FC write fIND_DC_INI_FC;
    property VL_DEB_CONTABIL: Currency read fVL_DEB_CONTABIL write fVL_DEB_CONTABIL;
    property VL_CRED_CONTABIL: Currency read fVL_CRED_CONTABIL write fVL_CRED_CONTABIL;
    property VL_DEB_FCONT_E: Currency read fVL_DEB_FCONT_E write fVL_DEB_FCONT_E;
    property VL_CRED_FCONT_E: Currency read fVL_CRED_FCONT_E write fVL_CRED_FCONT_E;
    property VL_DEB_FCONT_I: Currency read fVL_DEB_FCONT_I write fVL_DEB_FCONT_I;
    property VL_CRED_FCONT_I: Currency read fVL_CRED_FCONT_I write fVL_CRED_FCONT_I;
    property VL_TR_DEB: Currency read fVL_TR_DEB write fVL_TR_DEB;
    property VL_TR_CRED: Currency read fVL_TR_CRED write fVL_TR_CRED;
    property VL_TF_DEB: Currency read fVL_TF_DEB write fVL_TF_DEB;
    property VL_TF_CRED: Currency read fVL_TF_CRED write fVL_TF_CRED;
    property VL_TS_DEB: Currency read fVL_TS_DEB write fVL_TS_DEB;
    property VL_TS_CRED: Currency read fVL_TS_CRED write fVL_TS_CRED;
    property VL_EF_DEB: Currency read fVL_EF_DEB write fVL_EF_DEB;
    property VL_EF_CRED: Currency read fVL_EF_CRED write fVL_EF_CRED;
    property VL_SLD_FIN_FC: Currency read fVL_SLD_FIN_FC write fVL_SLD_FIN_FC;
    property IND_DC_FIN_FC: String read fIND_DC_FIN_FC write fIND_DC_FIN_FC;
    property VL_SLD_FIN_SOC: Currency read fVL_SLD_FIN_SOC write fVL_SLD_FIN_SOC;
    property IND_DC_FIN_SOC: String read fIND_DC_FIN_SOC write fIND_DC_FIN_SOC;
  end;

  /// Registro M155 - Lista

  TRegistroM155List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM155;
    procedure SetItem(Index: Integer; const Value: TRegistroM155);
  public
    function New: TRegistroM155;
    property Items[Index: Integer]: TRegistroM155 read GetItem write SetItem;
  end;


  TRegistroM355 = class
  private
    fCOD_CTA: String;    /// Código da conta analítica. Recuperado do Registro I355
    fCOD_CCUS: String;       /// Código do centro de custos. Recuperado do Registro I355.
    fCOD_CTA_REF: String;    /// Código da conta no plano de contas referencial, conforme tabela publicada pelos órgãos indicados no campo COD_ENT_REF 
    fVL_SLD_FIN_SOC: Currency; //Valor do saldo final societário. Calculado conforme - Cálculo do Saldo Societário Final - M355.
    fIND_DC_FIN_SOC: String; //Indicador da situação do saldo inicial societário: D - Devedor; C - Credor.
    fVL_DEB_FCONT_E: Currency; //Valor dos lançamentos de expurgos devedores. Calculado conforme - Cálculo dos Lançamentos - M355.
    fVL_CRED_FCONT_E: Currency; //Valor dos lançamentos de expurgos credores. Calculado conforme - Cálculo dos Lançamentos - M355.
    fVL_DEB_FCONT_I: Currency; //Valor dos lançamentos de inclusão devedores. Calculado conforme - Cálculo dos Lançamentos - M355.
    fVL_CRED_FCONT_I: Currency; //Valor dos lançamentos de inclusão credores. Calculado conforme - Cálculo dos Lançamentos - M355.
    fVL_SLD_FIN_FC_AL: Currency; //Valor do saldo referencial fiscal final. Calculado conforme - Cálculo do Saldo Fiscal Final após Lançamentos - M355.
    fIND_DC_FIN_FC_AL: String; //Indicador da situação do saldo final: D - Devedor; C - Credor.

  public
    property COD_CTA: String read fCOD_CTA write fCOD_CTA;
    property COD_CCUS: String read fCOD_CCUS write fCOD_CCUS;
    property COD_CTA_REF: String read fCOD_CTA_REF write fCOD_CTA_REF;
    property VL_SLD_FIN_SOC: Currency read fVL_SLD_FIN_SOC write fVL_SLD_FIN_SOC;
    property IND_DC_FIN_SOC: String read fIND_DC_FIN_SOC write fIND_DC_FIN_SOC;
    property VL_DEB_FCONT_E: Currency read fVL_DEB_FCONT_E write fVL_DEB_FCONT_E;
    property VL_CRED_FCONT_E: Currency read fVL_CRED_FCONT_E write fVL_CRED_FCONT_E;
    property VL_DEB_FCONT_I: Currency read fVL_DEB_FCONT_I write fVL_DEB_FCONT_I;
    property VL_CRED_FCONT_I: Currency read fVL_CRED_FCONT_I write fVL_CRED_FCONT_I;
    property VL_SLD_FIN_FC_AL: Currency read fVL_SLD_FIN_FC_AL write fVL_SLD_FIN_FC_AL;
    property IND_DC_FIN_FC_AL: String read fIND_DC_FIN_FC_AL write fIND_DC_FIN_FC_AL;
  end;

  TRegistroM355List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroM355;
    procedure SetItem(Index: Integer; const Value: TRegistroM355);
  public
    function New: TRegistroM355;
    property Items[Index: Integer]: TRegistroM355 read GetItem write SetItem;
  end;



  /// Registro M990 - ENCERRAMENTO DO BLOCO M

  TRegistroM990 = class
  private
    fQTD_LIN_M: Integer;    /// Quantidade total de linhas do Bloco M
  public
    property QTD_LIN_M: Integer read FQTD_LIN_M write FQTD_LIN_M;
  end;

implementation


{ TRegistroM025List }

function TRegistroM025List.GetItem(Index: Integer): TRegistroM025;
begin
  Result := TRegistroM025(Inherited Items[Index]);
end;

function TRegistroM025List.New: TRegistroM025;
begin
  Result := TRegistroM025.Create;
  Add(Result);
end;

procedure TRegistroM025List.SetItem(Index: Integer; const Value: TRegistroM025);
begin
  Put(Index, Value);
end;


{ TRegistroI030 }

constructor TRegistroM030.Create;
begin
   FRegistroM155 := TRegistroM155List.Create;
   FRegistroM355 := TRegistroM355List.Create;
end;

destructor TRegistroM030.Destroy;
begin
  FRegistroM155.Free;
  FRegistroM355.Free;
  inherited;
end;


{ TRegistroM030List }

function TRegistroM030List.GetItem(Index: Integer): TRegistroM030;
begin
  Result := TRegistroM030(Inherited Items[Index]);
end;

function TRegistroM030List.New: TRegistroM030;
begin
  Result := TRegistroM030.Create;
  Add(Result);
end;

procedure TRegistroM030List.SetItem(Index: Integer; const Value: TRegistroM030);
begin
  Put(Index, Value);
end;


{ TRegistroM155List }

function TRegistroM155List.GetItem(Index: Integer): TRegistroM155;
begin
  Result := TRegistroM155(Inherited Items[Index]);
end;

function TRegistroM155List.New: TRegistroM155;
begin
  Result := TRegistroM155.Create;
  Add(Result);
end;

procedure TRegistroM155List.SetItem(Index: Integer; const Value: TRegistroM155);
begin
  Put(Index, Value);
end;

{ TRegistroM355List }

function TRegistroM355List.GetItem(Index: Integer): TRegistroM355;
begin
  Result := TRegistroM355(Inherited Items[Index]);
end;

function TRegistroM355List.New: TRegistroM355;
begin
  Result := TRegistroM355.Create;
  Add(Result);
end;

procedure TRegistroM355List.SetItem(Index: Integer; const Value: TRegistroM355);
begin
  Put(Index, Value);
end;


end.
