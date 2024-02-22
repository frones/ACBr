{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti e Isaque Pinheiro            }
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

 {$I ACBr.inc}

unit ACBrECFBloco_0;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  TRegistro0010 = class;
  TRegistro0020 = class;
  TRegistro0021 = class;
  TRegistro0030 = class;
  TRegistro0035List = class;
  TRegistro0930List = class;

  { TRegistro0000 }

  TRegistro0000 = class(TBlocos)
  private
    fNOME_ESC: string;          // Identificação do Tipo de Sped. Texto fixo contendo “LECF”.
    fCOD_VER:  TACBrECFCodVer;  // Código da Versão do Leiaute Código da versão do leiaute conforme ato da RFB.
    fCNPJ:     string;          // CNPJ Número de inscrição da pessoa jurídica no CNPJ.
    fNOME:     string;          // Nome empresarial Nome empresarial da jurídica ou da SCP.
    fIND_SIT_INI_PER: string;   // Indicador do início do período Indicador do início do período:
                                // 0 – Regular (início no primeiro dia do ano)
                                // 1 – Abertura (início de atividades no ano calendário)
                                // 2 – Resultante de cisão/fusão ou remanescente de cisão, ou realizou    incorporação
                                // 3 – Resultante de Transformação
                                // 4 – Início de obrigatoriedade da entrega no curso do ano calendário.
                                // (Ex. Exclusão do Simples Nacional ou desenquadramento como imune ou isenta do IRPJ)
    fSIT_ESPECIAL: string;      // Indicador de situação especial e  outros eventos
                                // 0 – Normal (Sem ocorrência de situação especial ou evento)
                                // 1 – Extinção
                                // 2 – Fusão
                                // 3 – Incorporação \ Incorporada
                                // 4 – Incorporação \ Incorporadora
                                // 5 – Cisão Total
                                // 6 – Cisão Parcial
                                // 7 – Transformação
                                // 8 – Desenquadramento de Imune/Isenta;
                                // 9 – Inclusão no Simples Naciona
    fPAT_REMAN_CIS: Double;     // Patrimônio Remanescente Em Caso De Cisão (%)
    fDT_SIT_ESP: TDateTime;     // Data da situação especial ou evento.
    fDT_INI:   TDateTime;       // Data inicial das informações contidas no arquivo.
    fDT_FIN:   TDateTime;       // Data Final Data final das informações contidas no arquivo.
    fRETIFICADORA: string;      // Escrituração retificadora? Escrituração retificadora?
                                // “S” – ECF retificadora
                                // “N” – ECF original
    fNUM_REC:  string;          // Número do Recibo Anterior
    fTIP_ECF:  string;          // Tipo da ECF Indicador do tipo da ECF:
                                // 0 – ECF de empresa não participante de SCP como sócio ostensivo
                                // 1 – ECF de empresa participante de SCP como sócio ostensivo
                                // 2 – ECF da SCP
    fCOD_SCP:  string;          // Identificação da SCP Identificação da SCP.
                                // Obs.: Só deve ser preenchido pela própria SCP (não é preenchido pelo sócio ostensivo).
  public
    constructor Create;
    property NOME_ESC: string read FNOME_ESC;
    property COD_VER: TACBrECFCodVer read FCOD_VER write FCOD_VER;
    property CNPJ: string read FCNPJ write FCNPJ;
    property NOME: string read FNOME write FNOME;
    property IND_SIT_INI_PER: string read FIND_SIT_INI_PER write FIND_SIT_INI_PER;
    property SIT_ESPECIAL: string read FSIT_ESPECIAL write FSIT_ESPECIAL;
    property PAT_REMAN_CIS: Double read FPAT_REMAN_CIS write FPAT_REMAN_CIS;
    property DT_SIT_ESP: TDateTime read FDT_SIT_ESP write FDT_SIT_ESP;
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
    property RETIFICADORA: string read FRETIFICADORA write FRETIFICADORA;
    property NUM_REC: string read FNUM_REC write FNUM_REC;
    property TIP_ECF: string read FTIP_ECF write FTIP_ECF;
    property COD_SCP: string read FCOD_SCP write FCOD_SCP;
  end;

  { TRegistro0001 }

  TRegistro0001 = class(TOpenBlocos)
  private
    fRegistro0010: TRegistro0010;
    fRegistro0020: TRegistro0020;
    fRegistro0021: TRegistro0021;
    fRegistro0030: TRegistro0030;
    fRegistro0035: TRegistro0035List;
    fRegistro0930: TRegistro0930List;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Registro0010 : TRegistro0010 read fRegistro0010 write fRegistro0010;
    property Registro0020 : TRegistro0020 read fRegistro0020 write fRegistro0020;
    property Registro0021 : TRegistro0021 read fRegistro0021 write fRegistro0021;
    property Registro0030 : TRegistro0030 read fRegistro0030 write fRegistro0030;
    property Registro0035 : TRegistro0035List read fRegistro0035 write fRegistro0035;
    property Registro0930 : TRegistro0930List read fRegistro0930 write fRegistro0930;
  end;

  { TRegistro0010 }

  TRegistro0010 = class(TBlocos)
  private
    fAPUR_CSLL:   string;
    fCOD_QUALIF_PJ: String;
    fDIF_FCONT:   string;
    fFORMA_APUR:  string;
    fFORMA_APUR_I: string;
    fFORMA_TRIB:  TACBrFormaTributacaoLucro;
    fFORMA_TRIB_PER: string;
    fHASH_ECF_ANTERIOR: string;
    fMES_BAL_RED: string;
    fOPT_EXT_RTT: string;
    fOPT_PAES:    TACBrIndicador;
    fOPT_REFIS:   TACBrIndicador;
    fTIP_ENT:     String;
    fTIP_ESC_PRE: string;
    fIND_REC_RECEITA: TACBrIndRecReceita;
  public
    property HASH_ECF_ANTERIOR: string read fHASH_ECF_ANTERIOR write fHASH_ECF_ANTERIOR;
    property OPT_REFIS: TACBrIndicador read fOPT_REFIS write fOPT_REFIS;
    property OPT_PAES: TACBrIndicador read fOPT_PAES write fOPT_PAES;
    property FORMA_TRIB: TACBrFormaTributacaoLucro read fFORMA_TRIB write fFORMA_TRIB;
    property FORMA_APUR: string read fFORMA_APUR write fFORMA_APUR;
    property COD_QUALIF_PJ: String read fCOD_QUALIF_PJ write fCOD_QUALIF_PJ;
    property FORMA_TRIB_PER: string read fFORMA_TRIB_PER write fFORMA_TRIB_PER;
    property MES_BAL_RED: string read fMES_BAL_RED write fMES_BAL_RED;
    property TIP_ESC_PRE: string read fTIP_ESC_PRE write fTIP_ESC_PRE;
    property TIP_ENT: String read fTIP_ENT write fTIP_ENT;
    property FORMA_APUR_I: string read fFORMA_APUR_I write fFORMA_APUR_I;
    property APUR_CSLL: string read fAPUR_CSLL write fAPUR_CSLL;
    property OPT_EXT_RTT: string read fOPT_EXT_RTT write fOPT_EXT_RTT;
    property DIF_FCONT: string read fDIF_FCONT write fDIF_FCONT;
    property IND_REC_RECEITA: TACBrIndRecReceita read fIND_REC_RECEITA write fIND_REC_RECEITA;
  end;

  { TRegistro0020 }

  TRegistro0020 = class(TBlocos)
  private
    fIND_ADM_FUN_CLU: string;
    fIND_ALIQ_CSLL: string;
    fIND_AREA_COM: string;
    fIND_ATIV_EXT: string;
    fIND_ATIV_RURAL: string;
    fIND_CAP_INF:  string;
    fIND_COM_EXP:  string;
    fIND_DOA_ELEIT: string;
    fIND_ECOM_TI:  string;
    fIND_FIN:      string;
    fIND_INOV_TEC: string;
    fIND_LUC_EXP:  string;
    fIND_OP_EXT:   string;
    fIND_OP_VINC:  string;
    fIND_PART_COLIG: string;
    fIND_PART_CONS: string;
    fIND_PART_EXT: string;
    fIND_PGTO_EXT: string;
    fIND_PGTO_REM: string;
    fIND_PJ_ENQUAD: String;
    fIND_PJ_HAB:   string;
    fIND_POLO_AM:  string;
    fIND_QTE_SCP:  integer;
    fIND_REC_EXT:  string;
    fIND_RED_ISEN: string;
    fIND_REND_SERV: string;
    fIND_ROY_PAG:  string;
    fIND_ROY_REC:  string;
    fIND_VEND_EXP: string;
    fIND_ZON_EXP:  string;
    fIND_PAIS_A_PAIS: string;
    fIND_DEREX: string;
    fIND_PR_TRANSF: string;
  public
    property IND_ALIQ_CSLL: string read fIND_ALIQ_CSLL write fIND_ALIQ_CSLL;
    property IND_QTE_SCP: integer read fIND_QTE_SCP write fIND_QTE_SCP;
    property IND_ADM_FUN_CLU: string read fIND_ADM_FUN_CLU write fIND_ADM_FUN_CLU;
    property IND_PART_CONS: string read fIND_PART_CONS write fIND_PART_CONS;
    property IND_OP_EXT: string read fIND_OP_EXT write fIND_OP_EXT;
    property IND_OP_VINC: string read fIND_OP_VINC write fIND_OP_VINC;
    property IND_PJ_ENQUAD: String read fIND_PJ_ENQUAD write fIND_PJ_ENQUAD;
    property IND_PART_EXT: string read fIND_PART_EXT write fIND_PART_EXT;
    property IND_ATIV_RURAL: string read fIND_ATIV_RURAL write fIND_ATIV_RURAL;
    property IND_LUC_EXP: string read fIND_LUC_EXP write fIND_LUC_EXP;
    property IND_RED_ISEN: string read fIND_RED_ISEN write fIND_RED_ISEN;
    property IND_FIN: string read fIND_FIN write fIND_FIN;
    property IND_DOA_ELEIT: string read fIND_DOA_ELEIT write fIND_DOA_ELEIT;
    property IND_PART_COLIG: string read fIND_PART_COLIG write fIND_PART_COLIG;
    property IND_VEND_EXP: string read fIND_VEND_EXP write fIND_VEND_EXP;
    property IND_REC_EXT: string read fIND_REC_EXT write fIND_REC_EXT;
    property IND_ATIV_EXT: string read fIND_ATIV_EXT write fIND_ATIV_EXT;
    property IND_COM_EXP: string read fIND_COM_EXP write fIND_COM_EXP;
    property IND_PGTO_EXT: string read fIND_PGTO_EXT write fIND_PGTO_EXT;
    property IND_ECOM_TI: string read fIND_ECOM_TI write fIND_ECOM_TI;
    property IND_ROY_REC: string read fIND_ROY_REC write fIND_ROY_REC;
    property IND_ROY_PAG: string read fIND_ROY_PAG write fIND_ROY_PAG;
    property IND_REND_SERV: string read fIND_REND_SERV write fIND_REND_SERV;
    property IND_PGTO_REM: string read fIND_PGTO_REM write fIND_PGTO_REM;
    property IND_INOV_TEC: string read fIND_INOV_TEC write fIND_INOV_TEC;
    property IND_CAP_INF: string read fIND_CAP_INF write fIND_CAP_INF;
    property IND_PJ_HAB: string read fIND_PJ_HAB write fIND_PJ_HAB;
    property IND_POLO_AM: string read fIND_POLO_AM write fIND_POLO_AM;
    property IND_ZON_EXP: string read fIND_ZON_EXP write fIND_ZON_EXP;
    property IND_AREA_COM: string read fIND_AREA_COM write fIND_AREA_COM;
    property IND_PAIS_A_PAIS: string read fIND_PAIS_A_PAIS write fIND_PAIS_A_PAIS;
    property IND_DEREX: string read fIND_DEREX write fIND_DEREX;
    property IND_PR_TRANSF: string read fIND_PR_TRANSF write fIND_PR_TRANSF;
  end;

  { TRegistro0021 }

  TRegistro0021 = class(TBlocos)
  private
    fIND_REPES: string;
    fIND_RECAP: string;
    fIND_PADIS: string;
    fIND_PATVD: string;
    fIND_REIDI: string;
    fIND_REPENEC: string;
    fIND_REICOMP: string;
    fIND_RETAERO: string;
    fIND_RECINE: string;
    fIND_RESIDUOS_SOLIDOS: string;
    fIND_RECOPA: string;
    fIND_COPA_DO_MUNDO: string;
    fIND_RETID: string;
    fIND_REPNBL_REDES: string;
    fIND_REIF: string;
    fIND_OLIMPIADAS: string;
  public
    property IND_REPES: string read fIND_REPES write fIND_REPES;
    property IND_RECAP: string read fIND_RECAP write fIND_RECAP;
    property IND_PADIS: string read fIND_PADIS write fIND_PADIS;
    property IND_PATVD: string read fIND_PATVD write fIND_PATVD;
    property IND_REIDI: string read fIND_REIDI write fIND_REIDI;
    property IND_REPENEC: string read fIND_REPENEC write fIND_REPENEC;
    property IND_REICOMP: string read fIND_REICOMP write fIND_REICOMP;
    property IND_RETAERO: string read fIND_RETAERO write fIND_RETAERO;
    property IND_RECINE: string read fIND_RECINE write fIND_RECINE;
    property IND_RESIDUOS_SOLIDOS: string read fIND_RESIDUOS_SOLIDOS write fIND_RESIDUOS_SOLIDOS;
    property IND_RECOPA: string read fIND_RECOPA write fIND_RECOPA;
    property IND_COPA_DO_MUNDO: string read fIND_COPA_DO_MUNDO write fIND_COPA_DO_MUNDO;
    property IND_RETID: string read fIND_RETID write fIND_RETID;
    property IND_REPNBL_REDES: string read fIND_REPNBL_REDES write fIND_REPNBL_REDES;
    property IND_REIF: string read fIND_REIF write fIND_REIF;
    property IND_OLIMPIADAS: string read fIND_OLIMPIADAS write fIND_OLIMPIADAS;
  end;

  { TRegistro0030 }

  TRegistro0030 = class(TBlocos)
  private
    fBAIRRO: string;
    fCEP: string;
    fCNAE_FISCAL: String;
    fCOD_MUN: string;
    fCOD_NAT: string;
    fCOMPL: string;
    fEMAIL: string;
    fENDERECO: string;
    fNUM: string;
    fNUM_TEL: string;
    fUF: string;
  public
    property COD_NAT: String read fCOD_NAT write fCOD_NAT;
    property CNAE_FISCAL: String read fCNAE_FISCAL write fCNAE_FISCAL;
    property ENDERECO: string read fENDERECO write fENDERECO;
    property NUM: string read fNUM write fNUM;
    property COMPL: string read fCOMPL write fCOMPL;
    property BAIRRO: string read fBAIRRO write fBAIRRO;
    property UF: string read fUF write fUF;
    property COD_MUN: string read fCOD_MUN write fCOD_MUN;
    property CEP: string read fCEP write fCEP;
    property NUM_TEL: string read fNUM_TEL write fNUM_TEL;
    property EMAIL: string read fEMAIL write fEMAIL;
  end;

  { TRegistro0035 }

  TRegistro0035 = class(TBlocos)
  private
    fCOD_SCP:  string;
    fNOME_SCP: string;
  public
    property COD_SCP: string read fCOD_SCP write fCOD_SCP;
    property NOME_SCP: string read fNOME_SCP write fNOME_SCP;
  end;

  { TRegistro0035List }

  TRegistro0035List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0035;
    procedure SetItem(Index: Integer; const Value: TRegistro0035);
  public
    function New(): TRegistro0035;
    property Items[Index: Integer]: TRegistro0035 read GetItem write SetItem;
  end;
  { TRegistro0930 }

  TRegistro0930 = class(TBlocos)
  private
    fEMAIL:     string;
    fFONE:      string;
    fIDENT_CPF_CNPJ: string;
    fIDENT_NOM: string;
    fIDENT_QUALIF: TACBrQualificacaoAssinante;
    fIND_CRC:   string;
  public
    property IDENT_NOM: string read fIDENT_NOM write fIDENT_NOM;
    property IDENT_CPF_CNPJ: string read fIDENT_CPF_CNPJ write fIDENT_CPF_CNPJ;
    property IDENT_QUALIF: TACBrQualificacaoAssinante read fIDENT_QUALIF write fIDENT_QUALIF;
    property IND_CRC: string read fIND_CRC write fIND_CRC;
    property EMAIL: string read fEMAIL write fEMAIL;
    property FONE: string read fFONE write fFONE;
  end;

  { TRegistro0930List }

  TRegistro0930List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0930;
    procedure SetItem(Index: Integer; const Value: TRegistro0930);
  public
    function New(): TRegistro0930;
    property Items[Index: Integer]: TRegistro0930 read GetItem write SetItem;
  end;

  /// Registro 0990 - ENCERRAMENTO DO BLOCO 0
  TRegistro0990 = class(TCloseBlocos)
  private
    FQTD_LIN_0: integer; //Quantidade total de linhas do Bloco 0
  public
    property QTD_LIN_0: integer read FQTD_LIN_0 write FQTD_LIN_0;
  end;

implementation

{ TRegistro0930List }

function TRegistro0930List.GetItem(Index: Integer): TRegistro0930;
begin
  Result := TRegistro0930(Inherited Items[Index]);
end;

procedure TRegistro0930List.SetItem(Index: Integer; const Value: TRegistro0930);
begin
  Put(Index, Value);
end;

function TRegistro0930List.New(): TRegistro0930;
begin
  Result := TRegistro0930.Create;
  Add(Result);
end;

{ TRegistro0035List }

function TRegistro0035List.GetItem(Index: Integer): TRegistro0035;
begin
  Result := TRegistro0035(Inherited Items[Index]);
end;

procedure TRegistro0035List.SetItem(Index: Integer; const Value: TRegistro0035);
begin
  Put(Index, Value);
end;

function TRegistro0035List.New(): TRegistro0035;
begin
  Result := TRegistro0035.Create;
  Add(Result);
end;

{ TRegistro0001 }

constructor TRegistro0001.Create;
begin
  inherited;
  fRegistro0010 := TRegistro0010.Create;
  fRegistro0020 := TRegistro0020.Create;
  fRegistro0021 := TRegistro0021.Create;
  fRegistro0030 := TRegistro0030.Create;
  fRegistro0035 := TRegistro0035List.Create;
  fRegistro0930 := TRegistro0930List.create;
end;

destructor TRegistro0001.Destroy;
begin
  fRegistro0010.Free;
  fRegistro0020.Free;
  fRegistro0021.Free;
  fRegistro0030.Free;
  fRegistro0035.Free;
  fRegistro0930.Free;
  inherited;
end;

{ TRegistro0000 }

constructor TRegistro0000.Create;
begin
  inherited Create;
  fNOME_ESC := 'LECF';
end;

end.
