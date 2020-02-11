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

unit ACBrECDBloco_0;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECDBlocos;

type

  TRegistro0180List = class;

  /// Registro 0000 - ABERTURA  DO  ARQUIVO  DIGITAL  E  IDENTIFICAÇÃO  DO
  ///                 EMPRESÁRIO OU DA SOCIEDADE EMPRESÁRIA

  TRegistro0000 = class
  private
    fDT_INI: TDateTime;       /// Data inicial das informações contidas no arquivo
    fDT_FIN: TDateTime;       /// Data final das informações contidas no arquivo
    fNOME: String;        /// Nome empresarial do empresário ou sociedade empresária.
    fCNPJ: String;        /// Número de inscrição do empresário ou sociedade empresária no CNPJ.
    fUF: String;          /// Sigla da unidade da federação do empresário ou sociedade empresária.
    fIE: String;          /// Inscrição Estadual do empresário ou sociedade empresária.
    fCOD_MUN: String;     /// Código do município do domicílio fiscal do empresário ou sociedade empresária, conforme tabela do IBGE - Instituto Brasileiro de Geografia e Estatística.
    fIM: String;          /// Inscrição Municipal do empresário ou sociedade empresária.
    fIND_SIT_ESP: String; /// Indicador de situação especial (conforme tabela publicada pelo Sped).
    fIND_SIT_INI_PER: String; /// Indicador de situação no início do período (conforme tabela publicada pelo Sped).
    fIND_NIRE: String;        /// Indicador de existência de NIRE
    fIND_FIN_ESC: String;     /// Indicador de finalidade da escrituração
    fCOD_HASH_SUB: String;    /// Hash da escrituração substituída.
    fNIRE_SUBST: String;      /// NIRE da escrituração substituída.
    fIND_EMP_GRD_PRT: String; /// Indicador de empresa de grande porte:
    fTIP_ECD: String;         /// Indicador do tipo de ECD: 0 – ECD de empresa não participante de SCP como sócio ostensivo. 1 – ECD de empresa participante de SCP como sócio ostensivo. 2 – ECD da SCP.
    fCOD_SCP: String;         /// Identificação da SCP.
    fIDENT_MF: String;        /// Identificação de Moeda Funcional
    fIND_ESC_CONS: string;    /// Escriturações Contábeis Consolidadas: Deve ser preenchido pela empresa controladora obrigada, nos termos da lei, a informações demonstrações contábeis consolidadas
    fIND_CENTRALIZADA: string;/// Indicador da modalidade de escrituração centralizada ou descentralizada: 0 – Escrituração Centralizada 1 – Escrituração Descentralizada
    fIND_MUDANC_PC: string;   /// Indicador de mudança de plano de contas: 0 – Não houve mudança no plano de contas. 1 – Houve mudança no plano de contas.
    fCOD_PLAN_REF: string;    /// Código do Plano de Contas Referencial que será utilizado para o mapeamento de todas as contas analíticas Observação: Caso a pessoa jurídica não realize o mapeamento para os planos referenciais na ECD, este campo deve ficar em branco.
  public
    property DT_INI: TDateTime read FDT_INI write FDT_INI;
    property DT_FIN: TDateTime read FDT_FIN write FDT_FIN;
    property NOME: String read fNOME write fNOME;
    property CNPJ: String read fCNPJ write fCNPJ;
    property UF: String read fUF write fUF;
    property IE: String read fIE write fIE;
    property COD_MUN: String read fCOD_MUN write fCOD_MUN;
    property IM: String read fIM write fIM;
    property IND_SIT_ESP: String read fIND_SIT_ESP write fIND_SIT_ESP;
    property IND_SIT_INI_PER: String read fIND_SIT_INI_PER write fIND_SIT_INI_PER;
    property IND_NIRE: String read fIND_NIRE write fIND_NIRE;
    property IND_FIN_ESC: String read fIND_FIN_ESC write fIND_FIN_ESC;
    property COD_HASH_SUB: String read fCOD_HASH_SUB write fCOD_HASH_SUB;
    property NIRE_SUBST: String read fNIRE_SUBST write fNIRE_SUBST;
    property IND_EMP_GRD_PRT: String read fIND_EMP_GRD_PRT write fIND_EMP_GRD_PRT;
    property TIP_ECD: String read fTIP_ECD write fTIP_ECD;
    property COD_SCP: String read fCOD_SCP write fCOD_SCP;
    property IDENT_MF: String read fIDENT_MF write fIDENT_MF;
    property IND_ESC_CONS: String read fIND_ESC_CONS write fIND_ESC_CONS;
    property IND_CENTRALIZADA: String read fIND_CENTRALIZADA write fIND_CENTRALIZADA;
    property IND_MUDANC_PC: String read fIND_MUDANC_PC write fIND_MUDANC_PC;
    property COD_PLAN_REF: String read fCOD_PLAN_REF write fCOD_PLAN_REF;
  end;

  /// Registro 0001 - ABERTURA DO BLOCO 0

  TRegistro0001 = class(TOpenBlocos)
  private
  public
  end;

  /// Rregistro 0007 – ESCRITURAÇÃO CONTÁBIL DESCENTRALIZADA

  TRegistro0007 = class
  private
    fCOD_ENT_REF: String; /// Código da instituição responsável pela administração do cadastro (conforme tabela publicada pelo Sped).
    fCOD_INSCR: String;   /// Código cadastral do empresário ou sociedade empresária na instituição identificada no campo 02.
  public
    property COD_ENT_REF: String read fCOD_ENT_REF write fCOD_ENT_REF;
    property COD_INSCR: String read fCOD_INSCR write fCOD_INSCR;
  end;

  /// Registro 0007 - Lista

  TRegistro0007List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0007;
    procedure SetItem(Index: Integer; const Value: TRegistro0007);
  public
    function New: TRegistro0007;
    property Items[Index: Integer]: TRegistro0007 read GetItem write SetItem;
  end;

  /// Rregistro 0020 – OUTRAS INSCRIÇÕES CADASTRAIS DO EMPRESÁRIO OU
  ///                  SOCIEDADE EMPRESÁRIA

  TRegistro0020 = class
  private
    fIND_DEC: Integer;        /// Indicador de descentralização: 0 - escrituração da matriz; 1 - escrituração da filial.
    fCNPJ: String;        /// Número de inscrição do empresário ou sociedade empresária no CNPJ da matriz ou da filial.
    fUF: String;          /// Sigla da unidade da federação da matriz ou da filial.
    fIE: String;          /// Inscrição estadual da matriz ou da filial.
    fCOD_MUN: String;     /// Código do município do domicílio da matriz ou da filial.
    fIM: String;          /// Número de Inscrição Municipal da matriz ou da filial.
    fNIRE: String;        /// Número de Identificação do Registro de Empresas da matriz ou da filial na Junta Comercial.
  public
    property IND_DEC: Integer read fIND_DEC write fIND_DEC;
    property CNPJ: String read fCNPJ write fCNPJ;
    property UF: String read fUF write fUF;
    property IE: String read fIE write fIE;
    property COD_MUN: String read fCOD_MUN write fCOD_MUN;
    property IM: String read fIM write fIM;
    property NIRE: String read fNIRE write fNIRE;
  end;

  /// Registro 0020 - Lista

  TRegistro0020List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0020;
    procedure SetItem(Index: Integer; const Value: TRegistro0020);
  public
    function New: TRegistro0020;
    property Items[Index: Integer]: TRegistro0020 read GetItem write SetItem;
  end;

  /// Rregistro 0035 – IDENTIFICAÇÃO DAS SCP

  TRegistro0035 = class
  private
    fCOD_SCP: String;   /// Identificação da SCP (CNPJ – art. 52 da Instrução Normativa RFB no 1.470, de 30 de maio de 2014)
    fNOME_SCP: String;  /// Nome da SCP
  public
    property COD_SCP: String read fCOD_SCP write fCOD_SCP;
    property NOME_SCP: String read fNOME_SCP write fNOME_SCP;
  end;

  /// Registro 0035 - Lista

  TRegistro0035List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0035;
    procedure SetItem(Index: Integer; const Value: TRegistro0035);
  public
    function New: TRegistro0035;
    property Items[Index: Integer]: TRegistro0035 read GetItem write SetItem;
  end;

  /// Rregistro 0150 –  TABELA DE CADASTRO DO PARTICIPANTE

  TRegistro0150 = class
  private
    fCOD_PART: String;    /// Código de identificação do participante:
    fNOME: String;        /// Nome pessoal ou empresarial:
    fCOD_PAIS: String;    /// Código do país do participante:
    fCNPJ: String;        /// CNPJ do participante:
    fCPF: String;         /// CPF do participante na unidade da federação do destinatário:
    fNIT: String;         /// Número de Identificação do Trabalhador, Pis, Pasep, SUS.
    fUF: String;          /// Sigla da unidade da federação do participante.
    fIE: String;          /// Inscrição Estadual do participante:
    fIE_ST: String;       /// Inscrição Estadual do participante na unidade da federação do destinatário, na condição de contribuinte substituto
    fCOD_MUN: integer;        /// Código do município:
    fIM: String;          /// Inscrição Municipal do participante.
    fSUFRAMA: String;     /// Número de inscrição na Suframa:
    FRegistro0180: TRegistro0180List; /// BLOCO 0 - Lista de Registro0180 (FILHO)
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property COD_PART: String read fCOD_PART write fCOD_PART;
    property NOME: String read fNOME write fNOME;
    property COD_PAIS: String read fCOD_PAIS write fCOD_PAIS;
    property CNPJ: String read fCNPJ write fCNPJ;
    property CPF: String read fCPF write fCPF;
    property NIT: String read fNIT write fNIT;
    property UF: String read fUF write fUF;
    property IE: String read fIE write fIE;
    property IE_ST: String read fIE_ST write fIE_ST;
    property COD_MUN: integer read fCOD_MUN write fCOD_MUN;
    property IM: String read fIM write fIM;
    property SUFRAMA: String read fSUFRAMA write fSUFRAMA;
    property Registro0180: TRegistro0180List read FRegistro0180 write FRegistro0180;
  end;

  /// Registro 0150 - Lista

  TRegistro0150List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0150;
    procedure SetItem(Index: Integer; const Value: TRegistro0150);
  public
    function New: TRegistro0150;
    function LocalizaRegistro(const pCOD_PART: String): boolean;
    property Items[Index: Integer]: TRegistro0150 read GetItem write SetItem;
  end;

  /// Registro 0180 - IDENTIFICAÇÃO DO RELACIONAMENTO COM O PARTICIPANTE

  TRegistro0180 = class
  private
    fCOD_REL: String;     /// Código do relacionamento conforme tabela publicada pelo Sped.
    fDT_INI_REL: TDateTime;   /// Data do início do relacionamento.
    fDT_FIN_REL: TDateTime;   /// Data do término do relacionamento.
  public
    property COD_REL: String read fCOD_REL write fCOD_REL;
    property DT_INI_REL: TDateTime read fDT_INI_REL write fDT_INI_REL;
    property DT_FIN_REL: TDateTime read fDT_FIN_REL write fDT_FIN_REL;
  end;

  /// Registro 0180 - Lista

  TRegistro0180List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0180;
    procedure SetItem(Index: Integer; const Value: TRegistro0180);
  public
    function New: TRegistro0180;
    property Items[Index: Integer]: TRegistro0180 read GetItem write SetItem;
  end;

  /// Registro 0990 - ENCERRAMENTO DO BLOCO 0

  TRegistro0990 = class
  private
    fQTD_LIN_0: Integer; /// Quantidade total de linhas do Bloco 0
  public
    property QTD_LIN_0: Integer read fQTD_LIN_0 write fQTD_LIN_0;
  end;

implementation

{ TRegistro0007List }

function TRegistro0007List.GetItem(Index: Integer): TRegistro0007;
begin
  Result := TRegistro0007(Inherited Items[Index]);
end;

function TRegistro0007List.New: TRegistro0007;
begin
  Result := TRegistro0007.Create;
  Add(Result);
end;

procedure TRegistro0007List.SetItem(Index: Integer; const Value: TRegistro0007);
begin
  Put(Index, Value);
end;

{ TRegistro0020List }

function TRegistro0020List.GetItem(Index: Integer): TRegistro0020;
begin
  Result := TRegistro0020(Inherited Items[Index]);
end;

function TRegistro0020List.New: TRegistro0020;
begin
  Result := TRegistro0020.Create;
  Add(Result);
end;

procedure TRegistro0020List.SetItem(Index: Integer; const Value: TRegistro0020);
begin
  Put(Index, Value);
end;

{ TRegistro0150List }

function TRegistro0150List.GetItem(Index: Integer): TRegistro0150;
begin
  Result := TRegistro0150(Inherited Items[Index]);
end;

function TRegistro0150List.LocalizaRegistro(const pCOD_PART: String): boolean;
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

function TRegistro0150List.New: TRegistro0150;
begin
  Result := TRegistro0150.Create;
  Add(Result);
end;

procedure TRegistro0150List.SetItem(Index: Integer; const Value: TRegistro0150);
begin
  Put(Index, Value);
end;

{ TRegistro0180List }

function TRegistro0180List.GetItem(Index: Integer): TRegistro0180;
begin
  Result := TRegistro0180(Inherited Items[Index]);
end;

function TRegistro0180List.New: TRegistro0180;
begin
  Result := TRegistro0180.Create;
  Add(Result);
end;

procedure TRegistro0180List.SetItem(Index: Integer; const Value: TRegistro0180);
begin
  Put(Index, Value);
end;

{ TRegistro0035List }

function TRegistro0035List.GetItem(Index: Integer): TRegistro0035;
begin
  Result := TRegistro0035(Inherited Items[Index]);
end;

function TRegistro0035List.New: TRegistro0035;
begin
  Result := TRegistro0035.Create;
  Add(Result);
end;

procedure TRegistro0035List.SetItem(Index: Integer; const Value: TRegistro0035);
begin
  Put(Index, Value);
end;

{ TRegistro0150 }

constructor TRegistro0150.Create;
begin
FRegistro0180 := TRegistro0180List.Create;
end;

destructor TRegistro0150.Destroy;
begin
  FRegistro0180.Free;
inherited;
end;

end.
