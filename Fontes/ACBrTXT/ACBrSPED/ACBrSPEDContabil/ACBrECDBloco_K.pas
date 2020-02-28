{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro e Renato Rubinho                }
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

unit ACBrECDBloco_K;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECDBlocos;

type
  TRegistroK100List = class;
  TRegistroK110List = class;
  TRegistroK115List = class;
  TRegistroK200List = class;
  TRegistroK210List = class;
  TRegistroK300List = class;
  TRegistroK310List = class;
  TRegistroK315List = class;

  /// Registro K001 - ABERTURA DO BLOCO K

  TRegistroK001 = class(TOpenBlocos)
  private
  public
  end;

  /// Rregistro K030 – PERIODO DA ESCRITURACAO CONTABIL CONSOLIDADA
  TRegistroK030 = class
  private
    fDT_INI: TDateTime;     /// Data de inicio da escrituração.
    fDT_FIN: TDateTime;     /// Data de término da escrituração.

    FRegistroK100: TRegistroK100List;  /// BLOCO K - Lista de RegistroK100 (FILHO)
    FRegistroK200: TRegistroK200List;  /// BLOCO K - Lista de RegistroK200 (FILHO)
    FRegistroK300: TRegistroK300List;  /// BLOCO K - Lista de RegistroK300 (FILHO)
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy
    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    /// Registros FILHOS
    property RegistroK100: TRegistroK100List read FRegistroK100 write FRegistroK100;
    property RegistroK200: TRegistroK200List read FRegistroK200 write FRegistroK200;
    property RegistroK300: TRegistroK300List read FRegistroK300 write FRegistroK300;
  end;

  /// Registro K100 – DEMONSTRAÇÃO DO RESULTADO DO EXERCÍCIO

  TRegistroK100 = class
  private
    fCOD_PAIS: String;         /// Código do país da empresa, conforme tabela do Banco Central do Brasil.
    fEMP_COD: String;          /// Código de identificação da empresa participante.
    fCNPJ: String;             /// CNPJ(somente os 8 primeiros dígitos).
    fNOME: String;             /// Nome empresarial.
    fPER_PART: Currency;       /// Percentual de participação total do conglomerado na empresa no final do período consolidado: Informar a participação acionária.
    fEVENTO: String;           /// Evento societário ocorrido no período: S - Sim N – Não
    fPER_CONS: Currency;       /// Percentual de consolidação da empresa no final do período consolidado: Informar o percentual do resultado da empresa que foi para a consolidação.
    fDATA_INI_EMP: TDateTime;  /// Data inicial do período da escrituração contábil da empresa que foi consolidada.
    fDATA_FIN_EMP: TDateTime;  /// Data final do período da escrituração contábil da empresa que foi consolidada
    ///
    FRegistroK110: TRegistroK110List;  /// BLOCO K - Lista de RegistroK110 (FILHO)
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_PAIS: String read fCOD_PAIS write fCOD_PAIS;
    property EMP_COD: String read fEMP_COD write fEMP_COD;
    property CNPJ: String read fCNPJ write fCNPJ;
    property NOME: String read fNOME write fNOME;
    property PER_PART: Currency read fPER_PART write fPER_PART;
    property EVENTO: String read fEVENTO write fEVENTO;
    property PER_CONS: Currency read fPER_CONS write fPER_CONS;
    property DATA_INI_EMP: TDateTime read fDATA_INI_EMP write fDATA_INI_EMP;
    property DATA_FIN_EMP: TDateTime read fDATA_FIN_EMP write fDATA_FIN_EMP;
    /// Registros FILHOS
    property RegistroK110: TRegistroK110List read FRegistroK110 write FRegistroK110;
  end;

  /// Registro K100 - Lista

  TRegistroK100List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK100;
    procedure SetItem(Index: Integer; const Value: TRegistroK100);
  public
    function New: TRegistroK100;
    function LocalizaRegistro(const pEMP_COD: String): boolean;
    property Items[Index: Integer]: TRegistroK100 read GetItem write SetItem;
  end;

  /// Registro K110 - Relação dos Eventos Societários
  TRegistroK110 = class
  private
    fEVENTO: String;         /// Evento societário ocorrido no período: 1 – Aquisição 2 – Alienação 3 – Fusão 4 – Cisão Parcial 5 – Cisão Total 6 – Incorporação 7 – Extinção 8 – Constituição
    fDT_EVENTO: TDateTime;   /// Data do evento societário

    FRegistroK115: TRegistroK115List;  /// BLOCO K - Lista de RegistroK115(FILHO)
  public
    constructor Create; virtual; /// Create

    property EVENTO: String read fEVENTO write fEVENTO;
    property DT_EVENTO: TDateTime read fDT_EVENTO write fDT_EVENTO;

    /// Registros FILHOS
    property RegistroK115: TRegistroK115List read FRegistroK115 write FRegistroK115;
  end;

  /// Registro K110 - Lista

  TRegistroK110List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK110;
    procedure SetItem(Index: Integer; const Value: TRegistroK110);
  public
    function New: TRegistroK110;
    property Items[Index: Integer]: TRegistroK110 read GetItem write SetItem;
  end;

  /// Registro K115 - Empresas Participantes do Evento Societário
  TRegistroK115 = class
  private
    fEMP_COD_PART: String;   /// Código da empresa envolvida na operação
    fCOND_PART: String;      /// Condição da empresa relacionada à operação: 1 – Sucessora; 2 – Adquirente; 3 – Alienante.
    fPER_EVT: Currency;      /// Percentual da empresa participante envolvida na operação
  public
    property EMP_COD_PART: String read fEMP_COD_PART write fEMP_COD_PART;
    property COND_PART: String read fCOND_PART write fCOND_PART;
    property PER_EVT: Currency read fPER_EVT write fPER_EVT;
  end;

  /// Registro K115 - Lista

  TRegistroK115List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK115;
    procedure SetItem(Index: Integer; const Value: TRegistroK115);
  public
    function New: TRegistroK115;
    property Items[Index: Integer]: TRegistroK115 read GetItem write SetItem;
  end;

  /// Registro K200 – PLANO DE CONTAS CONSOLIDADO

  TRegistroK200 = class
  private
    fCOD_NAT: String;        /// Código da natureza da conta/grupo de contas, conforme tabela publicada pelo Sped.
    fIND_CTA: String;        /// Indicador do tipo de conta: S - Sintética (grupo de contas); A - Analítica (conta).
    fNIVEL: String;          /// Nível da conta
    fCOD_CTA: String;        /// Código da conta
    fCOD_CTA_SU: String;     /// Código da conta superior
    fCTA: String;            /// Nome da conta

    FRegistroK210: TRegistroK210List;  /// BLOCO K - Lista de RegistroK210 (FILHO)
  public
    constructor Create; virtual; /// Create

    property COD_NAT: String read fCOD_NAT write fCOD_NAT;
    property IND_CTA: String read fIND_CTA write fIND_CTA;
    property NIVEL: String read fNIVEL write fNIVEL;
    property COD_CTA: String read fCOD_CTA write fCOD_CTA;
    property COD_CTA_SU: String read fCOD_CTA_SU write fCOD_CTA_SU;
    property CTA: String read fCTA write fCTA;

    property RegistroK210: TRegistroK210List read FRegistroK210 write FRegistroK210;
  end;

  /// Registro K200 - Lista

  TRegistroK200List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK200;
    procedure SetItem(Index: Integer; const Value: TRegistroK200);
  public
    function New: TRegistroK200;
    property Items[Index: Integer]: TRegistroK200 read GetItem write SetItem;
  end;

  /// Registro K210 -  Mapeamento para Planos de Contas das Empresas Consolidadas 

  TRegistroK210 = class
  private
    fCOD_EMP: String;      /// Código de identificação da empresa participante
    fCOD_CTA_EMP: String;  /// Código da conta da empresa participante
  public
    property COD_EMP: String read fCOD_EMP write fCOD_EMP;
    property COD_CTA_EMP: String read fCOD_CTA_EMP write fCOD_CTA_EMP;
  end;

  /// Registro K210 - Lista

  TRegistroK210List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK210;
    procedure SetItem(Index: Integer; const Value: TRegistroK210);
  public
    function New: TRegistroK210;
    property Items[Index: Integer]: TRegistroK210 read GetItem write SetItem;
  end;

  /// Registro K300 - SALDOS DAS CONTAS CONSOLIDADAS

  TRegistroK300 = class
  private
    fCOD_CTA: String;         /// Código da conta consolidada
    fVAL_AG: Currency;        /// Valor absoluto aglutinado
    fIND_VAL_AG: String;      /// Indicador da situação do valor aglutinado: D – Devedor C – Credor
    fVAL_EL: Currency;        /// Valor absoluto das eliminações
    fIND_VAL_EL: String;      /// Indicador da situação do valor eliminado: D – Devedor C – Credor
    fVAL_CS: Currency;        /// Valor absoluto consolidado: VAL_CS = VAL_AG – VAL_EL
    fIND_VAL_CS: String;      /// Indicador da situação do valor consolidado: D – Devedor C – Credor

    FRegistroK310: TRegistroK310List;  /// BLOCO K - Lista de RegistroK310 (FILHO)
  public
    constructor Create; virtual; /// Create

    property COD_CTA: String read fCOD_CTA write fCOD_CTA;
    property VAL_AG: Currency read fVAL_AG write fVAL_AG;
    property IND_VAL_AG: String read fIND_VAL_AG write fIND_VAL_AG;
    property VAL_EL: Currency read fVAL_EL write fVAL_EL;
    property IND_VAL_EL: String read fIND_VAL_EL write fIND_VAL_EL;
    property VAL_CS: Currency read fVAL_CS write fVAL_CS;
    property IND_VAL_CS: String read fIND_VAL_CS write fIND_VAL_CS;

    property RegistroK310: TRegistroK310List read FRegistroK310 write FRegistroK310;
  end;

  /// Registro K300 - Lista

  TRegistroK300List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK300;
    procedure SetItem(Index: Integer; const Value: TRegistroK300);
  public
    function New: TRegistroK300;
    property Items[Index: Integer]: TRegistroK300 read GetItem write SetItem;
  end;

  /// Registro K310 -  EMPRESAS DETENTORAS DAS PARCELAS DO VALOR ELIMINADO TOTAL

  TRegistroK310 = class
  private
    fEMP_COD_PARTE: String;     /// Código da empresa detentora do valor aglutinado que foi eliminado
    fVALOR: Currency;           /// Parcela do valor eliminado total
    fIND_VALOR: String;         /// Indicador da situação do valor eliminado: D – Devedor C – Credor

    FRegistroK315: TRegistroK315List;  /// BLOCO K - Lista de RegistroK315 (FILHO)
  public
    constructor Create; virtual; /// Create

    property EMP_COD_PARTE: String read fEMP_COD_PARTE write fEMP_COD_PARTE;
    property VALOR: Currency read fVALOR write fVALOR;
    property IND_VALOR: String read fIND_VALOR write fIND_VALOR;

    property RegistroK315: TRegistroK315List read FRegistroK315 write FRegistroK315;
  end;

  /// Registro K310 - Lista

  TRegistroK310List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK310;
    procedure SetItem(Index: Integer; const Value: TRegistroK310);
  public
    function New: TRegistroK310;
    property Items[Index: Integer]: TRegistroK310 read GetItem write SetItem;
  end;

  /// Registro K315 - Empresas Contrapartes das Parcelas do Valor Eliminado Total

  TRegistroK315 = class
  private
    fEMP_COD_CONTRA: String; /// Código da empresa da contrapartida
    fCOD_CONTRA: String;     /// Código da conta consolidada da contrapartida
    fVALOR: Currency;        /// Parcela da contrapartida do valor eliminado total
    fIND_VALOR: String;      /// Indicador da situação do valor eliminado
  public
    property EMP_COD_CONTRA: String read fEMP_COD_CONTRA write fEMP_COD_CONTRA;
    property COD_CONTRA: String read fCOD_CONTRA write fCOD_CONTRA;
    property VALOR: Currency read fVALOR write fVALOR;
    property IND_VALOR: String read fIND_VALOR write fIND_VALOR;
  end;

  /// Registro K315 - Lista

  TRegistroK315List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK315;
    procedure SetItem(Index: Integer; const Value: TRegistroK315);
  public
    function New: TRegistroK315;
    property Items[Index: Integer]: TRegistroK315 read GetItem write SetItem;
  end;

  /// Registro K990 - ENCERRAMENTO DO BLOCO K

  TRegistroK990 = class
  private
    fQTD_LIN_K: Integer;    /// Quantidade total de linhas do Bloco K
  public
    property QTD_LIN_K: Integer read FQTD_LIN_K write FQTD_LIN_K;
  end;

implementation

{ TRegistroK030 }

constructor TRegistroK030.Create;
begin
  FRegistroK100 := TRegistroK100List.Create;
  FRegistroK200 := TRegistroK200List.Create;
  FRegistroK300 := TRegistroK300List.Create;
end;

destructor TRegistroK030.Destroy;
begin
  FRegistroK100.Free;
  FRegistroK200.Free;
  FRegistroK300.Free;
  inherited;
end;

{ TRegistroK100 }

constructor TRegistroK100.Create;
begin
  FRegistroK110 := TRegistroK110List.Create;
end;

destructor TRegistroK100.Destroy;
begin
  FRegistroK110.Free;
  inherited;
end;

{ TRegistroK100List }

function TRegistroK100List.GetItem(Index: Integer): TRegistroK100;
begin
  Result := TRegistroK100(Inherited Items[Index]);
end;

function TRegistroK100List.LocalizaRegistro(const pEMP_COD: String): boolean;
var
intFor: integer;
begin
   Result := false;
   for intFor := 0 to Self.Count - 1 do
   begin
      if Self.Items[intFor].EMP_COD = pEMP_COD then
      begin
         Result := true;
         Break;
      end;
   end;
end;

function TRegistroK100List.New: TRegistroK100;
begin
  Result := TRegistroK100.Create;
  Add(Result);
end;

procedure TRegistroK100List.SetItem(Index: Integer; const Value: TRegistroK100);
begin
  Put(Index, Value);
end;

{ TRegistroK110 }

constructor TRegistroK110.Create;
begin
  FRegistroK115 := TRegistroK115List.Create;
end;

{ TRegistroK110List }

function TRegistroK110List.GetItem(Index: Integer): TRegistroK110;
begin
  Result := TRegistroK110(Inherited Items[Index]);
end;

function TRegistroK110List.New: TRegistroK110;
begin
  Result := TRegistroK110.Create;
  Add(Result);
end;

procedure TRegistroK110List.SetItem(Index: Integer; const Value: TRegistroK110);
begin
  Put(Index, Value);
end;

{ TRegistroK115List }

function TRegistroK115List.GetItem(Index: Integer): TRegistroK115;
begin
  Result := TRegistroK115(Inherited Items[Index]);
end;

function TRegistroK115List.New: TRegistroK115;
begin
  Result := TRegistroK115.Create;
  Add(Result);
end;

procedure TRegistroK115List.SetItem(Index: Integer; const Value: TRegistroK115);
begin
  Put(Index, Value);
end;

{ TRegistroK200 }

constructor TRegistroK200.Create;
begin
  FRegistroK210 := TRegistroK210List.Create;
end;

{ TRegistroK200List }

function TRegistroK200List.GetItem(Index: Integer): TRegistroK200;
begin
  Result := TRegistroK200(Inherited Items[Index]);
end;

function TRegistroK200List.New: TRegistroK200;
begin
  Result := TRegistroK200.Create;
  Add(Result);
end;

procedure TRegistroK200List.SetItem(Index: Integer; const Value: TRegistroK200);
begin
  Put(Index, Value);
end;

{ TRegistroK210List }

function TRegistroK210List.GetItem(Index: Integer): TRegistroK210;
begin
  Result := TRegistroK210(Inherited Items[Index]);
end;

function TRegistroK210List.New: TRegistroK210;
begin
  Result := TRegistroK210.Create;
  Add(Result);
end;

procedure TRegistroK210List.SetItem(Index: Integer; const Value: TRegistroK210);
begin
  Put(Index, Value);
end;

{ TRegistroK300 }

constructor TRegistroK300.Create;
begin
  FRegistroK310 := TRegistroK310List.Create;
end;

{ TRegistroK300List }

function TRegistroK300List.GetItem(Index: Integer): TRegistroK300;
begin
  Result := TRegistroK300(Inherited Items[Index]);
end;

function TRegistroK300List.New: TRegistroK300;
begin
  Result := TRegistroK300.Create;
  Add(Result);
end;

procedure TRegistroK300List.SetItem(Index: Integer; const Value: TRegistroK300);
begin
  Put(Index, Value);
end;

{ TRegistroK310 }

constructor TRegistroK310.Create;
begin
  FRegistroK315 := TRegistroK315List.Create;
end;

{ TRegistroK310List }

function TRegistroK310List.GetItem(Index: Integer): TRegistroK310;
begin
  Result := TRegistroK310(Inherited Items[Index]);
end;

function TRegistroK310List.New: TRegistroK310;
begin
  Result := TRegistroK310.Create;
  Add(Result);
end;

procedure TRegistroK310List.SetItem(Index: Integer; const Value: TRegistroK310);
begin
  Put(Index, Value);
end;

{ TRegistroK315List }

function TRegistroK315List.GetItem(Index: Integer): TRegistroK315;
begin
  Result := TRegistroK315(Inherited Items[Index]);
end;

function TRegistroK315List.New: TRegistroK315;
begin
  Result := TRegistroK315.Create;
  Add(Result);
end;

procedure TRegistroK315List.SetItem(Index: Integer; const Value: TRegistroK315);
begin
  Put(Index, Value);
end;

end.
