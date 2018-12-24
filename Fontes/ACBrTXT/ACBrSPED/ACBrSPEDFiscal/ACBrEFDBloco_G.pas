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
*******************************************************************************}

unit ACBrEFDBloco_G;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEFDBlocos;

type
  TRegistroG110List = class;
  TRegistroG125List = class;
  TRegistroG126List = class;
  TRegistroG130List = class;
  TRegistroG140List = class;

  /// Registro G001 - ABERTURA DO BLOCO G

  TRegistroG001 = class(TOpenBlocos)
  private
    FRegistroG110: TRegistroG110List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroG110: TRegistroG110List read FRegistroG110 write FRegistroG110;
  end;

  /// Registro G110 - ICMS - ATIVO PERMANENTE - CIAP

  TRegistroG110 = class
  private
    fMODO_CIAP: String;         /// Modelo de CIAP adotado C ou D
    fSALDO_IN_ICMS: Currency;   /// Saldo inicial de ICMS do CIAP Modelo C
    fSALDO_FN_ICMS: Currency;   /// Saldo Final ICMS do CIAP Modelo C
    fSOM_PARC: Currency;        /// Somatorio das Parcelas ICMS Passivel de Apropriacao Modelo D
    fVL_TRIB_EXP: Currency;     /// Valor do somatorio das saidas tributadas e saidas para exportacao
    fVL_TOTAL: Currency;        /// Valor Total das Saidas
    fIND_PER_SAI: Extended;       /// Participacao percentual do valor do somatorio das saidas tributadas e para exportacao
    fICMS_APROP: Currency;      /// Parcela de ICMS a ser apropriada no Registro de Apuracao do ICMS
    FSOM_ICMS_OC: Currency;     /// Valor de outros créditos a ser apropriado na Apuração do ICMS, correspondente ao somatório do campo 09 do registro G126
    FRegistroG125: TRegistroG125List;  /// BLOCO G - Lista de RegistroG110 (FILHO fo FILHO)
  public
    constructor Create(AOwner: TRegistroG001); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property MODO_CIAP: String read fMODO_CIAP write fMODO_CIAP;                /// Até versão 102
    property SALDO_IN_ICMS: Currency read fSALDO_IN_ICMS write fSALDO_IN_ICMS;
    property SALDO_FN_ICMS: Currency read fSALDO_FN_ICMS write fSALDO_FN_ICMS;  /// Até versão 102
    property SOM_PARC: Currency read fSOM_PARC write fSOM_PARC;
    property VL_TRIB_EXP: Currency read fVL_TRIB_EXP write fVL_TRIB_EXP;
    property VL_TOTAL: Currency read fVL_TOTAL write fVL_TOTAL;
    property IND_PER_SAI: Extended read fIND_PER_SAI write fIND_PER_SAI;          /// Foi renomeado na versão 103
    property ICMS_APROP: Currency read fICMS_APROP write fICMS_APROP;
    property SOM_ICMS_OC: Currency read FSOM_ICMS_OC write FSOM_ICMS_OC;

    property RegistroG125: TRegistroG125List read FRegistroG125 write FRegistroG125;
  end;

  /// Registro G110 - Lista

  TRegistroG110List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroG110; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroG110); /// SetItem
  public
    function New(AOwner: TRegistroG001): TRegistroG110;
    property Items[Index: Integer]: TRegistroG110 read GetItem write SetItem;
  end;

  /// Registro G125 - MOVIMENTACAO DE BEM OU COMPONENTE DO ATIVO IMOBILIZADO

  TRegistroG125 = class
  private
    fCOD_IND_BEM: String;          /// Codigo individualizado do bem ou componente
    fDT_MOV: TDateTime;            /// Data movimentacao ou saldo inicial
    fTIPO_MOV: TACBrMovimentoBens; /// Tipo de movimentacao do bem ou componente
    fVL_IMOB_ICMS_OP: Currency;    /// Valor ICMS Operacao Propria na entrada do bem ou componente
    fVL_IMOB_ICMS_ST: Currency;    /// Valor ICMS Operacao Subst.Trib. na entrada do bem ou componente
    fVL_IMOB_ICMS_FRT: Currency;   /// Valor ICMS Frete CTC na entrada do bem ou componente
    fVL_IMOB_ICMS_DIF: Currency;   /// Valor ICMS Diferencial de Aliquota cfe. Doc. arrecadacao na entrada do bem ou componente
    fNUM_PARC: Integer;            /// Numero da Parcela do ICMS
    fVL_PARC_PASS: Currency;       /// Valor parcela icms passivel de apropriacao
    fVL_PARC_APROP: Currency;      /// Valor da parcela apropriada do ICMS

    FRegistroG130: TRegistroG130List;  /// BLOCO G - Lista de RegistroG130 (FILHO do FILHO)
    FRegistroG126: TRegistroG126List;
  public
    constructor Create(AOwner: TRegistroG110); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_IND_BEM: String read fCOD_IND_BEM write fCOD_IND_BEM;
    property DT_MOV: TDateTime  read fDT_MOV write fDT_MOV;
    property TIPO_MOV: TACBrMovimentoBens read fTIPO_MOV write fTIPO_MOV;
    property VL_IMOB_ICMS_OP: Currency    read fVL_IMOB_ICMS_OP write fVL_IMOB_ICMS_OP;
    property VL_IMOB_ICMS_ST: Currency    read fVL_IMOB_ICMS_ST write fVL_IMOB_ICMS_ST;
    property VL_IMOB_ICMS_FRT: Currency   read fVL_IMOB_ICMS_FRT write fVL_IMOB_ICMS_FRT;
    property VL_IMOB_ICMS_DIF: Currency   read fVL_IMOB_ICMS_DIF write fVL_IMOB_ICMS_DIF;
    property NUM_PARC: Integer            read fNUM_PARC write fNUM_PARC;
    property VL_PARC_PASS: Currency       read fVL_PARC_PASS write fVL_PARC_PASS;
    property VL_PARC_APROP: Currency      read fVL_PARC_APROP write fVL_PARC_APROP;   /// Até versão 102

    property RegistroG130: TRegistroG130List read FRegistroG130 write FRegistroG130;
    property RegistroG126: TRegistroG126List read FRegistroG126 write FRegistroG126;
  end;

  /// Registro G125 - Lista

  TRegistroG125List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroG125; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroG125); /// SetItem
  public
    function New(AOwner: TRegistroG110): TRegistroG125;
    property Items[Index: Integer]: TRegistroG125 read GetItem write SetItem;
  end;

  /// Registro G126 - OUTROS CRÉDITOS CIAP  
  TRegistroG126 = class
  private
    FDT_INI       : TDateTime;
    FDT_FIN       : TDateTime;
    FNUM_PARC     : Integer;
    FVL_PARC_PASS : Currency;
    FVL_TRIB_OC   : Currency;
    FVL_TOTAL     : Currency;
    FIND_PER_SAI  : Extended;
    FVL_PARC_APROP: Currency;
  public
    constructor Create(AOwner: TRegistroG125); virtual; /// Create

    property DT_INI: TDateTime        read fDT_INI        write fDT_INI;
    property DT_FIN: TDateTime        read FDT_FIN        write FDT_FIN;
    property NUM_PARC: Integer        read FNUM_PARC      write FNUM_PARC;
    property VL_PARC_PASS: Currency   read FVL_PARC_PASS  write FVL_PARC_PASS;
    property VL_TRIB_OC: Currency     read FVL_TRIB_OC    write FVL_TRIB_OC;
    property VL_TOTAL: Currency       read FVL_TOTAL      write FVL_TOTAL;
    property IND_PER_SAI: Extended    read FIND_PER_SAI   write FIND_PER_SAI;
    property VL_PARC_APROP: Currency  read FVL_PARC_APROP write FVL_PARC_APROP;
  end;

  /// Registro G126 - Lista

  TRegistroG126List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroG126; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroG126); /// SetItem
  public
    function New(AOwner: TRegistroG125): TRegistroG126;
    property Items[Index: Integer]: TRegistroG126 read GetItem write SetItem;
  end;

  /// Registro G130 - IDENTIFICACAO DO DOCUMENTO FISCAL

  TRegistroG130 = class
  private
    fIND_EMIT: TACBrIndEmit;     /// Código do ajuste da apuração e dedução, conforme a Tabela indicada no item 5.1.1.
    fCOD_PART: String;            /// Descrição complementar do ajuste da apuração.
    fCOD_MOD: String;             /// Valor do ajuste da apuração
    fSERIE: String;
    fNUM_DOC: String;
    fCHV_NFE_CTE: String;
    fDT_DOC: TDateTime;

    FRegistroG140: TRegistroG140List;  /// BLOCO G - Lista de RegistroG130 (FILHO fo FILHO)
  public
    constructor Create(AOwner: TRegistroG125); virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_EMIT: TACBrIndEmit read fIND_EMIT write fIND_EMIT;
    property COD_PART: String read fCOD_PART write fCOD_PART;
    property COD_MOD: String read fCOD_MOD write fCOD_MOD;
    property SERIE: String read fSERIE write fSERIE;
    property NUM_DOC: String read fNUM_DOC write fNUM_DOC;
    property CHV_NFE_CTE: String read fCHV_NFE_CTE write fCHV_NFE_CTE;
    property DT_DOC: TDateTime read fDT_DOC write fDT_DOC;

    property RegistroG140: TRegistroG140List read FRegistroG140 write FRegistroG140;
  end;

  /// Registro G130 - Lista
  TRegistroG130List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroG130; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroG130); /// SetItem
  public
    function New(AOwner: TRegistroG125): TRegistroG130;
    property Items[Index: Integer]: TRegistroG130 read GetItem write SetItem;
  end;

  /// Registro G140 - IDENTIFICACAO DO ITEM DO DOCUMENTO FISCAL

  TRegistroG140 = class
  private
    fNUM_ITEM: String;  /// Numero Sequencial do Item no documento fiscal
    fCOD_ITEM: String;  /// Codigo Correspondente do bem no documento fiscal
  public
    constructor Create(AOwner: TRegistroG130); virtual; /// Create

    property NUM_ITEM: String read fNUM_ITEM write fNUM_ITEM;
    property COD_ITEM: String read fCOD_ITEM write fCOD_ITEM;
  end;

  /// Registro G140 - Lista

  TRegistroG140List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroG140; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistroG140); /// SetItem
  public
    function New(AOwner: TRegistroG130): TRegistroG140;
    property Items[Index: Integer]: TRegistroG140 read GetItem write SetItem;
  end;

  /// Registro G990 - ENCERRAMENTO DO BLOCO G

  TRegistroG990 = class
  private
    fQTD_LIN_G: Integer;    /// Quantidade total de linhas do Bloco H
  public
    property QTD_LIN_G: Integer read fQTD_LIN_G write fQTD_LIN_G;
  end;

implementation


{ TRegistroG110 }

constructor TRegistroG110.Create(AOwner: TRegistroG001);
begin
  FRegistroG125 := TRegistroG125List.Create;  /// BLOCO G - Lista de RegistroG125 (FILHO fo FILHO)
end;

destructor TRegistroG110.Destroy;
begin
  FRegistroG125.Free;
  inherited;
end;

{ TRegistroG110List }

function TRegistroG110List.GetItem(Index: Integer): TRegistroG110;
begin
  Result := TRegistroG110(Inherited Items[Index]);
end;

function TRegistroG110List.New(AOwner: TRegistroG001): TRegistroG110;
begin
  Result := TRegistroG110.Create(AOwner);
  Add(Result);
end;

procedure TRegistroG110List.SetItem(Index: Integer; const Value: TRegistroG110);
begin
  Put(Index, Value);
end;

{ TRegistroG125 }

constructor TRegistroG125.Create(AOwner: TRegistroG110);
begin
  FRegistroG130 := TRegistroG130List.Create;  /// BLOCO G - Lista de RegistroG130 (FILHO fo FILHO)
  FRegistroG126 := TRegistroG126List.Create;  /// BLOCO G - Lista de RegistroG126 (FILHO fo FILHO)
end;

destructor TRegistroG125.Destroy;
begin
  FRegistroG130.Free;
  FRegistroG126.Free;
  inherited;
end;

{ TRegistroG125List }

function TRegistroG125List.GetItem(Index: Integer): TRegistroG125;
begin
  Result := TRegistroG125(Inherited Items[Index]);
end;

function TRegistroG125List.New(AOwner: TRegistroG110): TRegistroG125;
begin
  Result := TRegistroG125.Create(AOwner);
  Add(Result);
end;

procedure TRegistroG125List.SetItem(Index: Integer; const Value: TRegistroG125);
begin
  Put(Index, Value);
end;

{ TRegistroG130 }

constructor TRegistroG130.Create(AOwner: TRegistroG125);
begin
  FRegistroG140 := TRegistroG140List.Create;  /// BLOCO G - Lista de RegistroG130 (FILHO fo FILHO)
end;

destructor TRegistroG130.Destroy;
begin
  FRegistroG140.Free;
  inherited;
end;

{ TRegistroG130List }

function TRegistroG130List.GetItem(Index: Integer): TRegistroG130;
begin
  Result := TRegistroG130(Inherited Items[Index]);
end;

function TRegistroG130List.New(AOwner: TRegistroG125): TRegistroG130;
begin
  Result := TRegistroG130.Create(AOwner);
  Add(Result);
end;

procedure TRegistroG130List.SetItem(Index: Integer; const Value: TRegistroG130);
begin
  Put(Index, Value);
end;

{ TRegistroG140List }

function TRegistroG140List.GetItem(Index: Integer): TRegistroG140;
begin
  Result := TRegistroG140(Inherited Items[Index]);
end;

function TRegistroG140List.New(AOwner: TRegistroG130): TRegistroG140;
begin
  Result := TRegistroG140.Create(AOwner);
  Add(Result);
end;

procedure TRegistroG140List.SetItem(Index: Integer; const Value: TRegistroG140);
begin
  Put(Index, Value);
end;

{ TRegistroG001 }

constructor TRegistroG001.Create;
begin
   inherited Create;
   FRegistroG110 := TRegistroG110List.Create;
   //
   IND_MOV := imSemDados;
end;

destructor TRegistroG001.Destroy;
begin
   FRegistroG110.Free;
  inherited;
end;

{ TRegistroG126List }

function TRegistroG126List.GetItem(Index: Integer): TRegistroG126;
begin
  Result := TRegistroG126(Inherited Items[Index]);
end;

function TRegistroG126List.New(AOwner: TRegistroG125): TRegistroG126;
begin
  Result := TRegistroG126.Create(AOwner);
  Add(Result);
end;

procedure TRegistroG126List.SetItem(Index: Integer;
  const Value: TRegistroG126);
begin
  Put(Index, Value);
end;

{ TRegistroG126 }

constructor TRegistroG126.Create(AOwner: TRegistroG125);
begin
end;

{ TRegistroG140 }

constructor TRegistroG140.Create(AOwner: TRegistroG130);
begin
end;

end.
