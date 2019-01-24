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
|* 23/08/2013: Juliana Tamizou
|*  - Distribuição da Primeira Versao
*******************************************************************************}
{$I ACBr.inc}

unit ACBrSEF2_BlocoH;

interface

uses Classes, SysUtils, ACBrSEF2Conversao;

type
  TRegistroSEFH020List = class;
  TRegistroSEFH030List = class;
  TRegistroSEFH040List = class;
  TRegistroSEFH050List = class;
  TRegistroSEFH060List = class;

  //LINHA H001: ABERTURA DO BLOCO H

  TRegistroSEFH001 = class
  private
    fIND_DAD: TSEFIIIndicadorConteudo;
    fRegistroH020: TRegistroSEFH020List;
    fRegistroH030: TRegistroSEFH030List;
    fRegistroH040: TRegistroSEFH040List;
    fRegistroH050: TRegistroSEFH050List;
    fRegistroH060: TRegistroSEFH060List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy
    property IND_DAD: TSEFIIIndicadorConteudo read fIND_DAD write fIND_DAD;
    property RegistroH020: TRegistroSEFH020List read fRegistroH020 write fRegistroH020;
    property RegistroH030: TRegistroSEFH030List read fRegistroH030 write fRegistroH030;
    property RegistroH040: TRegistroSEFH040List read fRegistroH040 write fRegistroH040;
    property RegistroH050: TRegistroSEFH050List read fRegistroH050 write fRegistroH050;
    property RegistroH060: TRegistroSEFH060List read fRegistroH060 write fRegistroH060;
  end;


  TRegistroSEFH020 = class
  private
    FIND_DT: integer;
    FDT_INV: TDateTime;
    FVL_ESTQ: double;
    FVL_ICMS_REC: double;
    FVL_IPI_REC: double;
    FVL_PIS_REC: double;
    FVL_COFINS_REC: double;
    FVL_TRIB_NC: double;
    FVL_ESTQ_NC: double;
    FNUM_LCTO: string;
    FCOD_INF_OBS: string;
  public
    property IND_DT: integer read FIND_DT write FIND_DT;//Indicador da data do inventário:
    property DT_INV: TDateTime read FDT_INV write FDT_INV;//Data do inventário
    property VL_ESTQ: double read FVL_ESTQ write FVL_ESTQ;//Valor do estoque na data assinalada
    property VL_ICMS_REC: double read FVL_ICMS_REC write FVL_ICMS_REC;//Valor do ICMS a recuperar
    property VL_IPI_REC: double read FVL_IPI_REC write FVL_IPI_REC;//Valor do IPI a recuperar
    property VL_PIS_REC: double read FVL_PIS_REC write FVL_PIS_REC;//Valor do PIS a recuperar
    property VL_COFINS_REC: double read FVL_COFINS_REC write FVL_COFINS_REC;//Valor do COFINS a recuperar
    property VL_TRIB_NC: double read FVL_TRIB_NC write FVL_TRIB_NC;
    //Valor dos tributos não-cumulativos recuperáveis
    property VL_ESTQ_NC: double read FVL_ESTQ_NC write FVL_ESTQ_NC;
    //Valor do estoque, retirado o valor dos tributos recuperáveis
    property NUM_LCTO: string read FNUM_LCTO write FNUM_LCTO;
    //Número ou código de identificação única do lançamento contábil
    property COD_INF_OBS: string read FCOD_INF_OBS write FCOD_INF_OBS;
    //Código de referência à observação (campo 02 da Linha 0450)
  end;

  TRegistroSEFH020List = class(TACBrSEFIIRegistros)
  private
    function GetNotas(Index: integer): TRegistroSEFH020;
    procedure SetNotas(Index: integer; const Value: TRegistroSEFH020);
  public
    function New(): TRegistroSEFH020;
    property notas[Index: integer]: TRegistroSEFH020 read Getnotas write SetNotas;
  end;

  // ITENS INVENTARIADOS
  TRegistroSEFH030 = class
  private
    FIND_POSSE: integer;
    FCOD_PART: string;
    FIND_ITEM: integer;
    FCOD_NCM: integer;
    FCOD_ITEM: string;
    FUNID: string;
    FVL_UNIT: double;
    FQTD: double;
    FVL_ITEM: double;
    FVL_ICMS_REC_I: double;
    FVL_IPI_REC_I: double;
    FVL_PIS_REC_I: double;
    FVL_COFINS_REC_I: double;
    FVL_TRIB_NC_I: double;
    FCOD_INF_OBS: string;
  public
    property IND_POSSE: integer read FIND_POSSE write FIND_POSSE;//Indicador de propriedade/posse do item
    property COD_PART: string read FCOD_PART write FCOD_PART;
    //Código do participante (campo 02 da Linha 0150) do proprietário ou possuidor
    property IND_ITEM: integer read FIND_ITEM write FIND_ITEM;//Indicador do tipo de item inventariado:
    property COD_NCM: integer read FCOD_NCM write FCOD_NCM;
    //Código da Nomenclatura Comum do Mercosul, conforme a tabela externa indicada no item 3.3.1
    property COD_ITEM: string read FCOD_ITEM write FCOD_ITEM;//Código do item (campo 02 da Linha 0200)
    property UNID: string read FUNID write FUNID;//UNIDADE DO ITEM
    property VL_UNIT: double read FVL_UNIT write FVL_UNIT;//Valor do UNITARIO
    property QTD: double read FQTD write FQTD;//QUANTIDADE DO ITEM
    property VL_ITEM: double read FVL_ITEM write FVL_ITEM;//Valor do ITEM
    property VL_ICMS_REC_I: double read FVL_ICMS_REC_I write FVL_ICMS_REC_I;//Valor do ICMS a recuperar
    property VL_IPI_REC_I: double read FVL_IPI_REC_I write FVL_IPI_REC_I;//Valor do IPI a recuperar
    property VL_PIS_REC_I: double read FVL_PIS_REC_I write FVL_PIS_REC_I;//Valor do PIS a recuperar
    property VL_COFINS_REC_I: double read FVL_COFINS_REC_I write FVL_COFINS_REC_I;//Valor do COFINS a recuperar
    property VL_TRIB_NC_I: double read FVL_TRIB_NC_I write FVL_TRIB_NC_I;
    //Valor dos tributos não-cumulativos recuperáveis
    property COD_INF_OBS: string read FCOD_INF_OBS write FCOD_INF_OBS;
    //Código de referência à observação (campo 02 da Linha 0450)
  end;

  TRegistroSEFH030List = class(TACBrSEFIIRegistros)

  private
    function GetNotas(Index: integer): TRegistroSEFH030;
    procedure SetNotas(Index: integer; const Value: TRegistroSEFH030);
  public
    function New(): TRegistroSEFH030;
    property notas[Index: integer]: TRegistroSEFH030 read Getnotas write SetNotas;
  end;

  // SUBTOTAIS POR POSSUIDOR/PROPRIETÁRIO
  TRegistroSEFH040 = class
  private
    FIND_POSSE: integer;
    FVL_SUB_POSSE: double;
  public
    property IND_POSSE: integer read FIND_POSSE write FIND_POSSE;//Indicador de posse a ser totalizado:
    property VL_SUB_POSSE: double read FVL_SUB_POSSE write FVL_SUB_POSSE;
    //Valor subtotal por possuidor ou proprietário
  end;

  TRegistroSEFH040List = class(TACBrSEFIIRegistros)

  private
    function GetNotas(Index: integer): TRegistroSEFH040;
    procedure SetNotas(Index: integer; const Value: TRegistroSEFH040);
  public
    function New(): TRegistroSEFH040;
    property notas[Index: integer]: TRegistroSEFH040 read Getnotas write SetNotas;
  end;


  // SUBTOTAIS POR TIPO DE ITEM
  TRegistroSEFH050 = class
  private
    FIND_ITEM: integer;
    FVL_SUB_ITEM: double;
  public
    property IND_ITEM: integer read FIND_ITEM write FIND_ITEM;//Indicador de tipo item a ser totalizado
    property VL_SUB_ITEM: double read FVL_SUB_ITEM write FVL_SUB_ITEM;//Valor subtotal por tipo de item
  end;

  TRegistroSEFH050List = class(TACBrSEFIIRegistros)

  private
    function GetNotas(Index: integer): TRegistroSEFH050;
    procedure SetNotas(Index: integer; const Value: TRegistroSEFH050);
  public
    function New(): TRegistroSEFH050;
    property notas[Index: integer]: TRegistroSEFH050 read Getnotas write SetNotas;
  end;


  // SUBTOTAIS POR NCM
  TRegistroSEFH060 = class
  private
    FCOD_NCM: integer;
    FVL_SUB_NCM: double;
  public
    property COD_NCM: integer read FCOD_NCM write FCOD_NCM; //Código da Nomenclatura Comum do Mercosul
    property VL_SUB_NCM: double read FVL_SUB_NCM write FVL_SUB_NCM;//Valor subtotal por NCM
  end;

  TRegistroSEFH060List = class(TACBrSEFIIRegistros)

  private
    function GetNotas(Index: integer): TRegistroSEFH060;
    procedure SetNotas(Index: integer; const Value: TRegistroSEFH060);
  public
    function New(): TRegistroSEFH060;
    property notas[Index: integer]: TRegistroSEFH060 read Getnotas write SetNotas;
  end;

  /// Registro H990 - Encerramento do Bloco H
  TRegistroSEFH990 = class
  private
    fQTD_LIN_H: integer;
  public
    property QTD_LIN_H: integer read fQTD_LIN_H write fQTD_LIN_H;
  end;

implementation

constructor TRegistroSEFH001.Create;
begin
  fRegistroH020 := TRegistroSEFH020List.Create;
  fRegistroH030 := TRegistroSEFH030List.Create;
  fRegistroH040 := TRegistroSEFH040List.Create;
  fRegistroH050 := TRegistroSEFH050List.Create;
  fRegistroH060 := TRegistroSEFH060List.Create;
  IND_DAD := icSemConteudo;
end;

destructor TRegistroSEFH001.Destroy;
begin
  fRegistroH020.Free;
  fRegistroH030.Free;
  fRegistroH040.Free;
  fRegistroH050.Free;
  fRegistroH060.Free;
  inherited;
end;


procedure TRegistroSEFH020List.SetNotas(Index: integer; const Value: TRegistroSEFH020);
begin
  Put(Index, Value);
end;

function TRegistroSEFH020List.GetNotas(Index: integer): TRegistroSEFH020;
begin
  Result := TRegistroSEFH020(Get(Index));
end;

function TRegistroSEFH020List.New(): TRegistroSEFH020;
begin
  Result := TRegistroSEFH020.Create;
  Add(Result);
end;

procedure TRegistroSEFH030List.SetNotas(Index: integer; const Value: TRegistroSEFH030);
begin
  Put(Index, Value);
end;

function TRegistroSEFH030List.GetNotas(Index: integer): TRegistroSEFH030;
begin
  Result := TRegistroSEFH030(Get(Index));
end;

function TRegistroSEFH030List.New(): TRegistroSEFH030;
begin
  Result := TRegistroSEFH030.Create;
  Add(Result);
end;

procedure TRegistroSEFH040List.SetNotas(Index: integer; const Value: TRegistroSEFH040);
begin
  Put(Index, Value);
end;

function TRegistroSEFH040List.GetNotas(Index: integer): TRegistroSEFH040;
begin
  Result := TRegistroSEFH040(Get(Index));
end;

function TRegistroSEFH040List.New(): TRegistroSEFH040;
begin
  Result := TRegistroSEFH040.Create;
  Add(Result);
end;

procedure TRegistroSEFH050List.SetNotas(Index: integer; const Value: TRegistroSEFH050);
begin
  Put(Index, Value);
end;

function TRegistroSEFH050List.GetNotas(Index: integer): TRegistroSEFH050;
begin
  Result := TRegistroSEFH050(Get(Index));
end;

function TRegistroSEFH050List.New(): TRegistroSEFH050;
begin
  Result := TRegistroSEFH050.Create;
  Add(Result);
end;

procedure TRegistroSEFH060List.SetNotas(Index: integer; const Value: TRegistroSEFH060);
begin
  Put(Index, Value);
end;

function TRegistroSEFH060List.GetNotas(Index: integer): TRegistroSEFH060;
begin
  Result := TRegistroSEFH060(Get(Index));
end;

function TRegistroSEFH060List.New(): TRegistroSEFH060;
begin
  Result := TRegistroSEFH060.Create;
  Add(Result);
end;

end.
