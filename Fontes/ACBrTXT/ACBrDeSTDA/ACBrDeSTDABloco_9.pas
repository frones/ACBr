{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: João Pedro R Costa                              }
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

unit ACBrDeSTDABloco_9;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrDeSTDABlocos;

type
  TRegistro9020     = class;
  TRegistro9020List = class;
  TRegistro9030     = class;
  TRegistro9030List = class;

 /// Registro 9001 - ABERTURA DO BLOCO 9

  TRegistro9001 = class(TOpenBlocos)
  private
    FRegistro9020: TRegistro9020List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property Registro9020: TRegistro9020List read FRegistro9020 write FRegistro9020;
  end;

  /// REGISTRO 9020: INFORMAÇÕES DE ARQUIVOS ASSOCIADOS

  TRegistro9020 = class
  private
    FLEIAUTE      : TACBrDeLeiauteArquivos; ///Texto fixo contendo "LFPD" ou "LECD"
    FARQ_DT_INI   : TDateTime;              ///Data inicial das informações do arquivo associado
    FARQ_DT_FIN   : TDateTime;              ///Data final das informações do arquivo associado
    FARQ_NOME_EMPR: string;                 ///Nome empresarial do contribuinte do arquivo associado
    FARQ_CNPJ     : string;                 ///CNPJ do contribuinte do arquivo associado
    FARQ_UF       : string;                 ///Sigla da unidade da Federação do arquivo associado
    FARQ_IE       : string;                 ///Inscrição estadual do contribuinte do arquivo associado
    FARQ_COD_MUN  : Integer;                ///Código do município do arquivo associado
    FARQ_IM       : string;                 ///Inscrição municipal do contribuinte do arquivo associado
    FVAZIO        : string;                 ///Texto fixo contendo ""
    FARQ_SUFRAMA  : string;                 ///Inscrição do contribuinte na Suframa do arquivo associado
    FARQ_COD_VER  : string;                 ///Código da versão do leiaute do arquivo associado
    FARQ_COD_FIN  : string;                 ///Código da finalidade do arquivo associado
    FARQ_COD_CTD  : string;                 ///Código do conteúdo do arquivo associado
    FARQ_PAIS     : string;                 ///Texto fixo contendo "Brasil"
    FARQ_CPF      : string;                 ///CPF do contribuinte do arquivo associado
    FARQ_FANTASIA : string;                 ///Nome de fantasia associado ao nome empresarial do arquivo associado
    FARQ_QTD_LIN  : integer;                ///Quantidade total de registros do arquivo associado
    FARQ_NOME     : string;                 ///Nome atribuído pelo informante ao arquivo associado (com ponto separador e extensão)
    FASS_HASH     : string;                 ///Hash do arquivo associado

    FRegistro9030 : TRegistro9030List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property LEIAUTE      : TACBrDeLeiauteArquivos read FLEIAUTE       write FLEIAUTE;
    property ARQ_DT_INI   : TDateTime              read FARQ_DT_INI    write FARQ_DT_INI;
    property ARQ_DT_FIN   : TDateTime              read FARQ_DT_FIN    write FARQ_DT_FIN;
    property ARQ_NOME_EMPR: string                 read FARQ_NOME_EMPR write FARQ_NOME_EMPR;
    property ARQ_CNPJ     : string                 read FARQ_CNPJ      write FARQ_CNPJ;
    property ARQ_UF       : string                 read FARQ_UF        write FARQ_UF;
    property ARQ_IE       : string                 read FARQ_IE        write FARQ_IE;
    property ARQ_COD_MUN  : Integer                read FARQ_COD_MUN   write FARQ_COD_MUN;
    property ARQ_IM       : string                 read FARQ_IM        write FARQ_IM;
    property VAZIO        : string                 read FVAZIO         write FVAZIO;
    property ARQ_SUFRAMA  : string                 read FARQ_SUFRAMA   write FARQ_SUFRAMA;
    property ARQ_COD_VER  : string                 read FARQ_COD_VER   write FARQ_COD_VER;
    property ARQ_COD_FIN  : string                 read FARQ_COD_FIN   write FARQ_COD_FIN;
    property ARQ_COD_CTD  : string                 read FARQ_COD_CTD   write FARQ_COD_CTD;
    property ARQ_PAIS     : string                 read FARQ_PAIS      write FARQ_PAIS;
    property ARQ_CPF      : string                 read FARQ_CPF       write FARQ_CPF;
    property ARQ_FANTASIA : string                 read FARQ_FANTASIA  write FARQ_FANTASIA;
    property ARQ_QTD_LIN  : integer                read FARQ_QTD_LIN   write FARQ_QTD_LIN;
    property ARQ_NOME     : string                 read FARQ_NOME      write FARQ_NOME;
    property ASS_HASH     : string                 read FASS_HASH      write FASS_HASH;
    property Registro9030 : TRegistro9030List      read FRegistro9030  write FRegistro9030;
  end;

  TRegistro9020List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro9020;
    procedure SetItem(Index: Integer; const Value: TRegistro9020);
  public
    function New: TRegistro9020;
    property Items[Index: Integer]: TRegistro9020 read GetItem write SetItem;
  end;

  /// REGISTRO 9030: REGISTROS DO ARQUIVO ASSOCIADO

  TRegistro9030 = class
  private
    FARQ_LIN_BLC    : string;
    FARQ_QTD_LIN_BLC: integer;
  public
    property ARQ_LIN_BLC    : string  read FARQ_LIN_BLC     write FARQ_LIN_BLC;
    property ARQ_QTD_LIN_BLC: integer read FARQ_QTD_LIN_BLC write FARQ_QTD_LIN_BLC;
  end;

  TRegistro9030List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro9030;
    procedure SetItem(Index: Integer; const Value: TRegistro9030);
  public
    function New: TRegistro9030;
    property Items[Index: Integer]: TRegistro9030 read GetItem write SetItem;
  end;

 /// Registro 9900 - REGISTROS DO ARQUIVO

 TRegistro9900 = class
 private
   fREG_BLC: String;    /// Registro que será totalizado no próximo campo.
   fQTD_REG_BLC: Integer;   /// Total de registros do tipo informado no campo anterior.
 public
   property REG_BLC: String read fREG_BLC write fREG_BLC;
   property QTD_REG_BLC: Integer read fQTD_REG_BLC write fQTD_REG_BLC;
 end;

  /// Registro 9900 - Lista

  TRegistro9900List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro9900; /// GetItem
    procedure SetItem(Index: Integer; const Value: TRegistro9900); /// SetItem
  public
    function New: TRegistro9900;
    property Items[Index: Integer]: TRegistro9900 read GetItem write SetItem;
  end;

 /// Registro 9990 - ENCERRAMENTO DO BLOCO 9

 TRegistro9990 = class
 private
   fQTD_LIN_9: Integer; /// Quantidade total de linhas do arquivo digital.
 public
   property QTD_LIN_9: Integer read fQTD_LIN_9 write fQTD_LIN_9;
 end;

 /// Registro 9999 - ENCERRAMENTO DO ARQUIVO DIGITAL

 TRegistro9999 = class
 private
   fQTD_LIN: Integer; /// Quantidade total de linhas do arquivo digital.
 public
   property QTD_LIN: Integer read fQTD_LIN write fQTD_LIN;
 end;

implementation

{ TRegistro9900List }

function TRegistro9900List.GetItem(Index: Integer): TRegistro9900;
begin
  Result := TRegistro9900(Inherited Items[Index]);
end;

function TRegistro9900List.New: TRegistro9900;
begin
  Result := TRegistro9900.Create;
  Add(Result);
end;

procedure TRegistro9900List.SetItem(Index: Integer; const Value: TRegistro9900);
begin
  Put(Index, Value);
end;

{ TRegistro9020 }

constructor TRegistro9020.Create;
begin
  FRegistro9030 := TRegistro9030List.Create;
end;

destructor TRegistro9020.Destroy;
begin
  FRegistro9030.Free;
  inherited;
end;

{ TRegistro9020List }

function TRegistro9020List.GetItem(Index: Integer): TRegistro9020;
begin
  Result := TRegistro9020(Inherited Items[Index]);
end;

function TRegistro9020List.New: TRegistro9020;
begin
  Result := TRegistro9020.Create;
  Add(Result);
end;

procedure TRegistro9020List.SetItem(Index: Integer; const Value: TRegistro9020);
begin
  Put(Index, Value);
end;

{ TRegistro9030List }

function TRegistro9030List.GetItem(Index: Integer): TRegistro9030;
begin
  Result := TRegistro9030(Inherited Items[Index]);
end;

function TRegistro9030List.New: TRegistro9030;
begin
  Result := TRegistro9030.Create;
  Add(Result);
end;

procedure TRegistro9030List.SetItem(Index: Integer; const Value: TRegistro9030);
begin
  Put(Index, Value);
end;

{ TRegistro9001 }

constructor TRegistro9001.Create;
begin
  inherited Create;
  FRegistro9020 := TRegistro9020List.Create;
end;

destructor TRegistro9001.Destroy;
begin
  FRegistro9020.Free;
  inherited;
end;

end.
