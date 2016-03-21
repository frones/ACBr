{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2012   Albert Eije                          }
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
|* 04/07/2012: Albert Eije
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrPonto_AFDT;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils;

type
  // Registro tipo “1” - Cabeçalho
  TCabecalho = class
  private
    FCampo01: String; // Seqüencial do registro no arquivo.
    FCampo02: String; // Tipo do registro, “1”.
    FCampo03: String; // Tipo de identificador do empregador, “1” para CNPJ ou “2” para CPF.
    FCampo04: String; // CNPJ ou CPF do empregador.
    FCampo05: String; // CEI do empregador, quando existir.
    FCampo06: String; // Razão social ou nome do empregador.
    FCampo07: String; // Data inicial dos registros no arquivo, no formato “ddmmaaaa”.
    FCampo08: String; // Data final dos registros no arquivo, no formato “ddmmaaaa”.
    FCampo09: String; // Data de geração do arquivo, no formato “ddmmaaaa”.
    FCampo10: String; // Horário da geração do arquivo, no formato “hhmm”.

    FRegistroValido: Boolean;

  public
    constructor Create; virtual; 
    destructor Destroy; override;

    property RegistroValido: Boolean read FRegistroValido write FRegistroValido default True;
    property Campo01: String read FCampo01 write FCampo01;
    property Campo02: String read FCampo02 write FCampo02;
    property Campo03: String read FCampo03 write FCampo03;
    property Campo04: String read FCampo04 write FCampo04;
    property Campo05: String read FCampo05 write FCampo05;
    property Campo06: String read FCampo06 write FCampo06;
    property Campo07: String read FCampo07 write FCampo07;
    property Campo08: String read FCampo08 write FCampo08;
    property Campo09: String read FCampo09 write FCampo09;
    property Campo10: String read FCampo10 write FCampo10;
  end;

  // Registro DETALHE - Registro de inclusão ou alteração da identificação da empresa no REP
  TRegistro2 = class
  private
    FCampo01: String; // Seqüencial do registro no arquivo.
    FCampo02: String; // Tipo do registro, “2”.
    FCampo03: String; // Data da marcação do ponto, no formato “ddmmaaaa”.
    FCampo04: String; // Horário da marcação do ponto, no formato “hhmm”.
    FCampo05: String; // Número do PIS do empregado.
    FCampo06: String; // Número de fabricação do REP onde foi feito o registro.
    FCampo07: String; // Tipo de marcação, “E” para ENTRADA, “S” para SAÍDA ou “D” para registro a ser DESCONSIDERADO.
    FCampo08: String; // Número seqüencial por empregado e jornada para o conjunto Entrada/Saída. Vide observação.
    FCampo09: String; // Tipo de registro: “O” para registro eletrônico ORIGINAL, “I” para registro INCLUÍDO por digitação, “P” para intervalo PRÉ-ASSINALADO.
    FCampo10: String; // Motivo: Campo a ser preenchido se o campo 7 for “D” ou se o campo 9 for “I”.

    FRegistroValido: boolean;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property Campo01: String read FCampo01 write FCampo01;
    property Campo02: String read FCampo02 write FCampo02;
    property Campo03: String read FCampo03 write FCampo03;
    property Campo04: String read FCampo04 write FCampo04;
    property Campo05: String read FCampo05 write FCampo05;
    property Campo06: String read FCampo06 write FCampo06;
    property Campo07: String read FCampo07 write FCampo07;
    property Campo08: String read FCampo08 write FCampo08;
    property Campo09: String read FCampo09 write FCampo09;
    property Campo10: String read FCampo10 write FCampo10;
  end;

  // Registro 2 - Lista
  TRegistro2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro2;
    procedure SetItem(Index: Integer; const Value: TRegistro2);
  public
    function New: TRegistro2;
    property Items[Index: Integer]: TRegistro2 read GetItem write SetItem;
  end;

  // Registro Trailer
  TTrailer = class
  private
    FCampo01: String; // Seqüencial do registro no arquivo.
    FCampo02: String; // Tipo do registro, “9”.

    FRegistroValido: boolean;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property Campo01: String read FCampo01 write FCampo01;
    property Campo02: String read FCampo02 write FCampo02;
  end;

implementation

{ TCabecalho }

constructor TCabecalho.Create;
begin
  FRegistroValido := True;
end;

destructor TCabecalho.Destroy;
begin
  inherited;
end;

{ TRegistro2List }

function TRegistro2List.GetItem(Index: Integer): TRegistro2;
begin
  Result := TRegistro2(inherited Items[Index]);
end;

function TRegistro2List.New: TRegistro2;
begin
  Result := TRegistro2.Create;
  Add(Result);
end;

procedure TRegistro2List.SetItem(Index: Integer; const Value: TRegistro2);
begin
  Put(Index, Value);
end;

{ TRegistro2 }

constructor TRegistro2.Create;
begin
  FRegistroValido := True;
end;

destructor TRegistro2.Destroy;
begin
  inherited;
end;

{ TTrailer }

constructor TTrailer.Create;
begin
  FRegistroValido := True;
end;

destructor TTrailer.Destroy;
begin
  inherited;
end;

end.
