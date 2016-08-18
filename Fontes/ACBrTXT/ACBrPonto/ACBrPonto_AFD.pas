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

unit ACBrPonto_AFD;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils;

type
  // Registro tipo “1” - Cabeçalho
  TCabecalho = class
  private
    FCampo01: String; // “000000000”.
    FCampo02: String; // Tipo do registro, “1”.
    FCampo03: String; // Tipo de identificador do empregador, “1” para CNPJ ou “2” para CPF.
    FCampo04: String; // CNPJ ou CPF do empregador.
    FCampo05: String; // CEI do empregador, quando existir.
    FCampo06: String; // Razão social ou nome do empregador.
    FCampo07: String; // Número de fabricação do REP.
    FCampo08: String; // Data inicial dos registros no arquivo, no formato “ddmmaaaa”.
    FCampo09: String; // Data final dos registros no arquivo, no formato “ddmmaaaa”.
    FCampo10: String; // Data de geração do arquivo, no formato “ddmmaaaa”.
    FCampo11: String; // Horário da geração do arquivo, no formato “hhmm”.

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
    property Campo11: String read FCampo11 write FCampo11;
  end;

  // Registro 2 - Registro de inclusão ou alteração da identificação da empresa no REP
  TRegistro2 = class
  private
    FCampo01: String; // NSR.
    FCampo02: String; // Tipo do registro, “2”.
    FCampo03: String; // Data da gravação, no formata “ddmmaaaa”.
    FCampo04: String; // Horário da gravação, no formato “hhmm”
    FCampo05: String; // Tipo de identificador do empregador, “1” para CNPJ ou “2” para CPF.
    FCampo06: String; // CNPJ ou CPF do empregador.
    FCampo07: String; // CEI do empregador, quando existir.
    FCampo08: String; // Razão social ou nome do empregador.
    FCampo09: String; // Local de prestação de serviços.

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

  // Registro 3 - Registro de marcação de ponto
  TRegistro3 = class
  private
    FCampo01: String; // NSR.
    FCampo02: String; // tipo do registro, “3”.
    FCampo03: String; // Data da marcação de ponto, no formato “ddmmaaaa”.
    FCampo04: String; // Horário da gravação, no formato “hhmm”
    FCampo05: String; // Número do PIS do empregado.

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
  end;

  // Registro 3 - Lista
  TRegistro3List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro3;
    procedure SetItem(Index: Integer; const Value: TRegistro3);
  public
    function New: TRegistro3;
    property Items[Index: Integer]: TRegistro3 read GetItem write SetItem;
  end;
  
  // Registro 4 - Registro de ajuste do relógio de tempo real do REP
  TRegistro4 = class
  private
    FCampo01: String; // NSR.
    FCampo02: String; // Tipo do registro, “4”.
    FCampo03: String; // Data antes do ajuste, no formato “ddmmaaaa”.
    FCampo04: String; // Horário antes do ajuste, no formato “hhmm”.
    FCampo05: String; // Data ajustada, no formato “ddmmaaaa”.
    FCampo06: String; // Horário ajustado, no formato “hhmm”.

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
  end;

  // Registro 4 - Lista
  TRegistro4List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro4;
    procedure SetItem(Index: Integer; const Value: TRegistro4);
  public
    function New: TRegistro4;
    property Items[Index: Integer]: TRegistro4 read GetItem write SetItem;
  end;

  // Registro 5 - Registro de marcação de ponto
  TRegistro5 = class
  private
    FCampo01: String; // NSR.
    FCampo02: String; // Tipo do registro, “5”.
    FCampo03: String; // Data da gravação do registro, no formato “ddmmaaaa”.
    FCampo04: String; // Horário da gravação do registro, no formato “hhmm”.
    FCampo05: String; // Tipo de operação, “I” para inclusão, “A” para alteração e “E” para exclusão.
    FCampo06: String; // Número do PIS do empregado.
    FCampo07: String; // Nome do empregado.

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
  end;

  // Registro 5 - Lista
  TRegistro5List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro5;
    procedure SetItem(Index: Integer; const Value: TRegistro5);
  public
    function New: TRegistro5;
    property Items[Index: Integer]: TRegistro5 read GetItem write SetItem;
  end;

  // Registro Trailer
  TTrailer = class
  private
    FCampo01: String; // “999999999”.
    FCampo02: Integer; // Quantidade de registros tipo “2” no arquivo.
    FCampo03: Integer; // Quantidade de registros tipo “3” no arquivo.
    FCampo04: Integer; // Quantidade de registros tipo “4” no arquivo.
    FCampo05: Integer; // Quantidade de registros tipo “5” no arquivo.
    FCampo06: String; // Tipo do registro, “9”.

    FRegistroValido: boolean;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property Campo01: String read FCampo01 write FCampo01;
    property Campo02: Integer read FCampo02 write FCampo02;
    property Campo03: Integer read FCampo03 write FCampo03;
    property Campo04: Integer read FCampo04 write FCampo04;
    property Campo05: Integer read FCampo05 write FCampo05;
    property Campo06: String read FCampo06 write FCampo06;
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

{ TRegistro3List }

function TRegistro3List.GetItem(Index: Integer): TRegistro3;
begin
  Result := TRegistro3(inherited Items[Index]);
end;

function TRegistro3List.New: TRegistro3;
begin
  Result := TRegistro3.Create;
  Add(Result);
end;

procedure TRegistro3List.SetItem(Index: Integer; const Value: TRegistro3);
begin
  Put(Index, Value);
end;

{ TRegistro3 }

constructor TRegistro3.Create;
begin
  FRegistroValido := True;
end;

destructor TRegistro3.Destroy;
begin
  inherited;
end;

{ TRegistro4List }

function TRegistro4List.GetItem(Index: Integer): TRegistro4;
begin
  Result := TRegistro4(inherited Items[Index]);
end;

function TRegistro4List.New: TRegistro4;
begin
  Result := TRegistro4.Create;
  Add(Result);
end;

procedure TRegistro4List.SetItem(Index: Integer; const Value: TRegistro4);
begin
  Put(Index, Value);
end;

{ TRegistro4 }

constructor TRegistro4.Create;
begin
  FRegistroValido := True;
end;

destructor TRegistro4.Destroy;
begin
  inherited;
end;

{ TRegistro5List }

function TRegistro5List.GetItem(Index: Integer): TRegistro5;
begin
  Result := TRegistro5(inherited Items[Index]);
end;

function TRegistro5List.New: TRegistro5;
begin
  Result := TRegistro5.Create;
  Add(Result);
end;

procedure TRegistro5List.SetItem(Index: Integer; const Value: TRegistro5);
begin
  Put(Index, Value);
end;

{ TRegistro5 }

constructor TRegistro5.Create;
begin
  FRegistroValido := True;
end;

destructor TRegistro5.Destroy;
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
