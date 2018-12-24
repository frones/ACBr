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
|* 25/10/2012: Régys Borges da Silveira
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrPAF_TITP;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils;

type
  TTITP_Base = class
  private
    FDescricao: String;
    FCodigo: String;
    FAliquota: Double;
    FUnidade: String;
    FQuantidade: Double;
    FEan: String;
    FCST: String;
    FVlrUnitario: Double;
  public
    property Codigo: String      read FCodigo      write FCodigo;
    property Ean: String         read FEan         write FEan;
    property Descricao: String   read FDescricao   write FDescricao;
    property Unidade: String     read FUnidade     write FUnidade;
    property CST: String         read FCST         write FCST;
    property Aliquota: Double    read FAliquota    write FAliquota;
    property VlrUnitario: Double read FVlrUnitario write FVlrUnitario;
    property Quantidade: Double  read FQuantidade  write FQuantidade;
  end;

  TTITP_Insumo = class(TTITP_Base)
  private

  public

  end;

  TTITP_Insumos = class(TObjectList)
  private
    function GetItem(Index: Integer): TTITP_Insumo;
    procedure SetItem(Index: Integer; const Value: TTITP_Insumo);
  public
    function New: TTITP_Insumo;
    property Items[Index: Integer]: TTITP_Insumo read GetItem write SetItem; default;
  end;

  TTITP_Mercadoria = class(TTITP_Base)
  private
    FInsumos: TTITP_Insumos;
  public
    constructor Create;
    destructor Destroy; override;
    property Insumos: TTITP_Insumos read FInsumos write FInsumos;
  end;

  TTITP_Mercadorias = class(TObjectList)
  private
    function GetItem(Index: Integer): TTITP_Mercadoria;
    procedure SetItem(Index: Integer; const Value: TTITP_Mercadoria);
  public
    function New: TTITP_Mercadoria;
    property Items[Index: Integer]: TTITP_Mercadoria read GetItem write SetItem; default;
  end;

implementation

{ TTITP_Mercadorias }

function TTITP_Mercadorias.GetItem(Index: Integer): TTITP_Mercadoria;
begin
  Result := TTITP_Mercadoria(inherited Items[Index]);
end;

function TTITP_Mercadorias.New: TTITP_Mercadoria;
begin
  Result := TTITP_Mercadoria.Create;
  Add(Result);
end;

procedure TTITP_Mercadorias.SetItem(Index: Integer;
  const Value: TTITP_Mercadoria);
begin
  Put(Index, Value);
end;

{ TTITP_Insumos }

function TTITP_Insumos.GetItem(Index: Integer): TTITP_Insumo;
begin
  Result := TTITP_Insumo(inherited Items[Index]);
end;

function TTITP_Insumos.New: TTITP_Insumo;
begin
  Result := TTITP_Insumo.Create;
  Add(Result);
end;

procedure TTITP_Insumos.SetItem(Index: Integer; const Value: TTITP_Insumo);
begin
  Put(Index, Value);
end;

{ TTITP_Mercadoria }

constructor TTITP_Mercadoria.Create;
begin
  inherited Create;
  FInsumos := TTITP_Insumos.Create;
end;

destructor TTITP_Mercadoria.Destroy;
begin
  FreeAndNil(FInsumos);
  inherited;
end;

end.

