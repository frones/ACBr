{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

(*

  Documentação:
  https://github.com/bacen/pix-api

*)

{$I ACBr.inc}

unit ACBrPIXSchemasPaginacao;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrPIXBase;

type

  { TACBrPIXPaginacao }

  TACBrPIXPaginacao = class(TACBrPIXSchema)
  private
    fitensPorPagina: Integer;
    fpaginaAtual: Integer;
    fquantidadeDePaginas: Integer;
    fquantidadeTotalDeItens: Integer;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXPaginacao);

    property paginaAtual: Integer read fpaginaAtual write fpaginaAtual;
    property itensPorPagina: Integer read fitensPorPagina write fitensPorPagina;
    property quantidadeDePaginas: Integer read fquantidadeDePaginas write fquantidadeDePaginas;
    property quantidadeTotalDeItens: Integer read fquantidadeTotalDeItens write fquantidadeTotalDeItens;
  end;

implementation

{ TACBrPIXPaginacao }

constructor TACBrPIXPaginacao.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXPaginacao.Clear;
begin
  fitensPorPagina := 0;
  fpaginaAtual := 0;
  fquantidadeDePaginas := 0;
  fquantidadeTotalDeItens := 0;
end;

function TACBrPIXPaginacao.IsEmpty: Boolean;
begin
  Result := (fitensPorPagina = 0) and
            (fpaginaAtual = 0) and
            (fquantidadeDePaginas = 0) and
            (fquantidadeTotalDeItens = 0);
end;

procedure TACBrPIXPaginacao.Assign(Source: TACBrPIXPaginacao);
begin
  fitensPorPagina := Source.itensPorPagina;
  fpaginaAtual := Source.paginaAtual;
  fquantidadeDePaginas := Source.quantidadeDePaginas;
  fquantidadeTotalDeItens := Source.quantidadeTotalDeItens;
end;

procedure TACBrPIXPaginacao.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  aJson
    .AddPair('itensPorPagina', fitensPorPagina)
    .AddPair('paginaAtual', fpaginaAtual)
    .AddPair('quantidadeDePaginas', fquantidadeDePaginas)
    .AddPair('quantidadeTotalDeItens', fquantidadeTotalDeItens);
end;

procedure TACBrPIXPaginacao.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('itensPorPagina', fitensPorPagina)
    .Value('paginaAtual', fpaginaAtual)
    .Value('quantidadeDePaginas', fquantidadeDePaginas)
    .Value('quantidadeTotalDeItens', fquantidadeTotalDeItens);
end;

end.

