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
  Classes, SysUtils,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr
  {$Else}
   Jsons
  {$EndIf},
  ACBrPIXBase;

type

  { TACBrPIXPaginacao }

  TACBrPIXPaginacao = class(TACBrPIXSchema)
  private
    fitensPorPagina: Integer;
    fpaginaAtual: Integer;
    fquantidadeDePaginas: Integer;
    fquantidadeTotalDeItens: Integer;
  public
    constructor Create;
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXPaginacao);

    property paginaAtual: Integer read fpaginaAtual write fpaginaAtual;
    property itensPorPagina: Integer read fitensPorPagina write fitensPorPagina;
    property quantidadeDePaginas: Integer read fquantidadeDePaginas write fquantidadeDePaginas;
    property quantidadeTotalDeItens: Integer read fquantidadeTotalDeItens write fquantidadeTotalDeItens;

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;

implementation

{ TACBrPIXPaginacao }

constructor TACBrPIXPaginacao.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPIXPaginacao.Clear;
begin
  fitensPorPagina := 0;
  fpaginaAtual := 0;
  fquantidadeDePaginas := 0;
  fquantidadeTotalDeItens := 0;
end;

procedure TACBrPIXPaginacao.Assign(Source: TACBrPIXPaginacao);
begin
  fitensPorPagina := Source.itensPorPagina;
  fpaginaAtual := Source.paginaAtual;
  fquantidadeDePaginas := Source.quantidadeDePaginas;
  fquantidadeTotalDeItens := Source.quantidadeTotalDeItens;
end;

procedure TACBrPIXPaginacao.WriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.I['itensPorPagina'] := fitensPorPagina;
   AJSon.I['paginaAtual'] := fpaginaAtual;
   AJSon.I['quantidadeDePaginas'] := fquantidadeDePaginas;
   AJSon.I['quantidadeTotalDeItens'] := fquantidadeTotalDeItens;
  {$Else}
   AJSon['itensPorPagina'].AsInteger := fitensPorPagina;
   AJSon['paginaAtual'].AsInteger := fpaginaAtual;
   AJSon['quantidadeDePaginas'].AsInteger := fquantidadeDePaginas;
   AJSon['quantidadeTotalDeItens'].AsInteger := fquantidadeTotalDeItens;
  {$EndIf}
end;

procedure TACBrPIXPaginacao.ReadFromJSon(AJSon: TJsonObject);
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fitensPorPagina := AJSon.I['itensPorPagina'];
   fpaginaAtual := AJSon.I['paginaAtual'];
   fquantidadeDePaginas := AJSon.I['quantidadeDePaginas'];
   fquantidadeTotalDeItens := AJSon.I['quantidadeTotalDeItens'];
  {$Else}
   fitensPorPagina := AJSon['itensPorPagina'].AsInteger;
   fpaginaAtual := AJSon['paginaAtual'].AsInteger;
   fquantidadeDePaginas := AJSon['quantidadeDePaginas'].AsInteger;
   fquantidadeTotalDeItens := AJSon['quantidadeTotalDeItens'].AsInteger;
  {$EndIf}
end;

end.

