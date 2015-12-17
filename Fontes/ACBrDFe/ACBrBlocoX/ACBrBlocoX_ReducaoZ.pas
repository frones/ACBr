{******************************************************************************}
{ Projeto: Componente ACBrBlocoX                                               }
{ Biblioteca multiplataforma de componentes Delphi para Geração de arquivos    }
{ do Bloco X                                                                   }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{******************************************************************************}

unit ACBrBlocoX_ReducaoZ;

interface

uses
  ACBrBlocoX_Comum, Classes, SysUtils, StrUtils;

type
  TACBrBlocoX_ReducaoZItem = class(TCollectionItem)

  end;

  TACBrBlocoX_ReducaoZ = class(TACBrBlocoX_Base)
  private
    function GetItem(Index: integer): TACBrBlocoX_ReducaoZItem;
    procedure SetItem(Index: integer; const Value: TACBrBlocoX_ReducaoZItem);

  public
    function Add: TACBrBlocoX_ReducaoZItem;
    function Insert(Index: integer): TACBrBlocoX_ReducaoZItem;

    property Items[Index: integer]: TACBrBlocoX_ReducaoZItem read GetItem write SetItem; default;

    procedure SaveToFile(const AXmlFileName: string); override;
  end;

implementation

{ TACBrBlocoX_ReducaoZ }

function TACBrBlocoX_ReducaoZ.Add: TACBrBlocoX_ReducaoZItem;
begin
  Result := TACBrBlocoX_ReducaoZItem(inherited Add);
end;

function TACBrBlocoX_ReducaoZ.GetItem(Index: integer): TACBrBlocoX_ReducaoZItem;
begin
  Result := TACBrBlocoX_ReducaoZItem(inherited Items[Index]);
end;

function TACBrBlocoX_ReducaoZ.Insert(Index: integer): TACBrBlocoX_ReducaoZItem;
begin
  Result := TACBrBlocoX_ReducaoZItem(inherited Insert(Index));
end;

procedure TACBrBlocoX_ReducaoZ.SetItem(Index: integer;
  const Value: TACBrBlocoX_ReducaoZItem);
begin
  Items[Index].Assign(Value);
end;

procedure TACBrBlocoX_ReducaoZ.SaveToFile(const AXmlFileName: string);
begin

end;

end.
