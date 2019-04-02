{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}

{$I ACBr.inc}

unit pgnreRetProduto;

interface

uses
  SysUtils, Classes, pcnAuxiliar, pcnConversao, pcnLeitor,
  pgnreConfigUF;

type
  TRetInfProdutoCollection = class;
  TRetInfProdutoCollectionItem = class;
  TRetProduto = class;

  TRetInfProdutoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRetInfProdutoCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetInfProdutoCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TRetInfProdutoCollectionItem;
    property Items[Index: Integer]: TRetInfProdutoCollectionItem read GetItem write SetItem; default;
  end;

  TRetInfProdutoCollectionItem = class(TCollectionItem)
  private
    FRetProduto: TRetInfProduto;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property RetProduto: TRetInfProduto read FRetProduto write FRetProduto;
  end;

  TRetProduto = class(TPersistent)
  private
    FLeitor: TLeitor;
    FretProduto: TRetInfProdutoCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property retProduto: TRetInfProdutoCollection read FretProduto write FretProduto;
  end;

implementation

{ TRetInfProdutoCollection }

function TRetInfProdutoCollection.Add: TRetInfProdutoCollectionItem;
begin
  Result := TRetInfProdutoCollectionItem(inherited Add);
  Result.Create;
end;

constructor TRetInfProdutoCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TRetInfProdutoCollectionItem);
end;

function TRetInfProdutoCollection.GetItem(
  Index: Integer): TRetInfProdutoCollectionItem;
begin
  Result := TRetInfProdutoCollectionItem(inherited GetItem(Index));
end;

procedure TRetInfProdutoCollection.SetItem(Index: Integer;
  Value: TRetInfProdutoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TRetInfProdutoCollectionItem }

constructor TRetInfProdutoCollectionItem.Create;
begin
  FRetProduto := TRetInfProduto.Create;
end;

destructor TRetInfProdutoCollectionItem.Destroy;
begin
  FRetProduto.Free;
  inherited;
end;

{ TRetProduto }

constructor TRetProduto.Create;
begin
  FLeitor := TLeitor.Create;
  FretProduto := TRetInfProdutoCollection.Create(Self);
end;

destructor TRetProduto.Destroy;
begin
  FLeitor.Free;
  FretProduto.Free;
  inherited;
end;

function TRetProduto.LerXml: Boolean;
var i: Integer;
begin
  Result := False;
  try
    i := 0;
    if Leitor.rExtrai(1, 'ns1:produtos') <> '' then
    begin
      while Leitor.rExtrai(2, 'ns1:produto', '', i + 1) <> '' do
      begin
        retProduto.Add;
        retProduto.Items[i].RetProduto.codigo    := Leitor.rCampo(tcInt, 'ns1:codigo');
        retProduto.Items[i].RetProduto.descricao := Leitor.rCampo(tcStr, 'ns1:descricao');
        inc(i);
      end;

      if i = 0 then
        retProduto.Add;

      Result := True;
    end;
  except
    Result := false;
  end;
end;

end.
 