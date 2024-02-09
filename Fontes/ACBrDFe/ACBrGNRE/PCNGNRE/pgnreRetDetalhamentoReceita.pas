{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Claudemir Vitor Pereira                         }
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

{$I ACBr.inc}

unit pgnreRetDetalhamentoReceita;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnLeitor, pgnreConfigUF;

type
  TRetInfDetalhamentoReceitaCollection = class;
  TRetInfDetalhamentoReceitaCollectionItem = class;
  TRetDetalhamentoReceita = class;

  TRetInfDetalhamentoReceitaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetInfDetalhamentoReceitaCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetInfDetalhamentoReceitaCollectionItem);
  public
    function Add: TRetInfDetalhamentoReceitaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetInfDetalhamentoReceitaCollectionItem;
    property Items[Index: Integer]: TRetInfDetalhamentoReceitaCollectionItem read GetItem write SetItem; default;
  end;

  TRetInfDetalhamentoReceitaCollectionItem = class(TObject)
  private
    FRetDetalhamentoReceita: TRetInfDetalhamentoReceita;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property RetDetalhamentoReceita: TRetInfDetalhamentoReceita read FRetDetalhamentoReceita write FRetDetalhamentoReceita;
  end;

  TRetDetalhamentoReceita = class(TObject)
  private
    FLeitor: TLeitor;
    FretDetalhamentoReceita: TRetInfDetalhamentoReceitaCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;

    property Leitor: TLeitor read FLeitor write FLeitor;
    property retDetalhamentoReceita: TRetInfDetalhamentoReceitaCollection read FretDetalhamentoReceita write FretDetalhamentoReceita;
  end;

implementation

{ TRetInfDetalhamentoReceitaCollection }

function TRetInfDetalhamentoReceitaCollection.Add: TRetInfDetalhamentoReceitaCollectionItem;
begin
  Result := Self.New;
end;

function TRetInfDetalhamentoReceitaCollection.GetItem(
  Index: Integer): TRetInfDetalhamentoReceitaCollectionItem;
begin
  Result := TRetInfDetalhamentoReceitaCollectionItem(inherited Items[Index]);
end;

function TRetInfDetalhamentoReceitaCollection.New: TRetInfDetalhamentoReceitaCollectionItem;
begin
  Result := TRetInfDetalhamentoReceitaCollectionItem.Create();
  Self.Add(Result);
end;

procedure TRetInfDetalhamentoReceitaCollection.SetItem(Index: Integer;
  Value: TRetInfDetalhamentoReceitaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TRetInfDetalhamentoReceitaCollectionItem }

constructor TRetInfDetalhamentoReceitaCollectionItem.Create;
begin
  FRetDetalhamentoReceita := TRetInfDetalhamentoReceita.Create;
end;

destructor TRetInfDetalhamentoReceitaCollectionItem.Destroy;
begin
  FRetDetalhamentoReceita.Free;

  inherited;
end;

{ TRetDetalhamentoReceita }

constructor TRetDetalhamentoReceita.Create;
begin
  FLeitor := TLeitor.Create;
  FretDetalhamentoReceita := TRetInfDetalhamentoReceitaCollection.Create;
end;

destructor TRetDetalhamentoReceita.Destroy;
begin
  FLeitor.Free;
  FretDetalhamentoReceita.Free;

  inherited;
end;

function TRetDetalhamentoReceita.LerXml: Boolean;
var i: Integer;
begin
  Result := False;

  try
    i := 0;
    if Leitor.rExtrai(1, 'ns1:detalhamentosReceita') <> '' then
    begin
      while Leitor.rExtrai(2, 'ns1:detalhamentoReceita', '', i + 1) <> '' do
      begin
        retDetalhamentoReceita.New;
        retDetalhamentoReceita.Items[i].RetDetalhamentoReceita.codigo    := Leitor.rCampo(tcInt, 'ns1:codigo');
        retDetalhamentoReceita.Items[i].RetDetalhamentoReceita.descricao := Leitor.rCampo(tcStr, 'ns1:descricao');
        inc(i);
      end;

      if i = 0 then
        retDetalhamentoReceita.New;

      Result := True;       
    end;
  except
    Result := false;
  end;
end;

end.
