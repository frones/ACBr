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

unit pgnreRetTipoDocumentoOrigem;

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
  TRetInfTipoDocumentoOrigemCollection = class;
  TRetInfTipoDocumentoOrigemCollectionItem = class;
  TRetTipoDocumentoOrigem = class;

  TRetInfTipoDocumentoOrigemCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetInfTipoDocumentoOrigemCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetInfTipoDocumentoOrigemCollectionItem);
  public
    function Add: TRetInfTipoDocumentoOrigemCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetInfTipoDocumentoOrigemCollectionItem;
    property Items[Index: Integer]: TRetInfTipoDocumentoOrigemCollectionItem read GetItem write SetItem; default;
  end;

  TRetInfTipoDocumentoOrigemCollectionItem = class(TObject)
  private
    FRetTipoDocumentoOrigem: TRetInfTipoDocumentoOrigem;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property RetTipoDocumentoOrigem: TRetInfTipoDocumentoOrigem read FRetTipoDocumentoOrigem write FRetTipoDocumentoOrigem;
  end;

  TRetTipoDocumentoOrigem = class(TObject)
  private
    FLeitor: TLeitor;
    FretTipoDocumentoOrigem: TRetInfTipoDocumentoOrigemCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;

    property Leitor: TLeitor read FLeitor write FLeitor;
    property retTipoDocumentoOrigem: TRetInfTipoDocumentoOrigemCollection read FretTipoDocumentoOrigem write FretTipoDocumentoOrigem;
  end;

implementation

{ TRetInfTipoDocumentoOrigemCollection }

function TRetInfTipoDocumentoOrigemCollection.Add: TRetInfTipoDocumentoOrigemCollectionItem;
begin
  Result := Self.New;
end;

function TRetInfTipoDocumentoOrigemCollection.GetItem(
  Index: Integer): TRetInfTipoDocumentoOrigemCollectionItem;
begin
  Result := TRetInfTipoDocumentoOrigemCollectionItem(inherited Items[Index]);
end;

function TRetInfTipoDocumentoOrigemCollection.New: TRetInfTipoDocumentoOrigemCollectionItem;
begin
  Result := TRetInfTipoDocumentoOrigemCollectionItem.Create();
  Self.Add(Result);
end;

procedure TRetInfTipoDocumentoOrigemCollection.SetItem(Index: Integer;
  Value: TRetInfTipoDocumentoOrigemCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TRetInfTipoDocumentoOrigemCollectionItem }

constructor TRetInfTipoDocumentoOrigemCollectionItem.Create;
begin
  FRetTipoDocumentoOrigem := TRetInfTipoDocumentoOrigem.Create;
end;

destructor TRetInfTipoDocumentoOrigemCollectionItem.Destroy;
begin
  FRetTipoDocumentoOrigem.Free;

  inherited;
end;

{ TRetTipoDocumentoOrigem }

constructor TRetTipoDocumentoOrigem.Create;
begin
  FLeitor := TLeitor.Create;
  FretTipoDocumentoOrigem := TRetInfTipoDocumentoOrigemCollection.Create;
end;

destructor TRetTipoDocumentoOrigem.Destroy;
begin
  FLeitor.Free;
  FretTipoDocumentoOrigem.Free;

  inherited;
end;

function TRetTipoDocumentoOrigem.LerXml: Boolean;
var i: Integer;
begin
  Result := False;

  try
    i := 0;
    if Leitor.rExtrai(1, 'ns1:tiposDocumentosOrigem') <> '' then
    begin
      while Leitor.rExtrai(2, 'ns1:tipoDocumentoOrigem', '', i + 1) <> '' do
      begin
        retTipoDocumentoOrigem.New;
        retTipoDocumentoOrigem.Items[i].RetTipoDocumentoOrigem.codigo    := Leitor.rCampo(tcInt, 'ns1:codigo');
        retTipoDocumentoOrigem.Items[i].RetTipoDocumentoOrigem.descricao := Leitor.rCampo(tcStr, 'ns1:descricao');
        inc(i);
      end;

      if i = 0 then
        retTipoDocumentoOrigem.New;

      Result := True;
    end;
  except
    Result := false;
  end;
end;

end.
