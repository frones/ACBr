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

unit pgnreRetTipoDocumentoOrigem;

interface

uses
  SysUtils, Classes, pcnAuxiliar, pcnConversao, pcnLeitor,
  pgnreConfigUF;

type
  TRetInfTipoDocumentoOrigemCollection = class;
  TRetInfTipoDocumentoOrigemCollectionItem = class;
  TRetTipoDocumentoOrigem = class;

  TRetInfTipoDocumentoOrigemCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRetInfTipoDocumentoOrigemCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetInfTipoDocumentoOrigemCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TRetInfTipoDocumentoOrigemCollectionItem;
    property Items[Index: Integer]: TRetInfTipoDocumentoOrigemCollectionItem read GetItem write SetItem; default;
  end;

  TRetInfTipoDocumentoOrigemCollectionItem = class(TCollectionItem)
  private
    FRetTipoDocumentoOrigem: TRetInfTipoDocumentoOrigem;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property RetTipoDocumentoOrigem: TRetInfTipoDocumentoOrigem read FRetTipoDocumentoOrigem write FRetTipoDocumentoOrigem;
  end;

  TRetTipoDocumentoOrigem = class(TPersistent)
  private
    FLeitor: TLeitor;
    FretTipoDocumentoOrigem: TRetInfTipoDocumentoOrigemCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property retTipoDocumentoOrigem: TRetInfTipoDocumentoOrigemCollection read FretTipoDocumentoOrigem write FretTipoDocumentoOrigem;
  end;

implementation

{ TRetInfTipoDocumentoOrigemCollection }

function TRetInfTipoDocumentoOrigemCollection.Add: TRetInfTipoDocumentoOrigemCollectionItem;
begin
  Result := TRetInfTipoDocumentoOrigemCollectionItem(inherited Add);
  Result.Create;
end;

constructor TRetInfTipoDocumentoOrigemCollection.Create(
  AOwner: TPersistent);
begin
  inherited Create(TRetInfTipoDocumentoOrigemCollectionItem);
end;

function TRetInfTipoDocumentoOrigemCollection.GetItem(
  Index: Integer): TRetInfTipoDocumentoOrigemCollectionItem;
begin
  Result := TRetInfTipoDocumentoOrigemCollectionItem(inherited GetItem(Index));
end;

procedure TRetInfTipoDocumentoOrigemCollection.SetItem(Index: Integer;
  Value: TRetInfTipoDocumentoOrigemCollectionItem);
begin
  inherited SetItem(Index, Value);
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
  FretTipoDocumentoOrigem := TRetInfTipoDocumentoOrigemCollection.Create(Self);
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
        retTipoDocumentoOrigem.Add;
        retTipoDocumentoOrigem.Items[i].RetTipoDocumentoOrigem.codigo    := Leitor.rCampo(tcInt, 'ns1:codigo');
        retTipoDocumentoOrigem.Items[i].RetTipoDocumentoOrigem.descricao := Leitor.rCampo(tcStr, 'ns1:descricao');
        inc(i);
      end;

      if i = 0 then
        retTipoDocumentoOrigem.Add;

      Result := True;
    end;
  except
    Result := false;
  end;
end;

end.
 