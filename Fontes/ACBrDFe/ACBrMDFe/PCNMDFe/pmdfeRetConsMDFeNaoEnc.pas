{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pmdfeRetConsMDFeNaoEnc;

interface

uses
  SysUtils, Classes, Contnrs,
  pcnAuxiliar, pcnConversao, pcnLeitor;

type

  TRetInfMDFeCollectionItem = class;

  TRetInfMDFeCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRetInfMDFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetInfMDFeCollectionItem);
  public
    function Add: TRetInfMDFeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetInfMDFeCollectionItem;
    property Items[Index: Integer]: TRetInfMDFeCollectionItem read GetItem write SetItem; default;
  end;

  TRetInfMDFeCollectionItem = class(TObject)
  private
    FchMDFe: String;
    FnProt: String;
  public
    property chMDFe: String read FchMDFe write FchMDFe;
    property nProt: String  read FnProt  write FnProt;
  end;

  TRetConsMDFeNaoEnc = class(TObject)
  private
    FLeitor: TLeitor;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FInfMDFe: TRetInfMDFeCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
    property Leitor: TLeitor                read FLeitor   write FLeitor;
    property versao: String                 read Fversao   write Fversao;
    property tpAmb: TpcnTipoAmbiente        read FtpAmb    write FtpAmb;
    property verAplic: String               read FverAplic write FverAplic;
    property cStat: Integer                 read FcStat    write FcStat;
    property xMotivo: String                read FxMotivo  write FxMotivo;
    property cUF: Integer                   read FcUF      write FcUF;
    property InfMDFe: TRetInfMDFeCollection read FInfMDFe  write FInfMDFe;
  end;

implementation

{ TRetConsMDFeNaoEnc }

constructor TRetConsMDFeNaoEnc.Create;
begin
  inherited Create;
  FLeitor := TLeitor.Create;
end;

destructor TRetConsMDFeNaoEnc.Destroy;
begin
  FLeitor.Free;

  if Assigned(InfMDFe) then
    InfMDFe.Free;
  inherited;
end;

function TRetConsMDFeNaoEnc.LerXml: boolean;
var
  ok: boolean;
  i: Integer;
begin
  Result := False;
  
  try
    if leitor.rExtrai(1, 'retConsMDFeNaoEnc') <> '' then
    begin
      Fversao   := Leitor.rAtributo('versao', 'retConsMDFeNaoEnc');
      FtpAmb    := StrToTpAmb(ok, leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic := leitor.rCampo(tcStr, 'verAplic');
      FcStat    := leitor.rCampo(tcInt, 'cStat');
      FxMotivo  := leitor.rCampo(tcStr, 'xMotivo');
      FcUF      := leitor.rCampo(tcInt, 'cUF');

      if Assigned(InfMDFe) then
        InfMDFe.Free;

      InfMDFe := TRetInfMDFeCollection.Create;
      i := 0;
      while Leitor.rExtrai(1, 'infMDFe', '', i + 1) <> '' do
      begin
        InfMDFe.New;
        InfMDFe.Items[i].FchMDFe := leitor.rCampo(tcStr, 'chMDFe');
        InfMDFe.Items[i].FnProt  := leitor.rCampo(tcStr, 'nProt');
        inc(i);
      end;

      Result := True;
    end;

  except
    Result := False;
  end;
end;

{ TRetInfMDFeCollection }

function TRetInfMDFeCollection.Add: TRetInfMDFeCollectionItem;
begin
  Result := Self.New;
end;

function TRetInfMDFeCollection.GetItem(Index: Integer): TRetInfMDFeCollectionItem;
begin
  Result := TRetInfMDFeCollectionItem(inherited GetItem(Index));
end;

procedure TRetInfMDFeCollection.SetItem(Index: Integer;
  Value: TRetInfMDFeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TRetInfMDFeCollection.New: TRetInfMDFeCollectionItem;
begin
  Result := TRetInfMDFeCollectionItem.Create;
  Self.Add(Result);
end;

end.

