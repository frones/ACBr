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

unit pmdfeRetConsSitMDFe;

interface

uses
  SysUtils, Classes, Contnrs,
  pcnAuxiliar, pcnConversao, pcnLeitor,
  pmdfeProcMDFe, pmdfeRetEnvEventoMDFe;

type

  TRetEventoMDFeCollectionItem = class(TObject)
  private
    FRetEventoMDFe: TRetEventoMDFe;
  public
    constructor Create;
    destructor Destroy; override;
    property RetEventoMDFe: TRetEventoMDFe read FRetEventoMDFe write FRetEventoMDFe;
  end;

  TRetEventoMDFeCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRetEventoMDFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetEventoMDFeCollectionItem);
  public
    function Add: TRetEventoMDFeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetEventoMDFeCollectionItem;
    property Items[Index: Integer]: TRetEventoMDFeCollectionItem read GetItem write SetItem; default;
  end;

  TRetConsSitMDFe = class(TObject)
  private
    FLeitor: TLeitor;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FchMDFe: String;
    FprotMDFe: TProcMDFe;
    FprocEventoMDFe: TRetEventoMDFeCollection;
    FXMLprotMDFe: String;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
    property Leitor: TLeitor                          read FLeitor         write FLeitor;
    property versao: String                           read Fversao         write Fversao;
    property tpAmb: TpcnTipoAmbiente                  read FtpAmb          write FtpAmb;
    property verAplic: String                         read FverAplic       write FverAplic;
    property cStat: Integer                           read FcStat          write FcStat;
    property xMotivo: String                          read FxMotivo        write FxMotivo;
    property cUF: Integer                             read FcUF            write FcUF;
    property chMDFe: String                           read FchMDFe         write FchMDFe;
    property protMDFe: TProcMDFe                      read FprotMDFe       write FprotMDFe;
    property procEventoMDFe: TRetEventoMDFeCollection read FprocEventoMDFe write FprocEventoMDFe;
    property XMLprotMDFe: String                      read FXMLprotMDFe    write FXMLprotMDFe;
  end;

implementation

{ TRetConsSitMDFe }

constructor TRetConsSitMDFe.Create;
begin
  inherited Create;
  FLeitor   := TLeitor.Create;
  FprotMDFe := TProcMDFe.create;
end;

destructor TRetConsSitMDFe.Destroy;
begin
  FLeitor.Free;
  FprotMDFe.Free;
  if Assigned(procEventoMDFe) then
    procEventoMDFe.Free;
  inherited;
end;

function TRetConsSitMDFe.LerXml: boolean;
var
  ok: boolean;
  i: Integer;
begin
  Result := False;
  
  try
    if leitor.rExtrai(1, 'retConsSitMDFe') <> '' then
    begin
      Fversao   := Leitor.rAtributo('versao', 'retConsSitMDFe');
      FtpAmb    := StrToTpAmb(ok, leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic := leitor.rCampo(tcStr, 'verAplic');
      FcStat    := leitor.rCampo(tcInt, 'cStat');
      FxMotivo  := leitor.rCampo(tcStr, 'xMotivo');
      FcUF      := leitor.rCampo(tcInt, 'cUF');

      if FcStat in [100, 132] then
      begin
        if (Leitor.rExtrai(1, 'protMDFe') <> '') then
        begin
          // A propriedade XMLprotMDFe contem o XML que traz o resultado do
          // processamento do MDF-e.
          XMLprotMDFe := Leitor.Grupo;

          if Leitor.rExtrai(2, 'infProt') <> '' then
          begin
            protMDFe.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
            protMDFe.verAplic := Leitor.rCampo(tcStr, 'verAplic');
            protMDFe.chMDFe   := Leitor.rCampo(tcStr, 'chMDFe');
            protMDFe.dhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
            protMDFe.nProt    := Leitor.rCampo(tcStr, 'nProt');
            protMDFe.digVal   := Leitor.rCampo(tcStr, 'digVal');
            protMDFe.cStat    := Leitor.rCampo(tcInt, 'cStat');
            protMDFe.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
            FchMDFe           := protMDFe.chMDFe;
          end;
        end;
      end;

      if Assigned(procEventoMDFe) then
        procEventoMDFe.Free;
      procEventoMDFe := TRetEventoMDFeCollection.Create;
      i := 0;
      while Leitor.rExtrai(1, 'procEventoMDFe', '', i + 1) <> '' do
      begin
        procEventoMDFe.New;
        procEventoMDFe.Items[i].RetEventoMDFe.Leitor.Arquivo := Leitor.Grupo;
        procEventoMDFe.Items[i].RetEventoMDFe.XML := Leitor.Grupo;
        procEventoMDFe.Items[i].RetEventoMDFe.LerXml;
        inc(i);
      end;

      Result := True;
    end;

  except
    Result := False;
  end;
end;

{ TRetEventoMDFeCollection }

function TRetEventoMDFeCollection.Add: TRetEventoMDFeCollectionItem;
begin
  Result := Self.New;
end;

function TRetEventoMDFeCollection.GetItem(Index: Integer): TRetEventoMDFeCollectionItem;
begin
  Result := TRetEventoMDFeCollectionItem(inherited GetItem(Index));
end;

procedure TRetEventoMDFeCollection.SetItem(Index: Integer;
  Value: TRetEventoMDFeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TRetEventoMDFeCollection.New: TRetEventoMDFeCollectionItem;
begin
  Result := TRetEventoMDFeCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetEventoMDFeCollectionItem }

constructor TRetEventoMDFeCollectionItem.Create;
begin
  inherited Create;
  FRetEventoMDFe := TRetEventoMDFe.Create;
end;

destructor TRetEventoMDFeCollectionItem.Destroy;
begin
  FRetEventoMDFe.Free;
  inherited;
end;

end.

