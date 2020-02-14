{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pcnRetConsSitBPe;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcnLeitor, pcnProcBPe, pcnRetEnvEventoBPe;

type

  TRetEventoBPeCollectionItem = class;

  TRetEventoBPeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetEventoBPeCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetEventoBPeCollectionItem);
  public
    function Add: TRetEventoBPeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetEventoBPeCollectionItem;
    property Items[Index: Integer]: TRetEventoBPeCollectionItem read GetItem write SetItem; default;
  end;

  TRetEventoBPeCollectionItem = class(TObject)
  private
    FRetEventoBPe: TRetEventoBPe;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property RetEventoBPe: TRetEventoBPe read FRetEventoBPe write FRetEventoBPe;
  end;

  TRetConsSitBPe = class(TObject)
  private
    FLeitor: TLeitor;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FchBPe: String;
    FprotBPe: TProcBPe;
    FprocEventoBPe: TRetEventoBPeCollection;
    FXMLprotBPe: String;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
    property Leitor: TLeitor                        read FLeitor        write FLeitor;
    property versao: String                         read Fversao        write Fversao;
    property tpAmb: TpcnTipoAmbiente                read FtpAmb         write FtpAmb;
    property verAplic: String                       read FverAplic      write FverAplic;
    property cStat: Integer                         read FcStat         write FcStat;
    property xMotivo: String                        read FxMotivo       write FxMotivo;
    property cUF: Integer                           read FcUF           write FcUF;
    property chBPe: String                          read FchBPe         write FchBPe;
    property protBPe: TProcBPe                      read FprotBPe       write FprotBPe;
    property procEventoBPe: TRetEventoBPeCollection read FprocEventoBPe write FprocEventoBPe;
    property XMLprotBPe: String                     read FXMLprotBPe    write FXMLprotBPe;
  end;

implementation

{ TRetConsSitBPe }

constructor TRetConsSitBPe.Create;
begin
  FLeitor  := TLeitor.Create;
  FprotBPe := TProcBPe.create;
end;

destructor TRetConsSitBPe.Destroy;
begin
  FLeitor.Free;
  FprotBPe.Free;
  if Assigned(procEventoBPe) then
    procEventoBPe.Free;

  inherited;
end;

function TRetConsSitBPe.LerXml: Boolean;
var
  ok: Boolean;
  i: Integer;
begin
  Result := False;
  try
    if leitor.rExtrai(1, 'retConsSitBPe') <> '' then
    begin
      Fversao   := Leitor.rAtributo('versao');
      FtpAmb    := StrToTpAmb(ok, leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic := leitor.rCampo(tcStr, 'verAplic');
      FcStat    := leitor.rCampo(tcInt, 'cStat');
      FxMotivo  := leitor.rCampo(tcStr, 'xMotivo');
      FcUF      := leitor.rCampo(tcInt, 'cUF');

      case FcStat of
        100,101,102,104,110,150,151,155,301,302,303:
           begin
             if (Leitor.rExtrai(1, 'protBPe') <> '') then
             begin
               // A propriedade XMLprotBPe contem o XML que traz o resultado do
               // processamento do BP-e.
               XMLprotBPe := Leitor.Grupo;

               if Leitor.rExtrai(2, 'infProt') <> '' then
               begin
                 protBPe.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
                 protBPe.verAplic := Leitor.rCampo(tcStr, 'verAplic');
                 protBPe.chBPe    := Leitor.rCampo(tcStr, 'chBPe');
                 protBPe.dhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
                 protBPe.nProt    := Leitor.rCampo(tcStr, 'nProt');
                 protBPe.digVal   := Leitor.rCampo(tcStr, 'digVal');
                 protBPe.cStat    := Leitor.rCampo(tcInt, 'cStat');
                 protBPe.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
                 protBPe.cMsg     := Leitor.rCampo(tcInt, 'cMsg');
                 protBPe.xMsg     := Leitor.rCampo(tcStr, 'xMsg');
                 FchBPe           := protBPe.chBPe;
               end;
             end;
           end;
      end;

      if Assigned(procEventoBPe) then
        procEventoBPe.Free;

      procEventoBPe := TRetEventoBPeCollection.Create;
      i:=0;
      while Leitor.rExtrai(1, 'procEventoBPe', '', i + 1) <> '' do
      begin
        procEventoBPe.New;
        procEventoBPe.Items[i].RetEventoBPe.Leitor.Arquivo := Leitor.Grupo;
        procEventoBPe.Items[i].RetEventoBPe.XML            := Leitor.Grupo; 
        procEventoBPe.Items[i].RetEventoBPe.LerXml;
        inc(i);
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TRetEventoCollection }

function TRetEventoBPeCollection.Add: TRetEventoBPeCollectionItem;
begin
  Result := Self.New;
end;

function TRetEventoBPeCollection.GetItem(Index: Integer): TRetEventoBPeCollectionItem;
begin
  Result := TRetEventoBPeCollectionItem(inherited GetItem(Index));
end;

procedure TRetEventoBPeCollection.SetItem(Index: Integer;
  Value: TRetEventoBPeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TRetEventoCollectionItem }

constructor TRetEventoBPeCollectionItem.Create;
begin
  FRetEventoBPe := TRetEventoBPe.Create;
end;

destructor TRetEventoBPeCollectionItem.Destroy;
begin
  FRetEventoBPe.Free;
  inherited;
end;

function TRetEventoBPeCollection.New: TRetEventoBPeCollectionItem;
begin
  Result := TRetEventoBPeCollectionItem.Create;
  Self.Add(Result);
end;

end.

