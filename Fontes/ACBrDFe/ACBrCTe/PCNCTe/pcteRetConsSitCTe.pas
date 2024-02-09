{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit pcteRetConsSitCTe;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcnLeitor, pcteProcCTe, pcteRetEnvEventoCTe;

type

  TRetEventoCTeCollectionItem = class;
  TRetConsSitCTe = class;

  TRetCancCTe = class(TObject)
  private
    Fversao: String;
    FId: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FchCTe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
  public
    property versao: String          read Fversao   write Fversao;
    property Id: String              read FId       write FId;
    property tpAmb: TpcnTipoAmbiente read FtpAmb    write FtpAmb;
    property verAplic: String        read FverAplic write FverAplic;
    property cStat: Integer          read FcStat    write FcStat;
    property xMotivo: String         read FxMotivo  write FxMotivo;
    property cUF: Integer            read FcUF      write FcUF;
    property chCTe: String           read FchCTe    write FchCTe;
    property dhRecbto: TDateTime     read FdhRecbto write FdhRecbto;
    property nProt: String           read FnProt    write FnProt;
  end;

  TRetEventoCTeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetEventoCTeCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetEventoCTeCollectionItem);
  public
    function Add: TRetEventoCTeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetEventoCTeCollectionItem;
    property Items[Index: Integer]: TRetEventoCTeCollectionItem read GetItem write SetItem; default;
  end;

  TRetEventoCTeCollectionItem = class(TObject)
  private
    FRetEventoCTe: TRetEventoCTe;
  public
    constructor Create;
    destructor Destroy; override;
    property RetEventoCTe: TRetEventoCTe read FRetEventoCTe write FRetEventoCTe;
  end;

  TRetConsSitCTe = class(TObject)
  private
    FLeitor: TLeitor;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FchCTe: String;
    FprotCTe: TProcCTe;
    FretCancCTe: TRetCancCTe;
    FprocEventoCTe: TRetEventoCTeCollection;
    FXMLprotCTe: String;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
    property Leitor: TLeitor                        read FLeitor        write FLeitor;
    property versao: String                         read Fversao        write Fversao;
    property tpAmb: TpcnTipoAmbiente                read FtpAmb         write FtpAmb;
    property verAplic: String                       read FverAplic      write FverAplic;
    property cStat: Integer                         read FcStat         write FcStat;
    property xMotivo: String                        read FxMotivo       write FxMotivo;
    property cUF: Integer                           read FcUF           write FcUF;
    property chCTe: String                          read FchCTe         write FchCTe;
    property protCTe: TProcCTe                      read FprotCTe       write FprotCTe;
    property retCancCTe: TRetCancCTe                read FretCancCTe    write FretCancCTe;
    property procEventoCTe: TRetEventoCTeCollection read FprocEventoCTe write FprocEventoCTe;
    property XMLprotCTe: String                     read FXMLprotCTe    write FXMLprotCTe;
  end;

implementation

{ TRetConsSitCTe }

constructor TRetConsSitCTe.Create;
begin
  inherited Create;
  FLeitor     := TLeitor.Create;
  FprotCTe    := TProcCTe.create;
  FretCancCTe := TRetCancCTe.create;
end;

destructor TRetConsSitCTe.Destroy;
begin
  FLeitor.Free;
  FprotCTe.Free;
  FretCancCTe.Free;

  if Assigned(procEventoCTe) then
    procEventoCTe.Free;

  inherited;
end;

function TRetConsSitCTe.LerXml: boolean;
var
  ok: boolean;
  i: Integer;
begin
  Result := False;

  FcStat           := 0;
  protCTe.cStat    := 0;
  retCancCTe.cStat := 0;

  try
    if leitor.rExtrai(1, 'retConsSitCTe') <> '' then
    begin
      Fversao   := Leitor.rAtributo('versao', 'retConsSitCTe');
      FtpAmb    := StrToTpAmb(ok, leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic := leitor.rCampo(tcStr, 'verAplic');
      FcStat    := leitor.rCampo(tcInt, 'cStat');
      FxMotivo  := leitor.rCampo(tcStr, 'xMotivo');
      FcUF      := leitor.rCampo(tcInt, 'cUF');

      // status 100 = Autorizado, 101 = Cancelado, 110 = Denegado, 301 = Denegado
      // A SEFAZ-MS esta retornando Status=129 como status de retorno da consulta
      // mas o status do CT-e consultado é 100
      if (FcStat in  [100, 101, 104, 110, 129]) or (FcStat = 301) then
      begin
        if (Leitor.rExtrai(1, 'protCTe') <> '') then
        begin
          // A propriedade XMLprotCTe contem o XML que traz o resultado do
          // processamento do CT-e.
          XMLprotCTe := Leitor.Grupo;

          if Leitor.rExtrai(2, 'infProt') <> '' then
          begin
            protCTe.Id       := Leitor.rAtributo('Id=', 'infProt');
            protCTe.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
            protCTe.verAplic := Leitor.rCampo(tcStr, 'verAplic');
            protCTe.chCTe    := Leitor.rCampo(tcStr, 'chCTe');
            protCTe.dhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
            protCTe.nProt    := Leitor.rCampo(tcStr, 'nProt');
            protCTe.digVal   := Leitor.rCampo(tcStr, 'digVal');
            protCTe.cStat    := Leitor.rCampo(tcInt, 'cStat');
            protCTe.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
            protCTe.cMsg     := Leitor.rCampo(tcInt, 'cMsg');
            protCTe.xMsg     := Leitor.rCampo(tcStr, 'xMsg');
            FchCTe           := protCTe.chCTe;
          end;
        end;
      end;

      if FcStat = 101 then
      begin
        if Leitor.rExtrai(1, 'infCanc') <> '' then
        begin
          retCancCTe.Id       := Leitor.rAtributo('Id=', 'infCanc');
          retCancCTe.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
          retCancCTe.verAplic := Leitor.rCampo(tcStr, 'verAplic');
          retCancCTe.cStat    := Leitor.rCampo(tcInt, 'cStat');
          retCancCTe.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
          retCancCTe.cUF      := Leitor.rCampo(tcInt, 'cUF');
          retCancCTe.chCTe    := Leitor.rCampo(tcStr, 'chCTe');
          retCancCTe.dhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
          retCancCTe.nProt    := Leitor.rCampo(tcStr, 'nProt');
          FchCTe              := retCancCTe.chCTe;
        end;
      end;

      if Assigned(procEventoCTe) then
        procEventoCTe.Free;

      procEventoCTe := TRetEventoCTeCollection.Create;
      i := 0;
      while Leitor.rExtrai(1, 'procEventoCTe', '', i + 1) <> '' do
      begin
        procEventoCTe.New;
        procEventoCTe.Items[i].RetEventoCTe.Leitor.Arquivo := Leitor.Grupo;
        procEventoCTe.Items[i].RetEventoCTe.XML := Leitor.Grupo;
        procEventoCTe.Items[i].RetEventoCTe.LerXml;
        inc(i);
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TRetEventoCTeCollection }

function TRetEventoCTeCollection.Add: TRetEventoCTeCollectionItem;
begin
  Result := Self.New;
end;

function TRetEventoCTeCollection.GetItem(Index: Integer): TRetEventoCTeCollectionItem;
begin
  Result := TRetEventoCTeCollectionItem(inherited Items[Index]);
end;

procedure TRetEventoCTeCollection.SetItem(Index: Integer; Value: TRetEventoCTeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRetEventoCTeCollection.New: TRetEventoCTeCollectionItem;
begin
  Result := TRetEventoCTeCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetEventoCTeCollectionItem }

constructor TRetEventoCTeCollectionItem.Create;
begin
  inherited Create;
  FRetEventoCTe := TRetEventoCTe.Create;
end;

destructor TRetEventoCTeCollectionItem.Destroy;
begin
  FRetEventoCTe.Free;

  inherited;
end;

end.

