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

unit pcnRetEnvBPe;

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
  pcnConversao, pcnLeitor;

type

  TProtBPeCollection     = class;
  TProtBPeCollectionItem = class;

  TretEnvBPe = class(TObject)
  private
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FcStat: Integer;
    FLeitor: TLeitor;
    FcUF: Integer;
    FverAplic: String;
    FxMotivo: String;
    FProtBPe: TProtBPeCollection;

    procedure SetProtBPe(const Value: TProtBPeCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
    property Leitor: TLeitor         read FLeitor   write FLeitor;
    property versao: String          read Fversao    write Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb    write FtpAmb;
    property verAplic: String        read FverAplic write FverAplic;
    property cStat: Integer          read FcStat    write FcStat;
    property xMotivo: String         read FxMotivo  write FxMotivo;
    property cUF: Integer            read FcUF      write FcUF;
    property ProtBPe: TProtBPeCollection read FProtBPe  write SetProtBPe;
  end;

  TProtBPeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TProtBPeCollectionItem;
    procedure SetItem(Index: Integer; Value: TProtBPeCollectionItem);
  public
    function Add: TProtBPeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TProtBPeCollectionItem;
    property Items[Index: Integer]: TProtBPeCollectionItem read GetItem write SetItem; default;
  end;

  TProtBPeCollectionItem = class(TObject)
  private
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FchBPe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FXMLprotBPe: String;
  public
    property tpAmb: TpcnTipoAmbiente read FtpAmb      write FtpAmb;
    property verAplic: String        read FverAplic   write FverAplic;
    property chBPe: String           read FchBPe      write FchBPe;
    property dhRecbto: TDateTime     read FdhRecbto   write FdhRecbto;
    property nProt: String           read FnProt      write FnProt;
    property digVal: String          read FdigVal     write FdigVal;
    property cStat: Integer          read FcStat      write FcStat;
    property xMotivo: String         read FxMotivo    write FxMotivo;
    property XMLprotBPe: String      read FXMLprotBPe write FXMLprotBPe;
  end;

implementation

{ TretEnvBPe }

constructor TretEnvBPe.Create;
begin
  inherited;
  FLeitor  := TLeitor.Create;
  FProtBPe := TProtBPeCollection.Create;
end;

destructor TretEnvBPe.Destroy;
begin
  FLeitor.Free;
  FProtBPe.Free;
  inherited;
end;

procedure TretEnvBPe.SetProtBPe(const Value: TProtBPeCollection);
begin
  FProtBPe := Value;
end;

{ TProtBPeCollection }

function TProtBPeCollection.Add: TProtBPeCollectionItem;
begin
  Result := Self.New;
end;

function TProtBPeCollection.GetItem(Index: Integer): TProtBPeCollectionItem;
begin
  Result := TProtBPeCollectionItem(inherited GetItem(Index));
end;

procedure TProtBPeCollection.SetItem(Index: Integer; Value: TProtBPeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TretEnvBPe.LerXml: Boolean;
var
  ok: Boolean;
  i: Integer;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;
    if leitor.rExtrai(1, 'retBPe') <> '' then
    begin
      Fversao   := Leitor.rAtributo('versao');
      FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      FcUF      := Leitor.rCampo(tcInt, 'cUF');
      FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      FcStat    := Leitor.rCampo(tcInt, 'cStat');
      FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');

      i := 0;
      while (FcStat in [100, 102]) and (Leitor.rExtrai(1, 'protBPe', '', i + 1) <> '') do
      begin
        ProtBPe.New;

        // A propriedade XMLprotBPe contem o XML que traz o resultado do
        // processamento da BP-e.
        ProtBPe[i].XMLprotBPe := Leitor.Grupo;

        if Leitor.rExtrai(2, 'infProt') <> '' then
        begin
          ProtBPe[i].FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
          ProtBPe[i].FverAplic := Leitor.rCampo(tcStr, 'verAplic');
          ProtBPe[i].FchBPe    := Leitor.rCampo(tcStr, 'chBPe');
          ProtBPe[i].FdhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
          ProtBPe[i].FnProt    := Leitor.rCampo(tcStr, 'nProt');
          ProtBPe[i].FdigVal   := Leitor.rCampo(tcStr, 'digVal');
          ProtBPe[i].FcStat    := Leitor.rCampo(tcInt, 'cStat');
          ProtBPe[i].FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
        end;
        inc(i);
      end;

      Result := True;
    end;
  except
    Result := false;
  end;
end;

function TProtBPeCollection.New: TProtBPeCollectionItem;
begin
  Result := TProtBPeCollectionItem.Create;
  Self.Add(Result);
end;

end.

