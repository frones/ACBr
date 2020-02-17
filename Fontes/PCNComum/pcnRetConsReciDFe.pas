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

unit pcnRetConsReciDFe;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcnLeitor;

type

  TProtDFeCollection     = class;
  TProtDFeCollectionItem = class;

  TRetConsReciDFe = class
  private
    FLeitor: TLeitor;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FnRec: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FcMsg: Integer;
    FxMsg: String;
    FProtDFe: TProtDFeCollection;
    FtagGrupoMsg: String;

    procedure SetProtDFe(const Value: TProtDFeCollection);
  public
    constructor Create(const AtagGrupoMsg: String);
    destructor Destroy; override;
    function LerXML: Boolean;
    property Leitor: TLeitor             read FLeitor   write FLeitor;
    property versao: String              read Fversao   write Fversao;
    property tpAmb: TpcnTipoAmbiente     read FtpAmb    write FtpAmb;
    property verAplic: String            read FverAplic write FverAplic;
    property nRec: String                read FnRec     write FnRec;
    property cStat: Integer              read FcStat    write FcStat;
    property xMotivo: String             read FxMotivo  write FxMotivo;
    property cUF: Integer                read FcUF      write FcUF;
    property cMsg: Integer               read FcMsg     write FcMsg;
    property xMsg: String                read FxMsg     write FxMsg;
    property ProtDFe: TProtDFeCollection read FProtDFe  write SetProtDFe;
  end;

  TProtDFeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TProtDFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TProtDFeCollectionItem);
  public
    function New: TProtDFeCollectionItem;
    property Items[Index: Integer]: TProtDFeCollectionItem read GetItem write SetItem; default;
  end;

  TProtDFeCollectionItem = class(TObject)
  private
    FId: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FchDFe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FXMLprotDFe: String;
    FcMsg : Integer;
    FxMsg : String;
  public
    property Id: String              read FId         write FId;
    property tpAmb: TpcnTipoAmbiente read FtpAmb      write FtpAmb;
    property verAplic: String        read FverAplic   write FverAplic;
    property chDFe: String           read FchDFe      write FchDFe;
    property dhRecbto: TDateTime     read FdhRecbto   write FdhRecbto;
    property nProt: String           read FnProt      write FnProt;
    property digVal: String          read FdigVal     write FdigVal;
    property cStat: Integer          read FcStat      write FcStat;
    property xMotivo: String         read FxMotivo    write FxMotivo;
    property XMLprotDFe: String      read FXMLprotDFe write FXMLprotDFe;
    property cMSg: Integer           read FcMsg       write FcMsg;
    property xMsg: String            read FxMsg       write FxMsg;
  end;

implementation

{ TRetConsReciDFe }

constructor TRetConsReciDFe.Create(const AtagGrupoMsg: String);
begin
  inherited Create;

  FLeitor  := TLeitor.Create;
  FProtDFe := TProtDFeCollection.Create();

  FtagGrupoMsg := AtagGrupoMsg;
end;

destructor TRetConsReciDFe.Destroy;
begin
  FLeitor.Free;
  FProtDFe.Free;

  inherited;
end;

procedure TRetConsReciDFe.SetProtDFe(const Value: TProtDFeCollection);
begin
  FProtDFe.Assign(Value);
end;

{ TProtDFeCollection }

function TProtDFeCollection.GetItem(Index: Integer): TProtDFeCollectionItem;
begin
  Result := TProtDFeCollectionItem(inherited Items[Index]);
end;

procedure TProtDFeCollection.SetItem(Index: Integer; Value: TProtDFeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRetConsReciDFe.LerXML: Boolean;
var
  ok: Boolean;
  i: Integer;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;

    if Leitor.rExtrai(1, 'retConsReci' + FtagGrupoMsg) <> '' then
    begin
      Fversao   := Leitor.rAtributo('versao', 'retConsReci' + FtagGrupoMsg);
      FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      FnRec     := Leitor.rCampo(tcStr, 'nRec');
      FcStat    := Leitor.rCampo(tcInt, 'cStat');
      FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
      FcUF      := Leitor.rCampo(tcInt, 'cUF');
      FcMsg     := Leitor.rCampo(tcInt, 'cMsg');
      FxMsg     := Leitor.rCampo(tcStr, 'xMsg');

      i := 0;
      while (FcStat = 104) and (Leitor.rExtrai(1, 'prot' + FtagGrupoMsg, '', i + 1) <> '') do
      begin
        ProtDFe.New;

        // A propriedade XMLprotDFe contem o XML que traz o resultado do
        // processamento da NF-e.
        ProtDFe[i].XMLprotDFe := Leitor.Grupo;

        if Leitor.rExtrai(2, 'infProt') <> '' then
        begin
          ProtDFe[i].FId       := Leitor.rAtributo('Id=', 'infProt');
          ProtDFe[i].FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
          ProtDFe[i].FverAplic := Leitor.rCampo(tcStr, 'verAplic');
          ProtDFe[i].FchDFe    := Leitor.rCampo(tcStr, 'ch' + FtagGrupoMsg);
          ProtDFe[i].FdhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
          ProtDFe[i].FnProt    := Leitor.rCampo(tcStr, 'nProt');
          ProtDFe[i].FdigVal   := Leitor.rCampo(tcStr, 'digVal');
          ProtDFe[i].FcStat    := Leitor.rCampo(tcInt, 'cStat');
          ProtDFe[i].FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
          ProtDFe[i].FcMsg     := Leitor.rCampo(tcInt, 'cMsg');
          ProtDFe[i].FxMsg     := Leitor.rCampo(tcStr, 'xMsg');
        end;
        inc(i);
      end;

      Result := True;
    end;
  except
    result := False;
  end;
end;

function TProtDFeCollection.New: TProtDFeCollectionItem;
begin
  Result := TProtDFeCollectionItem.Create;
  Add(Result);
end;

end.

