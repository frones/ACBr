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

unit pmdfeRetConsReciMDFe;

interface

uses
  SysUtils, Classes,
  pcnAuxiliar, pcnConversao, pcnLeitor;

type

  TRetConsReciMDFe        = class;
  TProtMDFeCollection     = class;
  TProtMDFeCollectionItem = class;

  TRetConsReciMDFe = class(TPersistent)
  private
    FLeitor: TLeitor;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FnRec: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FProtMDFe: TProtMDFeCollection;
    FcMsg: Integer;
    FxMsg: String;

    procedure SetProtMDFe(const Value: TProtMDFeCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function LerXML: boolean;
  published
    property Leitor: TLeitor               read FLeitor   write FLeitor;
    property versao: String                read Fversao   write Fversao;
    property tpAmb: TpcnTipoAmbiente       read FtpAmb    write FtpAmb;
    property verAplic: String              read FverAplic write FverAplic;
    property nRec: String                  read FnRec     write FnRec;
    property cStat: Integer                read FcStat    write FcStat;
    property xMotivo: String               read FxMotivo  write FxMotivo;
    property cUF: Integer                  read FcUF      write FcUF;
    property ProtMDFe: TProtMDFeCollection read FProtMDFe write SetProtMDFe;
    property cMsg: Integer           read FcMsg     write FcMsg;
    property xMsg: String            read FxMsg     write FxMsg;
  end;

  TProtMDFeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TProtMDFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TProtMDFeCollectionItem);
  public
    constructor Create(AOwner: TRetConsReciMDFe); reintroduce;
    function Add: TProtMDFeCollectionItem;
    property Items[Index: Integer]: TProtMDFeCollectionItem read GetItem write SetItem; default;
  end;

  TProtMDFeCollectionItem = class(TCollectionItem)
  private
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FchMDFe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FXMLprotMDFe: String;
  published
    property tpAmb: TpcnTipoAmbiente read FtpAmb       write FtpAmb;
    property verAplic: String        read FverAplic    write FverAplic;
    property chMDFe: String          read FchMDFe      write FchMDFe;
    property dhRecbto: TDateTime     read FdhRecbto    write FdhRecbto;
    property nProt: String           read FnProt       write FnProt;
    property digVal: String          read FdigVal      write FdigVal;
    property cStat: Integer          read FcStat       write FcStat;
    property xMotivo: String         read FxMotivo     write FxMotivo;
    property XMLprotMDFe: String     read FXMLprotMDFe write FXMLprotMDFe;
  end;

implementation

{ TRetConsReciMDFe }

constructor TRetConsReciMDFe.Create;
begin
  FLeitor   := TLeitor.Create;
  FProtMDFe := TProtMDFeCollection.Create(self);
end;

destructor TRetConsReciMDFe.Destroy;
begin
  FLeitor.Free;
  FProtMDFe.Free;
  inherited;
end;

procedure TRetConsReciMDFe.SetProtMDFe(const Value: TProtMDFeCollection);
begin
  FProtMDFe.Assign(Value);
end;

function TRetConsReciMDFe.LerXML: boolean;
var
  ok: boolean;
  i: Integer;
begin
  Result := False;
  
  try
    Leitor.Grupo := Leitor.Arquivo;
    if Leitor.rExtrai(1, 'retConsReciMDFe') <> '' then
    begin
      Fversao   := Leitor.rAtributo('versao', 'retConsReciMDFe');
      FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      FnRec     := Leitor.rCampo(tcStr, 'nRec');
      FcStat    := Leitor.rCampo(tcInt, 'cStat');
      FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
      FcUF      := Leitor.rCampo(tcInt, 'cUF');
      FcMsg     := FcStat;
      FxMsg     := FxMotivo;

      i := 0;
      while (FcStat = 104) and (Leitor.rExtrai(1, 'protMDFe', '', i + 1) <> '') do
      begin
        ProtMDFe.Add;

        // A propriedade XMLprotMDFe contem o XML que traz o resultado do
        // processamento do MDF-e.
        ProtMDFe[i].XMLprotMDFe := Leitor.Grupo;

        if Leitor.rExtrai(2, 'infProt') <> '' then
        begin
          ProtMDFe[i].FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
          ProtMDFe[i].FverAplic := Leitor.rCampo(tcStr, 'verAplic');
          ProtMDFe[i].FchMDFe   := Leitor.rCampo(tcStr, 'chMDFe');
          ProtMDFe[i].FdhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
          ProtMDFe[i].FnProt    := Leitor.rCampo(tcStr, 'nProt');
          ProtMDFe[i].FdigVal   := Leitor.rCampo(tcStr, 'digVal');
          ProtMDFe[i].FcStat    := Leitor.rCampo(tcInt, 'cStat');
          ProtMDFe[i].FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
        end;
        inc(i);
      end;

      Result := True;
    end;

  except
    result := False;
  end;
end;

{ TProtMDFeCollection }

constructor TProtMDFeCollection.Create(AOwner: TRetConsReciMDFe);
begin
  inherited Create(TProtMDFeCollectionItem);
end;

function TProtMDFeCollection.Add: TProtMDFeCollectionItem;
begin
  Result := TProtMDFeCollectionItem(inherited Add);
end;

function TProtMDFeCollection.GetItem(Index: Integer): TProtMDFeCollectionItem;
begin
  Result := TProtMDFeCollectionItem(inherited GetItem(Index));
end;

procedure TProtMDFeCollection.SetItem(Index: Integer; Value: TProtMDFeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

end.

