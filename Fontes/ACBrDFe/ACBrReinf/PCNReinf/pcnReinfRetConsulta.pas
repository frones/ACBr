{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}

{$I ACBr.inc}

unit pcnReinfRetConsulta;

interface

uses
  SysUtils, Classes,
  ACBrUtil, pcnAuxiliar, pcnConversao, pcnLeitor,
  pcnCommonReinf, pcnConversaoReinf, pcnReinfR5001, pcnReinfR5011;

type
  TRetEventosCollection = class;
  TRetEventosCollectionItem = class;
  TRetConsulta = class;

  TRetEventosCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRetEventosCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetEventosCollectionItem);
  public
    constructor create(AOwner: TRetConsulta);

    function Add: TRetEventosCollectionItem;
    property Items[Index: Integer]: TRetEventosCollectionItem read GetItem
      write SetItem; default;
  end;

  TRetEventosCollectionItem = class(TCollectionItem)
  private
    FIDEvento: string;
    Ftipo: String;
    FArquivoReinf: string;
    FEvento: IEventoReinf;
  public
    property Id: string read FIDEvento write FIDEvento;
    property tipo: String read Ftipo write Ftipo;
    property ArquivoReinf: string read FArquivoReinf write FArquivoReinf;
    property Evento: IEventoReinf read FEvento write FEvento;
  end;

  TRetConsulta = class(TPersistent)
  private
    FLeitor: TLeitor;
    FRetEventos: TRetEventosCollection;

    procedure SetEventos(const Value: TRetEventosCollection);
  public
    constructor create;
    destructor Destroy; override;

    function LerXml: boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;

    property RetEventos: TRetEventosCollection read FRetEventos write SetEventos;
  end;

implementation

{ TRetEventosCollection }

function TRetEventosCollection.Add: TRetEventosCollectionItem;
begin
  Result := TRetEventosCollectionItem(inherited Add());
end;

constructor TRetEventosCollection.create(AOwner: TRetConsulta);
begin
  inherited create(TRetEventosCollectionItem);
end;

function TRetEventosCollection.GetItem(Index: Integer)
  : TRetEventosCollectionItem;
begin
  Result := TRetEventosCollectionItem(Inherited GetItem(Index));
end;

procedure TRetEventosCollection.SetItem(Index: Integer;
  Value: TRetEventosCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

{ TRetConsultaLote }

constructor TRetConsulta.create;
begin
  FLeitor := TLeitor.create;

  FRetEventos := TRetEventosCollection.create(Self);
end;

destructor TRetConsulta.Destroy;
begin
  FLeitor.Free;
  FRetEventos.Free;

  inherited;
end;

procedure TRetConsulta.SetEventos(const Value: TRetEventosCollection);
begin
  FRetEventos := Value;
end;

function TRetConsulta.LerXml: boolean;
var
  i: Integer;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;

    i := 0;
    while Leitor.rExtrai(1, 'Reinf', '', i + 1) <> '' do
    begin
      RetEventos.Add;
      RetEventos.Items[i].Id           := FLeitor.rAtributo('id=', 'evtTotalContrib');
      RetEventos.Items[i].ArquivoReinf := Leitor.Arquivo;

      if pos('evtTotal', RetEventos.Items[i].ArquivoReinf) > 0 then
      begin
        RetEventos.Items[i].Tipo       := 'R5001';
        RetEventos.Items[i].Evento     := TR5001.Create;
        RetEventos.Items[i].Evento.Xml := RetEventos.Items[i].ArquivoReinf;
      end;

      if pos('evtTotalContrib', RetEventos.Items[i].ArquivoReinf) > 0 then
      begin
        RetEventos.Items[i].Tipo       := 'R5011';
        RetEventos.Items[i].Evento     := TR5011.Create;
        RetEventos.Items[i].Evento.Xml := RetEventos.Items[i].ArquivoReinf;
      end;

      inc(i);
    end;

    Result := True;
  except
    Result := False;
  end;
end;

end.
