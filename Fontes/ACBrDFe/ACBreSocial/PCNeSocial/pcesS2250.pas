{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
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
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 01/03/2016: Guilherme Costa
|*  - Passado o namespace para geração do cabeçalho
******************************************************************************}
{$I ACBr.inc}

unit pcesS2250;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2250Collection = class;
  TS2250CollectionItem = class;
  TEvtAvPrevio = class;
  TInfoAvPrevio = class;
  TDetAvPrevio = class;
  TCancAvPrevio = class;

  TS2250Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2250CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2250CollectionItem);
  public
    function Add: TS2250CollectionItem;
    property Items[Index: Integer]: TS2250CollectionItem read GetItem write SetItem; default;
  end;

  TS2250CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtAvPrevio: TEvtAvPrevio;

    procedure setEvtAvPrevio(const Value: TEvtAvPrevio);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtAvPrevio: TEvtAvPrevio read FEvtAvPrevio write setEvtAvPrevio;
  end;

  TEvtAvPrevio = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FInfoAvPrevio: TInfoAvPrevio;

    procedure GerarInfoAvPrevio;
    procedure GerarDetAvPrevio;
    procedure GerarCancAvPrevio;
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor  Destroy; override;

    function GerarXML(ASequencial: Integer; ATipoEmpregador: TEmpregador): boolean; override;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property InfoAvPrevio: TInfoAvPrevio read FInfoAvPrevio write FInfoAvPrevio;
  end;

  TInfoAvPrevio = class
  private
    FDetAvPrevio : TDetAvPrevio;
    FCancAvPrevio: TCancAvPrevio;
  public
    constructor create;
    destructor Destroy; override;

    property DetAvPrevio: TDetAvPrevio read FDetAvPrevio write FDetAvPrevio;
    property CancAvPrevio: TCancAvPrevio read FCancAvPrevio write FCancAvPrevio;
  end;

  TDetAvPrevio = class
  private
    FdtAvPrv: TDateTime;
    FdtPrevDeslig: TDateTime;
    FtpAvPrevio: tpTpAvPrevio;
    Fobservacao: string;
  public
    property dtAvPrv: TDateTime read FdtAvPrv write FdtAvPrv;
    property dtPrevDeslig: TDateTime read FdtPrevDeslig write FdtPrevDeslig;
    property tpAvPrevio: tpTpAvPrevio read FtpAvPrevio write FtpAvPrevio;
    property observacao: string read Fobservacao write Fobservacao;
  end;

  TCancAvPrevio = class
  private
    FdtCancAvPrv: TDateTime;
    Fobservacao: string;
    FmtvCancAvPrevio: tpMtvCancAvPrevio;
  public
    property dtCancAvPrv: TDateTime read FdtCancAvPrv write FdtCancAvPrv;
    property observacao: string read Fobservacao write Fobservacao;
    property mtvCancAvPrevio: tpMtvCancAvPrevio read FmtvCancAvPrevio write FmtvCancAvPrevio;
  end;

implementation

{ TS2250Collection }

function TS2250Collection.Add: TS2250CollectionItem;
begin
  Result := TS2250CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2250Collection.GetItem(Index: Integer): TS2250CollectionItem;
begin
  Result := TS2250CollectionItem(inherited GetItem(Index));
end;

procedure TS2250Collection.SetItem(Index: Integer;
  Value: TS2250CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2250CollectionItem }

constructor TS2250CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento      := teS2250;
  FEvtAvPrevio := TEvtAvPrevio.Create(AOwner);
end;

destructor TS2250CollectionItem.Destroy;
begin
  FEvtAvPrevio.Free;

  inherited;
end;

procedure TS2250CollectionItem.setEvtAvPrevio(
  const Value: TEvtAvPrevio);
begin
  FEvtAvPrevio.Assign(Value);
end;

{ TEvtAvPrevio }

constructor TEvtAvPrevio.Create(AACBreSocial: TObject);
begin
  inherited;

  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo := TIdeVinculo.Create;
  FInfoAvPrevio := TInfoAvPrevio.Create;
end;


destructor TEvtAvPrevio.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FInfoAvPrevio.Free;

  inherited;
end;

procedure TEvtAvPrevio.GerarCancAvPrevio;
begin
  Gerador.wGrupo('cancAvPrevio');

  Gerador.wCampo(tcDat, '', 'dtCancAvPrv',     10,  10, 1, self.InfoAvPrevio.CancAvPrevio.dtCancAvPrv);
  Gerador.wCampo(tcStr, '', 'observacao',       1, 255, 0, self.InfoAvPrevio.CancAvPrevio.observacao);
  Gerador.wCampo(tcStr, '', 'mtvCancAvPrevio',  1,   1, 1, eSMtvCancAvPrevioToStr(self.InfoAvPrevio.CancAvPrevio.mtvCancAvPrevio));

  Gerador.wGrupo('/cancAvPrevio');
end;

procedure TEvtAvPrevio.GerarDetAvPrevio;
begin
  Gerador.wGrupo('detAvPrevio');

  Gerador.wCampo(tcDat, '', 'dtAvPrv',      10,  10, 1, self.InfoAvPrevio.DetAvPrevio.dtAvPrv);
  Gerador.wCampo(tcDat, '', 'dtPrevDeslig', 10,  10, 1, self.InfoAvPrevio.DetAvPrevio.dtPrevDeslig);
  Gerador.wCampo(tcStr, '', 'tpAvPrevio',    1,   1, 1, eSTpAvPrevioToStr(self.InfoAvPrevio.DetAvPrevio.tpAvPrevio));
  Gerador.wCampo(tcStr, '', 'observacao',    1, 255, 0, self.InfoAvPrevio.DetAvPrevio.observacao);

  Gerador.wGrupo('/detAvPrevio');
end;

procedure TEvtAvPrevio.GerarInfoAvPrevio;
begin
  Gerador.wGrupo('infoAvPrevio');

  if (DateToStr(self.InfoAvPrevio.DetAvPrevio.dtAvPrv) <> dDataBrancoNula) then
    GerarDetAvPrevio;

  if (DateToStr(self.InfoAvPrevio.CancAvPrevio.dtCancAvPrv) <> dDataBrancoNula) then
    GerarCancAvPrevio;

  Gerador.wGrupo('/infoAvPrevio');
end;

function TEvtAvPrevio.GerarXML(ASequencial: Integer; ATipoEmpregador: TEmpregador): boolean;
begin
  try
    GerarCabecalho('evtAvPrevio');
    Gerador.wGrupo('evtAvPrevio Id="' +
      GerarChaveEsocial(now, self.ideEmpregador.NrInsc, ASequencial, ATipoEmpregador) + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(self.IdeVinculo);
    GerarInfoAvPrevio;

    Gerador.wGrupo('/evtAvPrevio');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAvPrevio');

    Validar('evtAvPrevio');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

{ TInfoAvPrevio }

constructor TInfoAvPrevio.create;
begin
  FDetAvPrevio  := TDetAvPrevio.create;
  FCancAvPrevio := TCancAvPrevio.Create;
end;

destructor TInfoAvPrevio.destroy;
begin
  FDetAvPrevio.Free;
  FCancAvPrevio.Free;

  inherited;
end;

end.

