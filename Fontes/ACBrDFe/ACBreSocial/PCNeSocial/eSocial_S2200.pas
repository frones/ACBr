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
|*  - Passado o namespace para geração no cabeçalho
******************************************************************************}
{$I ACBr.inc}

unit eSocial_S2200;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  eSocial_Common, eSocial_Conversao, eSocial_Gerador;

type
  TS2200Collection = class;
  TS2200CollectionItem = class;
  TEvtAdmissao = class;

  TS2200Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2200CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2200CollectionItem);
  public
    function Add: TS2200CollectionItem;
    property Items[Index: Integer]: TS2200CollectionItem read GetItem write SetItem; default;
  end;

  TS2200CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtAdmissao: TEvtAdmissao;

    procedure setEvtAdmissao(const Value: TEvtAdmissao);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtAdmissao: TEvtAdmissao read FEvtAdmissao write setEvtAdmissao;
  end;

  TEvtAdmissao = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FTrabalhador: TTrabalhador;
    FVinculo: TVinculo;
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor destroy; override;

    function GerarXML: boolean; override;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property Trabalhador: TTrabalhador read FTrabalhador write FTrabalhador;
    property Vinculo: TVinculo read FVinculo write FVinculo;
  end;

implementation

uses
  eSocial_NaoPeriodicos;

{ TS2200Collection }
function TS2200Collection.Add: TS2200CollectionItem;
begin
  Result := TS2200CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2200Collection.GetItem(Index: Integer): TS2200CollectionItem;
begin
  Result := TS2200CollectionItem(inherited GetItem(Index));
end;

procedure TS2200Collection.SetItem(Index: Integer;
  Value: TS2200CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2200CollectionItem }
constructor TS2200CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS2200;
  FEvtAdmissao := TEvtAdmissao.Create(AOwner);
end;

destructor TS2200CollectionItem.Destroy;
begin
  FEvtAdmissao.Free;

  inherited;
end;

procedure TS2200CollectionItem.setEvtAdmissao(const Value: TEvtAdmissao);
begin
  FEvtAdmissao.Assign(Value);
end;

{ TEvtAdmissao }
constructor TEvtAdmissao.create(AACBreSocial: TObject);
begin
  inherited;

  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FTrabalhador := TTrabalhador.Create;
  FVinculo := TVinculo.Create;
end;

destructor TEvtAdmissao.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FTrabalhador.Free;
  FVinculo.Free;

  inherited;
end;

function TEvtAdmissao.GerarXML: boolean;
begin
  try
    GerarCabecalho('evtAdmissao');
    Gerador.wGrupo('evtAdmissao Id="' + GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0) + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    GerarTrabalhador(self.Trabalhador, 'trabalhador', 2);
    GerarVinculo(self.Vinculo, 2);

    Gerador.wGrupo('/evtAdmissao');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAdmissao');
    Validar('evtAdmissao');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

end.
