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
******************************************************************************}
{$I ACBr.inc}

unit pcesS4999;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS4999Collection = class;
  TS4999CollectionItem = class;
  TEvtAdesao = class;
  TInfoAdesao = class;


  TS4999Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS4999CollectionItem;
    procedure SetItem(Index: Integer; Value: TS4999CollectionItem);
  public
    function Add: TS4999CollectionItem;
    property Items[Index: Integer]: TS4999CollectionItem read GetItem write SetItem; default;
  end;

  TS4999CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtAdesao: TEvtAdesao;
    procedure setEvtAdesao(const Value: TEvtAdesao);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtAdesao: TEvtAdesao read FEvtAdesao write setEvtAdesao;
  end;

  TEvtAdesao = class(TESocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FInfoAdesao: TInfoAdesao;

    {Geradores específicos da classe}
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor  Destroy; override;

    function GerarXML(ASequencial: Integer; ATipoEmpregador: TEmpregador): boolean; override;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property InfoAdesao: TInfoAdesao read FInfoAdesao write FInfoAdesao;
  end;

  TInfoAdesao = class
  private
    FdtAdesao: string;
  public
    property dtAdesao: string read FdtAdesao write FdtAdesao;
  end;


implementation

{ TS4999Collection }

function TS4999Collection.Add: TS4999CollectionItem;
begin
  Result := TS4999CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS4999Collection.GetItem(Index: Integer): TS4999CollectionItem;
begin
  Result := TS4999CollectionItem(inherited GetItem(Index));
end;

procedure TS4999Collection.SetItem(Index: Integer;
  Value: TS4999CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS4999CollectionItem }

constructor TS4999CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS4999;
  FEvtAdesao := TEvtAdesao.Create(AOwner);
end;

destructor TS4999CollectionItem.Destroy;
begin
  FEvtAdesao.Free;
  inherited;
end;

procedure TS4999CollectionItem.setEvtAdesao(const Value: TEvtAdesao);
begin
  FEvtAdesao.Assign(Value);
end;

{ TEvtSolicTotal }

constructor TEvtAdesao.Create(AACBreSocial: TObject);
begin
  inherited;
  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoAdesao := TInfoAdesao.Create;
end;

destructor TEvtAdesao.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoAdesao.Free;
  inherited;
end;

function TEvtAdesao.GerarXML(ASequencial: Integer; ATipoEmpregador: TEmpregador): boolean;
begin
  try
    (* Não é mais gerado na versão 2.4.01*)
    GerarCabecalho('');
      Gerador.wGrupo('evtAdesao Id="' +
      GerarChaveEsocial(now, self.ideEmpregador.NrInsc, ASequencial, ATipoEmpregador) + '"');
        //gerarIdVersao(self);
        gerarIdeEvento2(self.IdeEvento);
        gerarIdeEmpregador(self.IdeEmpregador);
        Gerador.wGrupo('infoAdesao');
          Gerador.wCampo(tcStr, '', 'dtAdesao', 0, 0, 0, self.InfoAdesao.dtAdesao);
        Gerador.wGrupo('/infoAdesao');
      Gerador.wGrupo('/evtAdesao');
    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAdesao');
    Validar('evtAdesao');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

end.
