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

unit pcesS2100;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2100Collection = class;
  TS2100CollectionItem = class;
  TevtCadInicial = class;

  {Classes específicas deste evento}

  TS2100Collection = class(TOwnedCollection)
  private
    FIniciais : TComponent;
    function GetItem(Index: Integer): TS2100CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2100CollectionItem);
  public
    function Add: TS2100CollectionItem;
    property Items[Index: Integer]: TS2100CollectionItem read GetItem write SetItem; default;
  end;

  TS2100CollectionItem = class(TCollectionItem)
  private
    FIniciais : TComponent;
    FTipoEvento: TTipoEvento;
    FevtCadInicial: TevtCadInicial;
    procedure setevtCadInicial(const Value: TevtCadInicial);
  public
    constructor Create(AIniciais: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtCadInicial: TevtCadInicial read FevtCadInicial write setevtCadInicial;
  end;


  TevtCadInicial = class(TeSocialEvento)
  private
    FIniciais : TComponent;
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FTrabalhador: TTrabalhador;
    FVinculo: TVinculo;

    {Geradores específicos da classe}
  public
    constructor Create(AACBreSocial: TObject); overload;
    destructor Destroy; override;

    function GerarXML: boolean; override;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property trabalhador: TTrabalhador read Ftrabalhador write Ftrabalhador;
    property vinculo: TVinculo read FVinculo write FVinculo;
  end;

implementation

{ TS2100Collection }

function TS2100Collection.Add: TS2100CollectionItem;
begin
  Result := TS2100CollectionItem(inherited Add);
  Result.create(TComponent(Self.Owner));
end;

function TS2100Collection.GetItem(Index: Integer): TS2100CollectionItem;
begin
  Result := TS2100CollectionItem(inherited GetItem(Index));
end;

procedure TS2100Collection.SetItem(Index: Integer;
  Value: TS2100CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2100CollectionItem }

constructor TS2100CollectionItem.Create(AIniciais: TComponent);
begin
  FIniciais := AIniciais;
  FTipoEvento := teS2100;
  FevtCadInicial := TevtCadInicial.Create(FIniciais);
end;

destructor TS2100CollectionItem.Destroy;
begin
  FevtCadInicial.Free;
  inherited;
end;

procedure TS2100CollectionItem.setevtCadInicial(const Value: TevtCadInicial);
begin
  FevtCadInicial.Assign(Value);
end;

{ TevtCadInicial }

constructor TevtCadInicial.Create(AACBreSocial: TObject);
begin
  inherited;
  FIdeEvento:= TIdeEvento2.Create;
  FIdeEmpregador:= TIdeEmpregador.Create;
  FTrabalhador:= TTrabalhador.Create;
  FVinculo:= TVinculo.Create;
end;

destructor TevtCadInicial.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FTrabalhador.Free;
  FVinculo.Free;
  inherited;
end;

function TevtCadInicial.GerarXML: boolean;
begin
  try
    (* Não é mais gerado na versão 2.4.01*)
    GerarCabecalho('evtCadInicial');
      Gerador.wGrupo('evtCadInicial Id="'+ GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0) +'"');
        GerarIdeEvento2(Self.ideEvento);
        GerarIdeEmpregador(Self.IdeEmpregador);
        GerarTrabalhador(Self.trabalhador);
        GerarVinculo(Self.vinculo);
      Gerador.wGrupo('/evtCadInicial');
    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtCadInicial');//Gerador.ArquivoFormatoXML;
    Validar('evtCadInicial');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;
  Result := (Gerador.ArquivoFormatoXML <> '')
end;

end.
