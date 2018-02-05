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

unit pcesS1040;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1040Collection = class;
  TS1040CollectionItem = class;
  TEvtTabFuncao = class;
  TIdeFuncao = class;
  TDadosFuncao = class;
  TInfoFuncao = class;

  TS1040Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS1040CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1040CollectionItem);
  public
    function Add: TS1040CollectionItem;
    property Items[Index: Integer]: TS1040CollectionItem read GetItem write SetItem; default;
  end;

  TS1040CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabFuncao: TEvtTabFuncao;
    procedure setEvtTabFuncao(const Value: TEvtTabFuncao);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabFuncao: TEvtTabFuncao read FEvtTabFuncao write setEvtTabFuncao;
  end;

  TEvtTabFuncao = class(TeSocialEvento)
  private
    FModoLancamento: TModoLancamento;
    fIdeEvento: TIdeEvento;
    fIdeEmpregador: TIdeEmpregador;
    fInfoFuncao: TInfoFuncao;

    {Geradores específicos da classe}
    procedure GerarDadosFuncao;
    procedure GerarIdeFuncao;
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor  Destroy; override;

    function GerarXML(ATipoEmpregador: TEmpregador): boolean; override;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property IdeEvento: TIdeEvento read fIdeEvento write fIdeEvento;
    property IdeEmpregador: TIdeEmpregador read fIdeEmpregador write fIdeEmpregador;
    property InfoFuncao: TInfoFuncao read fInfoFuncao write fInfoFuncao;
  end;

  TIdeFuncao = class(TPersistent)
  private
    FCodFuncao: string;
    FIniValid: string;
    FFimValid: string;
  public
    property codFuncao: string read FCodFuncao write FCodFuncao;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TDadosFuncao = class(TPersistent)
  private
    FDscFuncao: string;
    FCodCBO: string;
  public
    property dscFuncao: string read FDscFuncao write FDscFuncao;
    property codCBO: string read FCodCBO write FCodCBO;
  end;

  TInfoFuncao = class(TPersistent)
  private
    fIdeFuncao: TIdeFuncao;
    fDadosFuncao: TDadosFuncao;
    fNovaValidade: TIdePeriodo;

    function getDadosFuncao: TDadosFuncao;
    function getNovaValidade: TIdePeriodo;
  public
    constructor create;
    destructor destroy; override;

    function dadosFuncaoInst(): Boolean;
    function novaValidadeInst(): Boolean;

    property IdeFuncao: TIdeFuncao read fIdeFuncao write fIdeFuncao;
    property DadosFuncao: TDadosFuncao read getDadosFuncao write fDadosFuncao;
    property NovaValidade: TIdePeriodo read getNovaValidade write fNovaValidade;
  end;

implementation

{ TS1040Collection }

function TS1040Collection.Add: TS1040CollectionItem;
begin
  Result := TS1040CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS1040Collection.GetItem(Index: Integer): TS1040CollectionItem;
begin
  Result := TS1040CollectionItem(inherited GetItem(Index));
end;

procedure TS1040Collection.SetItem(Index: Integer;
  Value: TS1040CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS1040CollectionItem }

constructor TS1040CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS1040;
  FEvtTabFuncao := TEvtTabFuncao.Create(AOwner);
end;

destructor TS1040CollectionItem.Destroy;
begin
  FEvtTabFuncao.Free;

  inherited;
end;

procedure TS1040CollectionItem.setEvtTabFuncao(const Value: TEvtTabFuncao);
begin
  FEvtTabFuncao.Assign(Value);
end;

{ TInfoFuncao }

constructor TInfoFuncao.create;
begin
  fIdeFuncao := TIdeFuncao.Create;
  fDadosFuncao := nil;
  fNovaValidade := nil;
end;

function TInfoFuncao.dadosFuncaoInst: Boolean;
begin
  Result := Assigned(fDadosFuncao);
end;

destructor TInfoFuncao.destroy;
begin
  fIdeFuncao.Free;
  FreeAndNil(fDadosFuncao);
  FreeAndNil(fNovaValidade);

  inherited;
end;

function TInfoFuncao.getDadosFuncao: TDadosFuncao;
begin
  if Not(Assigned(fDadosFuncao)) then
    fDadosFuncao := TDadosFuncao.Create;
  Result := fDadosFuncao;
end;

function TInfoFuncao.getNovaValidade: TIdePeriodo;
begin
  if Not(Assigned(FNovaValidade)) then
    FNovaValidade := TIdePeriodo.Create;
  Result := FNovaValidade;
end;

function TInfoFuncao.novaValidadeInst: Boolean;
begin
  Result := Assigned(FNovaValidade);
end;

{ TEvtTabFuncao }

constructor TEvtTabFuncao.Create(AACBreSocial: TObject);
begin
  inherited;

  fIdeEvento := TIdeEvento.Create;
  fIdeEmpregador := TIdeEmpregador.Create;
  fInfoFuncao := TInfoFuncao.Create;
end;

destructor TEvtTabFuncao.destroy;
begin
  fIdeEvento.Free;
  fIdeEmpregador.Free;
  fInfoFuncao.Free;

  inherited;
end;

procedure TEvtTabFuncao.GerardadosFuncao;
begin
  Gerador.wGrupo('dadosFuncao');

  Gerador.wCampo(tcStr, '', 'dscFuncao', 1, 100, 1, self.InfoFuncao.DadosFuncao.dscFuncao);
  Gerador.wCampo(tcStr, '', 'codCBO',    1,   6, 1, self.InfoFuncao.DadosFuncao.codCBO);

  Gerador.wGrupo('/dadosFuncao');
end;

procedure TEvtTabFuncao.GerarIdeFuncao;
begin
  Gerador.wGrupo('ideFuncao');

  Gerador.wCampo(tcStr, '', 'codFuncao', 1, 30, 1, self.InfoFuncao.IdeFuncao.codFuncao);
  Gerador.wCampo(tcStr, '', 'iniValid',  7,  7, 1, self.InfoFuncao.IdeFuncao.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid',  7,  7, 0, self.InfoFuncao.IdeFuncao.fimValid);

  Gerador.wGrupo('/ideFuncao');
end;

function TEvtTabFuncao.GerarXML(ATipoEmpregador: TEmpregador): boolean;
begin
  try
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc,
     self.Sequencial, ATipoEmpregador);

    GerarCabecalho('evtTabFuncao');
    Gerador.wGrupo('evtTabFuncao Id="' + Self.Id + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoFuncao');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeFuncao;

    if Self.ModoLancamento <> mlExclusao then
    begin
      GerarDadosFuncao;

      if Self.ModoLancamento = mlAlteracao then
        if (InfoFuncao.novaValidadeInst()) then
          GerarIdePeriodo(self.InfoFuncao.NovaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoFuncao');
    Gerador.wGrupo('/evtTabFuncao');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabFuncao');

    Validar('evtTabFuncao');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

end.
 
