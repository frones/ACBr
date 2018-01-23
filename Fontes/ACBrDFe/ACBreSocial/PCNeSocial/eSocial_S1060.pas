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
|* 01/03/2015: Guilherme Costa
|*  - Alterado o nome do XSD para validação
******************************************************************************}
{$I ACBr.inc}

unit eSocial_S1060;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador,
  eSocial_Common, eSocial_Conversao, eSocial_Gerador;

type
  TS1060Collection = class;
  TS1060CollectionItem = class;
  TIdeAmbiente = class;
  TFatorRiscoCollectionItem = class;
  TFatorRiscoCollection = class;
  TDadosAmbiente = class;
  TInfoAmbiente = class;
  TEvtTabAmbiente = class;


  TS1060Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS1060CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1060CollectionItem);
  public
    function Add: TS1060CollectionItem;
    property Items[Index: Integer]: TS1060CollectionItem read GetItem write SetItem; default;
  end;

  TS1060CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabAmbiente: TEvtTabAmbiente;
    procedure setEvtTabAmbiente(const Value: TEvtTabAmbiente);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabAmbiente: TEvtTabAmbiente read FEvtTabAmbiente write setEvtTabAmbiente;
  end;

  TEvtTabAmbiente = class(TeSocialEvento)
  private
    FModoLancamento: TModoLancamento;
    FIdeEvento: TIdeEvento;
    FIdeEmpregador: TIdeEmpregador;
    FInfoAmbiente: TInfoAmbiente;

    procedure GerarIdeAmbiente;
    procedure GerarFatorRisco;
    procedure GerarDadosAmbiente;
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor  Destroy; override;

    function GerarXML: boolean; override;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property ideEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property infoAmbiente: TInfoAmbiente read FInfoAmbiente write FInfoAmbiente;
  end;

  TIdeAmbiente = class(TPersistent)
  private
    FCodAmb: string;
    FIniValid: string;
    FFimValid: string;
  public
    property codAmb: string read FCodAmb write FCodAmb;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TFatorRiscoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TFatorRiscoCollectionItem;
    procedure SetItem(Index: Integer; Value: TFatorRiscoCollectionItem);
  public
    constructor create; reintroduce;
    function add: TFatorRiscoCollectionItem;
    property Items[Index: Integer]: TFatorRiscoCollectionItem read GetItem write SetItem;
  end;

  TFatorRiscoCollectionItem = class(TCollectionItem)
  private
    FCodFatRis: string;
  public
    constructor create; reintroduce;

    property codFatRis: string read FCodFatRis write FCodFatRis;
  end;

  TDadosAmbiente = class(TPersistent)
  private
    FDscAmb: string;
    FLocalAmb: tpLocalAmb;
    FTpInsc: tpTpInscAmbTab;
    FNrInsc: string;
    FFatorRisco: TFatorRiscoCollection;
  public
    constructor create;
    destructor destroy; override;

    property dscAmb: string read FDscAmb write FDscAmb;
    property localAmb: tpLocalAmb read FLocalAmb write FLocalAmb;
    property tpInsc: tpTpInscAmbTab read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property fatorRisco: TFatorRiscoCollection read FFatorRisco write FFatorRisco;
  end;

  TInfoAmbiente = class(TPersistent)
  private
    FIdeAmbiente: TIdeAmbiente;
    FDadosAmbiente: TDadosAmbiente;
    FNovaValidade: TIdePeriodo;

    function getDadosAmbiente: TDadosAmbiente;
    function getNovaValidade: TIdePeriodo;
  public
    constructor create;
    destructor destroy; override;

    function dadosAmbienteInst(): Boolean;
    function novaValidadeInst(): Boolean;

    property ideAmbiente: TIdeAmbiente read FIdeAmbiente write FIdeAmbiente;
    property dadosAmbiente: TDadosAmbiente read getDadosAmbiente write FDadosAmbiente;
    property novaValidade: TIdePeriodo read getNovaValidade write FNovaValidade;
  end;

implementation

uses
  eSocial_Tabelas;

{ TS1060Collection }

function TS1060Collection.Add: TS1060CollectionItem;
begin
  Result := TS1060CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS1060Collection.GetItem(Index: Integer): TS1060CollectionItem;
begin
  Result := TS1060CollectionItem(inherited GetItem(Index));
end;

procedure TS1060Collection.SetItem(Index: Integer; Value: TS1060CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS1060CollectionItem }

constructor TS1060CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS1060;
  FEvtTabAmbiente := TEvtTabAmbiente.Create(AOwner);
end;

destructor TS1060CollectionItem.Destroy;
begin
  FEvtTabAmbiente.Free;

  inherited;
end;

procedure TS1060CollectionItem.setEvtTabAmbiente(const Value: TEvtTabAmbiente);
begin
  FEvtTabAmbiente.Assign(Value);
end;

{ TFatorRiscoCollectionItem }

constructor TFatorRiscoCollectionItem.create;
begin

end;

{ TFatorRiscoCollection }

function TFatorRiscoCollection.add: TFatorRiscoCollectionItem;
begin
  Result := TFatorRiscoCollectionItem(inherited add);
  Result.create;
end;

constructor TFatorRiscoCollection.create;
begin
  inherited create(TFatorRiscoCollectionItem)
end;
            
function TFatorRiscoCollection.GetItem(
  Index: Integer): TFatorRiscoCollectionItem;
begin
  Result := TFatorRiscoCollectionItem(inherited GetItem(Index));
end;

procedure TFatorRiscoCollection.SetItem(Index: Integer;
  Value: TFatorRiscoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDadosAmbiente }

constructor TDadosAmbiente.create;
begin
  FFatorRisco := TFatorRiscoCollection.Create;
end;

destructor TDadosAmbiente.destroy;
begin
  FFatorRisco.Free;

  inherited;
end;

{ TInfoAmbiente }

constructor TInfoAmbiente.create;
begin
  FIdeAmbiente := TIdeAmbiente.Create;
  FDadosAmbiente := nil;
  FNovaValidade := nil;
end;

function TInfoAmbiente.dadosAmbienteInst: Boolean;
begin
  Result := Assigned(FDadosAmbiente);
end;

destructor TInfoAmbiente.destroy;
begin
  FIdeAmbiente.Free;
  FreeAndNil(FDadosAmbiente);
  FreeAndNil(FNovaValidade);

  inherited;
end;

function TInfoAmbiente.getDadosAmbiente: TDadosAmbiente;
begin
  if Not(Assigned(FDadosAmbiente)) then
    FDadosAmbiente := TDadosAmbiente.create;
  Result := FDadosAmbiente;
end;

function TInfoAmbiente.getNovaValidade: TIdePeriodo;
begin
  if Not(Assigned(FNovaValidade)) then
    FNovaValidade := TIdePeriodo.Create;
  Result := FNovaValidade;
end;

function TInfoAmbiente.novaValidadeInst: Boolean;
begin
  Result := Assigned(FNovaValidade);
end;

{ TEvtTabAmbiente }

constructor TEvtTabAmbiente.Create(AACBreSocial: TObject);
begin
  inherited;

  FIdeEvento := TIdeEvento.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoAmbiente := TInfoAmbiente.create;
end;

destructor TEvtTabAmbiente.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoAmbiente.Free;

  inherited;
end;

procedure TEvtTabAmbiente.GerarDadosAmbiente;
begin
  Gerador.wGrupo('dadosAmbiente');

  Gerador.wCampo(tcStr, '', 'dscAmb',   1, 999, 1, infoAmbiente.dadosAmbiente.dscAmb);
  Gerador.wCampo(tcStr, '', 'localAmb', 1,   1, 1, eSLocalAmbToStr(infoAmbiente.dadosAmbiente.localAmb));
  Gerador.wCampo(tcStr, '', 'tpInsc',   1,   1, 1, eStpTpInscAmbTabToStr(infoAmbiente.dadosAmbiente.tpInsc));
  Gerador.wCampo(tcStr, '', 'nrInsc',   1,  15, 1, infoAmbiente.dadosAmbiente.nrInsc);

  GerarFatorRisco;

  Gerador.wGrupo('/dadosAmbiente');
end;

procedure TEvtTabAmbiente.GerarFatorRisco;
var
  i: Integer;
  objFatorRisco: TFatorRiscoCollectionItem;
begin
  for i := 0 to infoAmbiente.dadosAmbiente.fatorRisco.Count - 1 do
  begin
    objFatorRisco := infoAmbiente.dadosAmbiente.fatorRisco.Items[i];

    Gerador.wGrupo('fatorRisco');

    Gerador.wCampo(tcStr, '', 'codFatRis', 1, 10, 1, objFatorRisco.codFatRis);

    Gerador.wGrupo('/fatorRisco');
  end;

  if infoAmbiente.dadosAmbiente.fatorRisco.Count > 999 then
    Gerador.wAlerta('', 'fatorRisco', 'Lista de Fator de Risco', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtTabAmbiente.GerarIdeAmbiente;
begin
  Gerador.wGrupo('ideAmbiente');

  Gerador.wCampo(tcStr, '', 'codAmb',   1, 30, 1, infoAmbiente.ideAmbiente.codAmb);
  Gerador.wCampo(tcStr, '', 'iniValid', 7,  7, 1, infoAmbiente.ideAmbiente.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid', 7,  7, 0, infoAmbiente.ideAmbiente.fimValid);

  Gerador.wGrupo('/ideAmbiente');
end;

function TEvtTabAmbiente.GerarXML: boolean;
begin
  try
    GerarCabecalho('evtTabAmbiente');
    Gerador.wGrupo('evtTabAmbiente Id="'+ GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0) +'"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoAmbiente');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeAmbiente;

    if Self.ModoLancamento <> mlExclusao then
    begin
      GerarDadosAmbiente;

      if Self.ModoLancamento = mlAlteracao then
        if (infoAmbiente.novaValidadeInst()) then
          GerarIdePeriodo(infoAmbiente.novaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoAmbiente');
    Gerador.wGrupo('/evtTabAmbiente');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabAmbiente');
    
    Validar('evtTabAmbiente');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

end.
