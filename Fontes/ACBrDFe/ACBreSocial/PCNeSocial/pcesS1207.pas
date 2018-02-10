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
|* 29/02/2016: Guilherme Costa
|*  - Alterado os atributos que não estavam de acordo com o leiaute/xsd
******************************************************************************}
{$I ACBr.inc}

unit pcesS1207;

interface

uses
  SysUtils, Classes, Dialogs, Controls,
  pcnConversao, pcnGerador, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type

  TEvtBenPrRP = class;
  TS1207CollectionItem = class;
  TS1207Collection = class;
  TDMDevCollection = class;
  TDMDevCollectionItem = class;
  TIdeBenef = class;
  TItensCollection = class;
  TItensCollectionItem = class;

  TS1207Collection = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TS1207CollectionItem;
    procedure SetItem(Index: integer; Value: TS1207CollectionItem);
  public
    function Add: TS1207CollectionItem;
    property Items[Index: integer]: TS1207CollectionItem read GetItem write SetItem;
      default;
  end;

  TS1207CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtBenPrRP: TEvtBenPrRP;

    procedure seTEvtBenPrRP(const Value: TEvtBenPrRP);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtBenPrRP: TEvtBenPrRP read FEvtBenPrRP write seTEvtBenPrRP;
  end;

  TDMDevCollection = class(TCollection)
  private
    function GetItem(Index: integer): TDMDevCollectionItem;
    procedure SetItem(Index: integer; Value: TDMDevCollectionItem);
  public
    constructor Create(); reintroduce;

    function Add: TDMDevCollectionItem;
    property Items[Index: integer]: TDMDevCollectionItem read GetItem write SetItem;
      default;
  end;

  TDMDevCollectionItem = class(TCollectionItem)
  private
    FTpBenef: Integer;
    FNrBenefic: string;
    FIdeDmDev: string;
    FItens: TItensCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property tpBenef: integer read FTpBenef write FTpBenef;
    property nrBenefic: string read FNrBenefic write FNrBenefic;
    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property itens: TItensCollection read FItens write FItens;
  end;

  TItensCollection = class(TCollection)
  private
    function GetItem(Index: integer): TItensCollectionItem;
    procedure SetItem(Index: integer; Value: TItensCollectionItem);
  public
    constructor Create(); reintroduce;

    function Add: TItensCollectionItem;
    property Items[Index: integer]: TItensCollectionItem read GetItem write SetItem;
      default;
  end;

  TItensCollectionItem = class(TCollectionItem)
  private
    FCodRubr: string;
    FIdeTabRubr: string;
    FVrRubr: double;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property codRubr: string read FCodRubr write FCodRubr;
    property ideTabRubr: string read FIdeTabRubr write FIdeTabRubr;
    property vrRubr: double read FVrRubr write FVrRubr;
  end;

  TEvtBenPrRP = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FIdeBenef: TIdeBenef;
    FDMDev: TDMDevCollection;
    {Geradores específicos desta classe}

    procedure GerarIdeBenef;
    procedure GerarDmDev;
    procedure GerarItens(pItens: TItensCollection);
  public
    constructor Create(AACBreSocial: TObject); overload;
    destructor Destroy; override;

    function GerarXML(ATipoEmpregador: TEmpregador): boolean; override;

    property ideEvento: TIdeEvento3 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property ideBenef: TIdeBenef read FIdeBenef write FIdeBenef;
    property dmDev: TDMDevCollection read FDMDev write FDMDev;
  end;

  TIdeBenef = class(TPersistent)
  private
    FCpfBenef: string;
  public
    property cpfBenef: string read FCpfBenef write FCpfBenef;
  end;

implementation

{ TItensCollection }

constructor TItensCollection.Create;
begin
  inherited Create(TItensCollectionItem);
end;

function TItensCollection.Add: TItensCollectionItem;
begin
  Result := TItensCollectionItem(inherited add());
  Result.Create;
end;

function TItensCollection.GetItem(Index: integer): TItensCollectionItem;
begin
  Result := TItensCollectionItem(inherited GetItem(Index));
end;

procedure TItensCollection.SetItem(Index: integer; Value: TItensCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TItensCollectionItem }

constructor TItensCollectionItem.Create;
begin

end;

destructor TItensCollectionItem.Destroy;
begin
  inherited;
end;

{ TDMDevCollection }

constructor TDMDevCollection.Create;
begin
  inherited Create(TDMDevCollectionItem);
end;

function TDMDevCollection.Add: TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem(inherited add());
  Result.Create;
end;

function TDMDevCollection.GetItem(Index: integer): TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem(inherited GetItem(Index));
end;

procedure TDMDevCollection.SetItem(Index: integer; Value: TDMDevCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDMDevCollectionItem }

constructor TDMDevCollectionItem.Create;
begin
  FItens := TItensCollection.Create;
end;

destructor TDMDevCollectionItem.Destroy;
begin
  FItens.Free;

  inherited;
end;

{ TEvtBenPrRP }
constructor TEvtBenPrRP.Create(AACBreSocial: TObject);
begin
  inherited;

  FIdeEvento := TIdeEvento3.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeBenef := TIdeBenef.Create;
  FDMDev := TDMDevCollection.Create;
end;

destructor TEvtBenPrRP.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeBenef.Free;
  FDMDev.Free;

  inherited;
end;

procedure TEvtBenPrRP.GerarIdeBenef;
begin
  Gerador.wGrupo('ideBenef');

  Gerador.wCampo(tcStr, '', 'cpfBenef', 11, 11, 1, ideBenef.cpfBenef);

  Gerador.wGrupo('/ideBenef');
end;

procedure TEvtBenPrRP.GerarItens(pItens: TItensCollection);
var
  i: integer;
begin
  for i := 0 to pItens.Count - 1 do
  begin
    Gerador.wGrupo('itens');

    Gerador.wCampo(tcStr, '', 'codRubr',    1, 30, 1, pItens[i].codRubr);
    Gerador.wCampo(tcStr, '', 'ideTabRubr', 1,  8, 1, pItens[i].ideTabRubr);
    Gerador.wCampo(tcDe2, '', 'vrRubr',     1, 14, 1, pItens[i].vrRubr);

    Gerador.wGrupo('/itens');
  end;

  if pItens.Count > 99 then
    Gerador.wAlerta('', 'itens', 'Lista de Detalhamento de Valores', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtBenPrRP.GerarDmDev;
var
  i: integer;
begin
  for i := 0 to dmDev.Count - 1 do
  begin
    Gerador.wGrupo('dmDev');

    Gerador.wCampo(tcInt, '', 'tpBenef',   2,   2, 1, dmDev[i].tpBenef);
    Gerador.wCampo(tcStr, '', 'nrBenefic', 1, 200, 1, dmDev[i].nrBenefic);
    Gerador.wCampo(tcStr, '', 'ideDmDev',  1,  30, 1, dmDev[i].ideDmDev);

    GerarItens(dmDev[i].itens);

    Gerador.wGrupo('/dmDev');
  end;

  if dmDev.Count > 99 then
    Gerador.wAlerta('', 'dmDev', 'Lista de Demostrativos', ERR_MSG_MAIOR_MAXIMO + '99');
end;

function TEvtBenPrRP.GerarXML(ATipoEmpregador: TEmpregador): boolean;
begin
  try
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc,
     self.Sequencial, ATipoEmpregador);

    GerarCabecalho('evtBenPrRP');
    Gerador.wGrupo('evtBenPrRP Id="' + Self.Id + '"');

    GerarIdeEvento3(Self.IdeEvento);
    GerarIdeEmpregador(Self.ideEmpregador);
    GerarIdeBenef;
    GerarDmDev;

    Gerador.wGrupo('/evtBenPrRP');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtBenPrRP');

    Validar(schevtBenPrRP);
  except
    on e: Exception do
      raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

{ TS1207CollectionItem }
constructor TS1207CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS1207;
  FEvtBenPrRP := TEvtBenPrRP.Create(AOwner);
end;

destructor TS1207CollectionItem.Destroy;
begin
  FEvtBenPrRP.Free;

  inherited;
end;

procedure TS1207CollectionItem.seTEvtBenPrRP(const Value: TEvtBenPrRP);
begin
  FEvtBenPrRP.Assign(Value);
end;

{ TS1207Collection }
function TS1207Collection.Add: TS1207CollectionItem;
begin
  Result := TS1207CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS1207Collection.GetItem(Index: integer): TS1207CollectionItem;
begin
  Result := TS1207CollectionItem(inherited GetItem(Index));
end;

procedure TS1207Collection.SetItem(Index: integer; Value: TS1207CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

end.
