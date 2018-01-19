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

unit eSocial_S1050;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  eSocial_Common, eSocial_Conversao, eSocial_Gerador;


type
  TS1050Collection = class;
  TS1050CollectionItem = class;
  TEvtTabHorTur = class;
  TideHorContratual = class;
  TDadosHorContratual = class;
  TInfoHorContratual = class;
  THorarioIntervaloCollectionItem = class;
  THorarioIntervaloCollection = class;

  TS1050Collection = class(TOwnedCollection)
   private
    function GetItem(Index: Integer): TS1050CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1050CollectionItem);
  public
    function Add: TS1050CollectionItem;
    property Items[Index: Integer]: TS1050CollectionItem read GetItem write SetItem; default;
  end;

  TS1050CollectionItem = class(TCollectionItem)
   private
    FTipoEvento: TTipoEvento;
    FEvtTabHorContratual: TEvtTabHorTur;
    procedure setEvtTabHorContratual(const Value: TEvtTabHorTur);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabHorContratual: TEvtTabHorTur read FEvtTabHorContratual write setEvtTabHorContratual;
  end;

  TEvtTabHorTur = class(TESocialEvento)
   private
    FModoLancamento: TModoLancamento;
    fIdeEvento: TIdeEvento;
    fIdeEmpregador: TIdeEmpregador;
    fInfoHorContratual: TInfoHorContratual;

    {Geradores específicos da classe}
    procedure gerarDadosHorContratual();
    procedure gerarHorarioIntervalo();
    procedure gerarIdeHorContratual();
  public
    constructor Create(AACBreSocial: TObject); overload;
    destructor  Destroy; override;

    function GerarXML: boolean; override;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property IdeEvento: TIdeEvento read fIdeEvento write fIdeEvento;
    property IdeEmpregador: TIdeEmpregador read fIdeEmpregador write fIdeEmpregador;
    property InfoHorContratual: TInfoHorContratual read fInfoHorContratual write fInfoHorContratual;
  end;

  THorarioIntervaloCollection = class(TCollection)
   private
    function GetItem(Index: Integer): THorarioIntervaloCollectionItem;
    procedure SetItem(Index: Integer; Value: THorarioIntervaloCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: THorarioIntervaloCollectionItem;
    property Items[Index: Integer]: THorarioIntervaloCollectionItem read GetItem write SetItem;
  end;

  THorarioIntervaloCollectionItem = class(TCollectionItem)
    private
    FTpInterv : tpTpIntervalo;
    FDurInterv: integer;
    FIniInterv: string;
    FTermInterv : string;
  public
    constructor create; reintroduce;

    property tpInterv: tpTpIntervalo read FTpInterv write FTpInterv;
    property durInterv: integer read FDurInterv write FDurInterv;
    property iniInterv: string read FIniInterv write FIniInterv;
    property termInterv: string read FTermInterv write FTermInterv;
  end;

  TideHorContratual = class(TPersistent)
   private
    FCodHorContrat: string;
    FIniValid: string;
    FFimValid: string;
  public
    property codHorContrat: string read FCodHorContrat write FCodHorContrat;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TDadosHorContratual = class(TPersistent)
   private
    FHrEntr: string;
    FHrSaida: string;
    FDurJornada: integer;
    FPerHorFlexivel: tpSimNao;
    FHorarioIntervalo: THorarioIntervaloCollection;
  public
    constructor create;
    destructor destroy; override;

    property hrEntr: string read FHrEntr write FHrEntr;
    property hrSaida: string read FHrSaida write FHrSaida;
    property durJornada: integer read FDurJornada write FDurJornada;
    property perHorFlexivel: tpSimNao read FPerHorFlexivel write FPerHorFlexivel;
    property horarioIntervalo: THorarioIntervaloCollection read FHorarioIntervalo write FHorarioIntervalo;
  end;

  TInfoHorContratual = class(TPersistent)
   private
    fideHorContratual: TideHorContratual;
    fdadosHorContratual: TdadosHorContratual;
    fnovaValidade : TIdePeriodo;
    function getDadosHorContratual: TdadosHorContratual;
    function getNovaValidade: TIdePeriodo;
  public
    constructor create;
    destructor destroy; override;
    function dadosHorContratualInst(): Boolean;
    function novaValidadeInst(): Boolean;

    property ideHorContratual: TideHorContratual read fideHorContratual write fideHorContratual;
    property dadosHorContratual: TdadosHorContratual read getDadosHorContratual write fdadosHorContratual;
    property novaValidade: TIdePeriodo read getNovaValidade write fnovaValidade;
  end;

implementation

uses
  eSocial_Tabelas;

{ TS1050Collection }

function TS1050Collection.Add: TS1050CollectionItem;
begin
  Result := TS1050CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS1050Collection.GetItem(Index: Integer): TS1050CollectionItem;
begin
  Result := TS1050CollectionItem(inherited GetItem(Index));
end;

procedure TS1050Collection.SetItem(Index: Integer;
  Value: TS1050CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS1050CollectionItem }

constructor TS1050CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS1050;
  FEvtTabHorContratual := TEvtTabHorTur.Create(AOwner);
end;

destructor TS1050CollectionItem.Destroy;
begin
  FEvtTabHorContratual.Free;
  inherited;
end;

procedure TS1050CollectionItem.setEvtTabHorContratual(
  const Value: TEvtTabHorTur);
begin
  FEvtTabHorContratual.Assign(Value);
end;

{ TdadosHorContratual }

constructor TdadosHorContratual.create;
begin
  FHorarioIntervalo := THorarioIntervaloCollection.Create;
end;

destructor TdadosHorContratual.destroy;
begin
  FHorarioIntervalo.Free;
  inherited;
end;

{ TInfoHorContratual }

constructor TInfoHorContratual.create;
begin
  fideHorContratual := TideHorContratual.Create;
  fdadosHorContratual := nil;
  fnovaValidade := nil;
end;

function TInfoHorContratual.dadosHorContratualInst: Boolean;
begin
  Result := Assigned(fdadosHorContratual);
end;

destructor TInfoHorContratual.destroy;
begin
  fideHorContratual.Free;
  FreeAndNil(fdadosHorContratual);
  FreeAndNil(fnovaValidade);
  inherited;
end;

function TInfoHorContratual.getDadosHorContratual: TdadosHorContratual;
begin
  if Not(Assigned(fdadosHorContratual)) then
    fdadosHorContratual := TDadosHorContratual.create;
  Result := fdadosHorContratual;
end;

function TInfoHorContratual.getNovaValidade: TIdePeriodo;
begin
  if Not(Assigned(fnovaValidade)) then
    fnovaValidade := TIdePeriodo.Create;
  Result := fnovaValidade;
end;

function TInfoHorContratual.novaValidadeInst: Boolean;
begin
  Result := Assigned(fnovaValidade);
end;

{ TEvtTabHorContratual }

constructor TEvtTabHorTur.Create(AACBreSocial: TObject);
begin
  inherited;
  fIdeEvento := TIdeEvento.Create;
  fIdeEmpregador := TIdeEmpregador.Create;
  fInfoHorContratual := TInfoHorContratual.Create;
end;

destructor TEvtTabHorTur.Destroy;
begin
  fIdeEvento.Free;
  fIdeEmpregador.Free;
  fInfoHorContratual.Free;
  inherited;
end;

procedure TEvtTabHorTur.gerarDadosHorContratual;
begin
  Gerador.wGrupo('dadosHorContratual');
    Gerador.wCampo(tcStr, '', 'hrEntr', 0, 0, 0, self.InfoHorContratual.dadosHorContratual.hrEntr);
    Gerador.wCampo(tcStr, '', 'hrSaida', 0, 0, 0, self.InfoHorContratual.dadosHorContratual.hrSaida);
    Gerador.wCampo(tcStr, '', 'durJornada', 0, 0, 0, self.InfoHorContratual.dadosHorContratual.durJornada);
    Gerador.wCampo(tcStr, '', 'perHorFlexivel', 0, 0, 0, eSSimNaoToStr(self.InfoHorContratual.dadosHorContratual.perHorFlexivel));
    gerarHorarioIntervalo();
  Gerador.wGrupo('/dadosHorContratual');
end;


procedure TEvtTabHorTur.gerarHorarioIntervalo;
  var
      iHorarioIntervalo: Integer;
      objHorarioIntervalo: THorarioIntervaloCollectionItem;
begin
  for iHorarioIntervalo := 0 to InfoHorContratual.dadosHorContratual.horarioIntervalo.Count - 1 do
  begin
    objHorarioIntervalo := InfoHorContratual.dadosHorContratual.horarioIntervalo.Items[iHorarioIntervalo];
    Gerador.wGrupo('horarioIntervalo');
      Gerador.wCampo(tcStr, '', 'tpInterv', 0, 0, 0, eSTpIntervaloToStr(objHorarioIntervalo.tpInterv));
      Gerador.wCampo(tcStr, '', 'durInterv', 0, 0, 0, objHorarioIntervalo.durInterv);

      if (eSTpIntervaloToStr(objHorarioIntervalo.tpInterv) = '1') then
      begin
        Gerador.wCampo(tcStr, '', 'iniInterv', 0, 0, 0, objHorarioIntervalo.iniInterv);
        Gerador.wCampo(tcStr, '', 'termInterv', 0, 0, 0, objHorarioIntervalo.termInterv);
      end;
    Gerador.wGrupo('/horarioIntervalo');
  end;
end;

procedure TEvtTabHorTur.gerarIdeHorContratual;
begin
  Gerador.wGrupo('ideHorContratual');
    Gerador.wCampo(tcStr, '', 'codHorContrat', 0, 0, 0, InfoHorContratual.ideHorContratual.codHorContrat);
    Gerador.wCampo(tcStr, '', 'iniValid', 0, 0, 0, InfoHorContratual.ideHorContratual.iniValid);
    Gerador.wCampo(tcStr, '', 'fimValid', 0, 0, 0, InfoHorContratual.ideHorContratual.fimValid);
  Gerador.wGrupo('/ideHorContratual');
end;

function TEvtTabHorTur.GerarXML: boolean;
begin
  try
    gerarCabecalho('evtTabHorTur');
    Gerador.wGrupo('evtTabHorTur Id="'+ GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0) +'"');
    //gerarIdVersao(self);
    gerarIdeEvento(self.IdeEvento);
    gerarIdeEmpregador(self.IdeEmpregador);
    Gerador.wGrupo('infoHorContratual');
      gerarModoAbertura(Self.ModoLancamento);
        gerarIdeHorContratual();
        if Self.ModoLancamento <> mlExclusao then
        begin
          gerarDadosHorContratual();
          if Self.ModoLancamento = mlAlteracao then
            if (InfoHorContratual.novaValidadeInst()) then
              GerarIdePeriodo(self.InfoHorContratual.novaValidade, 'novaValidade');
        end;
      gerarModoFechamento(Self.ModoLancamento);
    Gerador.wGrupo('/infoHorContratual');
    Gerador.wGrupo('/evtTabHorTur');
    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabHorTur');
    Validar('evtTabHorTur');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '') 
end;

{ THorarioIntervaloCollectionItem }

constructor THorarioIntervaloCollectionItem.create;
begin
end;

{ THorarioIntervaloCollection }

function THorarioIntervaloCollection.Add: THorarioIntervaloCollectionItem;
begin
  Result := THorarioIntervaloCollectionItem(inherited Add);
  Result.create;
end;

constructor THorarioIntervaloCollection.Create;
begin
  inherited create(THorarioIntervaloCollectionItem);
end;

function THorarioIntervaloCollection.GetItem(
  Index: Integer): THorarioIntervaloCollectionItem;
begin
  Result := THorarioIntervaloCollectionItem(inherited GetItem(Index));
end;

procedure THorarioIntervaloCollection.SetItem(Index: Integer;
  Value: THorarioIntervaloCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

end.
 
