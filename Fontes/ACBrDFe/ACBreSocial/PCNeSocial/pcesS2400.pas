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
|* 01/06/2017: Guilherme Costa
|*  - Criação do evento
******************************************************************************}
{$I ACBr.inc}

unit pcesS2400;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2400Collection = class;
  TS2400CollectionItem = class;
  TEvtCdBenPrRP = class;
  TIdeBenef = class;
  TDadosBenef = class;
  TInfoBeneficio = class;
  TBeneficio = class;
  TInfoPenMorte = class;
  TFimBeneficio = class;

  TS2400Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2400CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2400CollectionItem);
  public
    function Add: TS2400CollectionItem;
    property Items[Index: Integer]: TS2400CollectionItem read GetItem write SetItem; default;
  end;

  TS2400CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtCdBenPrRP : TEvtCdBenPrRP;
    procedure setEvtCdBenPrRP(const Value: TEvtCdBenPrRP);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtCdBenPrRP: TEvtCdBenPrRP read FEvtCdBenPrRP write setEvtCdBenPrRP;
  end;

  TEvtCdBenPrRP = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeBenef: TIdeBenef;
    FInfoBeneficio: TInfoBeneficio;

    procedure GerarIdeBenef(pIdeBenef: TIdeBenef);
    procedure GerarDadosBenef(pDadosBenef: TDadosBenef);
    procedure GerarInfoBeneficio(pInfoBeneficio: TInfoBeneficio);
    procedure GerarBeneficio(pBeneficio: TBeneficio; pGroupName: String);
    procedure GerarInfoPenMorte(pInfoPenMorte: TInfoPenMorte);
    procedure GerarFimBeneficio(pFimBeneficio: TFimBeneficio);
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor Destroy; override;

    function GerarXML(ATipoEmpregador: TEmpregador): boolean; override;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property ideBenef: TIdeBenef read FIdeBenef write FIdeBenef;
    property infoBeneficio: TInfoBeneficio read FInfoBeneficio write FInfoBeneficio;
  end;

  TFimBeneficio = class(TPersistent)
  private
    FTpBenef: integer;
    FNrBenefic: string;
    FDtFimBenef: TDateTime;
    FMtvFim: integer;
  public
    property tpBenef: integer read FTpBenef write FTpBenef;
    property nrBenefic: string read FNrBenefic write FNrBenefic;
    property dtFimBenef: TDateTime read FDtFimBenef write FDtFimBenef;
    property mtvFim: Integer read FMtvFim write FMtvFim;
  end;

  TInfoPenMorte = class(TPersistent)
  private
    FIdQuota: String;
    FCpfInst: String;
  public
    property idQuota: string read FIdQuota write FIdQuota;
    property cpfInst: string read FCpfInst write FCpfInst;
  end;

  TBeneficio = class(TPersistent)
  private
    FTpBenef: integer;
    FNrBenefic: string;
    FDtIniBenef: TDateTime;
    FVrBenef: Double;
    FInfoPenMorte: TInfoPenMorte;

    function getInfoPenMorte: TInfoPenMorte;
  public
    constructor create;
    function infoPenMorteInst: boolean;

    property tpBenef: integer read FTpBenef write FTpBenef;
    property nrBenefic: string read FNrBenefic write FNrBenefic;
    property dtIniBenef: TDateTime read FDtIniBenef write FDtIniBenef;
    property vrBenef: Double read FVrBenef write FVrBenef;
    property infoPenMorte: TInfoPenMorte read getInfoPenMorte write FInfoPenMorte;
  end;

  TInfoBeneficio = class(TPersistent)
  private
    FTpPlanRP: tpPlanRP;
    FIniBeneficio: TBeneficio;
    FAltBeneficio: TBeneficio;
    FFimBeneficio: TFimBeneficio;

    function getIniBeneficio: TBeneficio;
    function getAltBeneficio: TBeneficio;
    function getFimBeneficio: TFimBeneficio;
  public
    constructor create;
    function iniBeneficioInst: boolean;
    function altBeneficioInst: boolean;
    function fimBeneficioInst: boolean;

    property tpPlanRP: tpPlanRP read FTpPlanRP write FTpPlanRP;
    property iniBeneficio: TBeneficio read getIniBeneficio write FIniBeneficio;
    property altBeneficio: TBeneficio read getAltBeneficio write FAltBeneficio;
    property fimBeneficio: TFimBeneficio read getFimBeneficio write FFimBeneficio;
  end;

  TDadosBenef = class(TPersistent)
  private
    FDadosNasc: TNascimento;
    FEndereco: TEndereco;
  public
    constructor Create;

    property dadosNasc: TNascimento read FDadosNasc write FDadosNasc;
    property endereco: TEndereco read FEndereco write FEndereco;
  end;

  TIdeBenef = class(TPersistent)
  private
    FCpfBEnef: string;
    FNmBenefic: string;
    FDadosBenef: TDadosBenef;
  public
    constructor Create;

    property cpfBenef: String read FCpfBEnef write FCpfBEnef;
    property nmBenefic: string read FNmBenefic write FNmBenefic;
    property dadosBenef: TDadosBenef read FDadosBenef write FDadosBenef;
  end;

implementation

{ TS2400Collection }

function TS2400Collection.Add: TS2400CollectionItem;
begin
  Result := TS2400CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2400Collection.GetItem(Index: Integer): TS2400CollectionItem;
begin
  Result := TS2400CollectionItem(inherited GetItem(Index));
end;

procedure TS2400Collection.SetItem(Index: Integer; Value: TS2400CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2400CollectionItem }

constructor TS2400CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS2400;
  FEvtCdBenPrRP := TEvtCdBenPrRP.Create(AOwner);
end;

destructor TS2400CollectionItem.Destroy;
begin
  FEvtCdBenPrRP.Free;

  inherited;
end;

procedure TS2400CollectionItem.setEvtCdBenPrRP(const Value: TEvtCdBenPrRP);
begin
  FEvtCdBenPrRP.Assign(Value);
end;

{ TEvtCdBenPrRP }

constructor TEvtCdBenPrRP.Create(AACBreSocial: TObject);
begin
  inherited;

  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeBenef := TIdeBenef.Create;
  FInfoBeneficio := TInfoBeneficio.Create;
end;

destructor TEvtCdBenPrRP.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeBenef.Free;
  FInfoBeneficio.Free;

  inherited;
end;

procedure TEvtCdBenPrRP.GerarDadosBenef(pDadosBenef: TDadosBenef);
begin
  Gerador.wGrupo('dadosBenef');

  GerarNascimento(pDadosBenef.dadosNasc, 'dadosNasc');

  GerarEndereco(pDadosBenef.endereco, Assigned(pDadosBenef.endereco.Exterior));

  Gerador.wGrupo('/dadosBenef');
end;

procedure TEvtCdBenPrRP.GerarIdeBenef(pIdeBenef: TIdeBenef);
begin
  Gerador.wGrupo('ideBenef');

  Gerador.wCampo(tcStr, '', 'cpfBenef',  11, 11, 1, pIdeBenef.cpfBenef);
  Gerador.wCampo(tcStr, '', 'nmBenefic',  0, 70, 1, pIdeBenef.nmBenefic);

  GerarDadosBenef(pIdeBenef.dadosBenef);

  Gerador.wGrupo('/ideBenef');
end;

procedure TEvtCdBenPrRP.GerarInfoPenMorte(pInfoPenMorte: TInfoPenMorte);
begin
  Gerador.wGrupo('infoPenMorte');

  Gerador.wCampo(tcStr, '', 'idQuota',  1, 30, 1, pInfoPenMorte.idQuota);
  Gerador.wCampo(tcStr, '', 'cpfInst', 11, 11, 1, pInfoPenMorte.cpfInst);

  Gerador.wGrupo('/infoPenMorte');
end;

procedure TEvtCdBenPrRP.GerarBeneficio(pBeneficio: TBeneficio; pGroupName: String);
begin
  Gerador.wGrupo(pGroupName);

  Gerador.wCampo(tcInt, '', 'tpBenef',     1,  2, 1, pBeneficio.tpBenef);
  Gerador.wCampo(tcStr, '', 'nrBenefic',   1, 20, 1, pBeneficio.nrBenefic);
  Gerador.wCampo(tcDat, '', 'dtIniBenef', 10, 10, 1, pBeneficio.dtIniBenef);
  Gerador.wCampo(tcDe2, '', 'vrBenef',     1, 14, 1, pBeneficio.vrBenef);

  if pBeneficio.infoPenMorteInst then
    GerarInfoPenMorte(pBeneficio.infoPenMorte);

  Gerador.wGrupo('/' + pGroupName);
end;

procedure TEvtCdBenPrRP.GerarFimBeneficio(pFimBeneficio: TFimBeneficio);
begin
  Gerador.wGrupo('fimBeneficio');

  Gerador.wCampo(tcInt, '', 'tpBenef',     1,  2, 1, pFimBeneficio.tpBenef);
  Gerador.wCampo(tcStr, '', 'nrBenefic',   1, 20, 1, pFimBeneficio.nrBenefic);
  Gerador.wCampo(tcDat, '', 'dtFimBenef', 10, 10, 1, pFimBeneficio.dtFimBenef);
  Gerador.wCampo(tcInt, '', 'mtvFim',      1,  2, 1, pFimBeneficio.mtvFim);

  Gerador.wGrupo('/fimBeneficio');
end;

procedure TEvtCdBenPrRP.GerarInfoBeneficio(pInfoBeneficio: TInfoBeneficio);
begin
  Gerador.wGrupo('infoBeneficio');

  Gerador.wCampo(tcStr, '', 'tpPlanRP', 1, 1, 1, eSTpPlanRPToStr(pInfoBeneficio.tpPlanRP));

  if pInfoBeneficio.iniBeneficioInst then
    GerarBeneficio(pInfoBeneficio.iniBeneficio, 'iniBeneficio');

  if pInfoBeneficio.altBeneficioInst then
    GerarBeneficio(pInfoBeneficio.altBeneficio, 'altBeneficio');

  if pInfoBeneficio.fimBeneficioInst then
    GerarFimBeneficio(pInfoBeneficio.fimBeneficio);

  Gerador.wGrupo('/infoBeneficio');
end;

function TEvtCdBenPrRP.GerarXML(ATipoEmpregador: TEmpregador): boolean;
begin
  try
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc,
     self.Sequencial, ATipoEmpregador);

    GerarCabecalho('evtCdBenPrRP');
    Gerador.wGrupo('evtCdBenPrRP Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeBenef(ideBenef);
    GerarInfoBeneficio(infoBeneficio);

    Gerador.wGrupo('/evtCdBenPrRP');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtCdBenPrRP');

    Validar('evtCdBenPrRP');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

{ TBeneficio }

constructor TBeneficio.Create;
begin
  FInfoPenMorte := nil;
end;

function TBeneficio.getInfoPenMorte: TInfoPenMorte;
begin
  if not Assigned(FInfoPenMorte) then
    FInfoPenMorte := TInfoPenMorte.Create;
  Result := FInfoPenMorte;
end;

function TBeneficio.infoPenMorteInst: boolean;
begin
  result := Assigned(FInfoPenMorte);
end;

{ TInfoBeneficio }

constructor TInfoBeneficio.Create;
begin
  FIniBeneficio := nil;
  FAltBeneficio := nil;
  FFimBeneficio := nil;
end;

function TInfoBeneficio.getIniBeneficio: TBeneficio;
begin
  if not Assigned(FIniBeneficio) then
    FIniBeneficio := TBeneficio.Create;
  result := FIniBeneficio;
end;

function TInfoBeneficio.getAltBeneficio: TBeneficio;
begin
  if not Assigned(FAltBeneficio) then
    FAltBeneficio := TBeneficio.Create;
  result := FAltBeneficio;
end;

function TInfoBeneficio.getFimBeneficio: TFimBeneficio;
begin
  if not Assigned(FFimBeneficio) then
    FFimBeneficio := TFimBeneficio.Create;
  Result := FFimBeneficio;
end;

function TInfoBeneficio.iniBeneficioInst: boolean;
begin
  result := Assigned(FIniBeneficio);
end;

function TInfoBeneficio.altBeneficioInst: boolean;
begin
  result := Assigned(FAltBeneficio);
end;

function TInfoBeneficio.fimBeneficioInst: boolean;
begin
  Result := Assigned(FFimBeneficio);
end;

{ TIdeBenef }

constructor TIdeBenef.Create;
begin
  FDadosBenef := TDadosBenef.Create;
end;

{ TDadosBenef }

constructor TDadosBenef.Create;
begin
  FDadosNasc := TNascimento.Create;
  FEndereco := TEndereco.Create;
end;

end.
