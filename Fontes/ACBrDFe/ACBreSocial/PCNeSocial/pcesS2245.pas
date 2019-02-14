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
|* 13/02/2019: Arce
|*  - Criada classes do evento S-2245
******************************************************************************}
{$I ACBr.inc}

unit pcesS2245;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2245Collection = class;
  TS2245CollectionItem = class;
  TEvtTreiCap = class;
  TTreiCap = class;
  TInfoComplem = class;
  TIdeProfRespCollection = class;
  TIdeProfRespCollectionItem = class;

  TS2245Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2245CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2245CollectionItem);
  public
    function Add: TS2245CollectionItem;
    property Items[Index: Integer]: TS2245CollectionItem read GetItem write SetItem; default;
  end;

  TS2245CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtTreiCap: TEvtTreiCap;
    procedure setEvtTreiCap(const Value: TEvtTreiCap);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor  Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTreiCap: TEvtTreiCap read FEvtTreiCap write setEvtTreiCap;
  end;

  TEvtTreiCap = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FTreiCap: TTreiCap;
    FACBreSocial: TObject;

    { Geradores da classe }
    procedure GerarTreiCap(objTreiCap: TTreiCap);
    procedure GerarInfoComplem(objInfoComplem: TInfoComplem);
    procedure GerarIdeProfResp(objIdeProfResp: TIdeProfRespCollection);
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property treiCap: TTreiCap read FTreiCap write FTreiCap;
  end;

  TTreiCap = class(TPersistent)
  private
    FcodTreiCap: String;
    FobsTreiCap: String;
    FInfoComplem: TInfoComplem;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property codTreiCap: String read FcodTreiCap write FcodTreiCap;
    property obsTreiCap: String read FobsTreiCap write FobsTreiCap;
    property infoComplem: TInfoComplem read FInfoComplem write FInfoComplem;
  end;

  TInfoComplem = class(TPersistent)
  private
    FdtTreiCap: TDateTime;
    FdurTreiCap: Double;
    FModTreiCap: tpModTreiCap;
    FTpTreiCap: tpTpTreiCap;
    FIdeProfResp: TIdeProfRespCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property dtTreiCap: TDateTime read FdtTreiCap write FdtTreiCap;
    property durTreiCap: Double read FdurTreiCap write FdurTreiCap;
    property modTreiCap: tpModTreiCap read FModTreiCap write FModTreiCap;
    property tpTreiCap: tpTpTreiCap read FTpTreiCap write FTpTreiCap;
    property ideProfResp: TIdeProfRespCollection read FIdeProfResp write FIdeProfResp;
  end;

  TIdeProfRespCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TIdeProfRespCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeProfRespCollectionItem);
  public
    constructor Create; reintroduce;

    function Add: TIdeProfRespCollectionItem;
    property Items[Index: Integer]: TIdeProfRespCollectionItem read GetItem write SetItem; default;
  end;

  TIdeProfRespCollectionItem = class(TCollectionItem)
  private
    FcpfProf: String;
    FNmProf: String;
    FTpProf: tpTpProf;
    FFormProf: String;
    FcodCBO: String;
    FnacProf: tpNacProf;
  public
    property cpfProf: string read FcpfProf write FcpfProf;
    property nmProf: string read FNmProf write FNmProf;
    property tpProf: tpTpProf read FTpProf write FTpProf;
    property formProf: string read FFormProf write FFormProf;
    property codCBO: string read FcodCBO write FcodCBO;
    property nacProf: tpNacProf read FnacProf write FnacProf;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS2245CollectionItem }

constructor TS2245CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS2245;
  FEvtTreiCap   := TEvtTreiCap.Create(AOwner);
end;

destructor TS2245CollectionItem.Destroy;
begin
  FEvtTreiCap.Free;

  inherited;
end;

procedure TS2245CollectionItem.setEvtTreiCap(const Value: TEvtTreiCap);
begin
  FEvtTreiCap.Assign(Value);
end;

{ TS2245Collection }

function TS2245Collection.Add: TS2245CollectionItem;
begin
  Result := TS2245CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2245Collection.GetItem(Index: Integer): TS2245CollectionItem;
begin
  Result := TS2245CollectionItem(inherited GetItem(Index));
end;

procedure TS2245Collection.SetItem(Index: Integer; Value: TS2245CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

constructor TEvtTreiCap.Create(AACBreSocial: TObject);
begin
  inherited;

  FACBreSocial   := AACBreSocial;
  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo    := TIdeVinculo.Create;
  FTreiCap  := TTreiCap.Create;
end;

destructor TEvtTreiCap.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FTreiCap.Free;

  inherited;
end;

procedure TEvtTreiCap.GerarIdeProfResp(objIdeProfResp: TIdeProfRespCollection);
var
  i: Integer;
begin
  for i := 0 to objIdeProfResp.Count - 1 do
  begin
    Gerador.wGrupo('ideProfResp');

    Gerador.wCampo(tcStr, '', 'cpfProf',  1,  11, 0, objIdeProfResp.Items[i].cpfProf);
    Gerador.wCampo(tcStr, '', 'nmProf',   1,   2, 1, objIdeProfResp.Items[i].nmProf);
    Gerador.wCampo(tcStr, '', 'tpProf',   1,   1, 1, tpTpProfToStr(objIdeProfResp.Items[i].tpProf));
    Gerador.wCampo(tcStr, '', 'formProf', 1, 255, 1, objIdeProfResp.Items[i].formProf);
    Gerador.wCampo(tcStr, '', 'codCBO',   1,   6, 1, objIdeProfResp.Items[i].codCBO);
    Gerador.wCampo(tcStr, '', 'nacProf',  1,   1, 1, tpNacProfToStr(objIdeProfResp.Items[i].nacProf));

    Gerador.wGrupo('/ideProfResp');
  end;

  if objIdeProfResp.Count > 99 then
    Gerador.wAlerta('', 'ideProfResp', 'Lista de Informações relativas ao profissional responsável pelo treinamento/capacitação/exercício simulado', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtTreiCap.GerarInfoComplem(objInfoComplem: TInfoComplem);
begin
  if (objInfoComplem.dtTreiCap > 0) then
  begin
    Gerador.wGrupo('infoComplem');

    Gerador.wCampo(tcDat, '', 'dtTreiCap',  10, 10, 1, objInfoComplem.dtTreiCap);
    Gerador.wCampo(tcDe2, '', 'durTreiCap',  1,  6, 1, objInfoComplem.durTreiCap);
    Gerador.wCampo(tcStr, '', 'modTreiCap',  1,  1, 1, tpModTreiCapToStr(objInfoComplem.modTreiCap));
    Gerador.wCampo(tcStr, '', 'tpTreiCap',   1,  1, 1, tpTpTreiCapToStr(objInfoComplem.tpTreiCap));

    GerarIdeProfResp(objInfoComplem.ideProfResp);

    Gerador.wGrupo('/infoComplem');
  end;  
end;

procedure TEvtTreiCap.GerarTreiCap(objTreiCap: TTreiCap);
begin
  Gerador.wGrupo('treiCap');

  Gerador.wCampo(tcStr, '', 'codTreiCap', 1,   4, 1, objTreiCap.codTreiCap);
  Gerador.wCampo(tcStr, '', 'obsTreiCap', 1, 999, 0, objTreiCap.obsTreiCap);

  GerarInfoComplem(objTreiCap.infoComplem);

  Gerador.wGrupo('/treiCap');
end;

function TEvtTreiCap.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, Self.ideEmpregador.NrInsc, Self.Sequencial);

    GerarCabecalho('evtTreiCap');
    Gerador.wGrupo('evtTreiCap Id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarTreiCap(Self.treiCap);

    Gerador.wGrupo('/evtTreiCap');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTreiCap');

    Validar(schevtTreiCap);

  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtTreiCap.LerArqIni(const AIniString: String): Boolean;
begin
  Result := False;

  { Implementar }

end;

{ TIdeProfRespCollection }

function TIdeProfRespCollection.Add: TIdeProfRespCollectionItem;
begin
  Result := TIdeProfRespCollectionItem(inherited Add);
end;

constructor TIdeProfRespCollection.Create;
begin
  inherited Create(TIdeProfRespCollectionItem);
end;

function TIdeProfRespCollection.GetItem(Index: Integer): TIdeProfRespCollectionItem;
begin
  Result := TIdeProfRespCollectionItem(inherited GetItem(index));
end;

procedure TIdeProfRespCollection.SetItem(Index: Integer; Value: TIdeProfRespCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoComplem }

constructor TInfoComplem.Create;
begin
  inherited;

  FIdeProfResp := TIdeProfRespCollection.Create;
end;

destructor TInfoComplem.Destroy;
begin
  FIdeProfResp.Free;

  inherited;
end;

{ TTreiCap }

constructor TTreiCap.Create;
begin
  inherited;

  FInfoComplem := TInfoComplem.Create;
end;

destructor TTreiCap.Destroy;
begin
  FInfoComplem.Free;
  
  inherited;
end;

end.
