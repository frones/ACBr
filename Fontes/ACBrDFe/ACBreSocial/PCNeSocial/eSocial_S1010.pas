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
|* 29/02/2015: Guilherme Costa
|*  - não estava sendo gerada a tag "tpProc"
******************************************************************************}
{$I ACBr.inc}

unit eSocial_S1010;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador,
  eSocial_Common, eSocial_Conversao, eSocial_Gerador;

type
  TS1010Collection = class;
  TS1010CollectionItem = class;
  TEvtTabRubrica = class;
  TInfoRubrica = class;
  TDadosRubrica = class;
  TIdeRubrica = class;
  TIdeProcessoCPCollection = class;
  TIdeProcessoCPCollectionItem = class;
  TIdeProcessoIRRFCollection = class;
  TIdeProcessoFGTSCollection = class;
  TIdeProcessoSindCollection = class;

  TS1010Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS1010CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1010CollectionItem);
  public
    function Add: TS1010CollectionItem;
    property Items[Index: Integer]: TS1010CollectionItem read GetItem write SetItem; default;
  end;

  TS1010CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabRubrica: TEvtTabRubrica;
    procedure setEvtTabRubrica(const Value: TEvtTabRubrica);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabRubrica: TEvtTabRubrica read FEvtTabRubrica write setEvtTabRubrica;
  end;

  TProcessoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TProcesso;
    procedure SetItem(Index: Integer; Value: TProcesso);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TProcesso;
    property Items[Index: Integer]: TProcesso read GetItem write SetItem; default;
  end;

  TEvtTabRubrica = class(TeSocialEvento)
  private
    FModoLancamento: TModoLancamento;
    FIdeEmpregador: TIdeEmpregador;
    FIdeEvento: TIdeEvento;
    FInfoRubrica: TInfoRubrica;

    {Geradores específicos da classe}
    procedure GerarIdeRubrica;
    procedure GerarDadosRubrica;
    procedure GerarIdeProcessoCP;
    procedure GerarProcessos(pChave: string; pProcessoCollection: TProcessoCollection);
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor Destroy; override;

    function GerarXML: boolean; override;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property IdeEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property InfoRubrica: TInfoRubrica read FInfoRubrica write FInfoRubrica;
  end;

  TInfoRubrica = class
  private
    FDadosRubrica: TDadosRubrica;
    FideRubrica: TideRubrica;
    FnovaValidade: TidePeriodo;

    function getDadosRubrica: TDadosRubrica;
    function getNovaValidade: TidePeriodo;
  public
    constructor Create;
    destructor Destroy; override;

    function dadosRubricaInst(): Boolean;
    function novaValidadInst(): Boolean;

    property ideRubrica: TideRubrica read FideRubrica write FideRubrica;
    property DadosRubrica: TDadosRubrica read getDadosRubrica write FDadosRubrica;
    property novaValidade: TidePeriodo read getNovaValidade write FnovaValidade;
  end;

  TDadosRubrica = class
  private
    FDscRubr: string;
    FNatRubr: integer;
    FTpRubr: tpTpRubr;
    FCodIncCP: tpCodIncCP;
    FCodIncIRRF : tpCodIncIRRF;
    FCodIncFGTS : tpCodIncFGTS;
    FCodIncSIND: tpCodIncSIND;
    FObservacao: string;
    FIdeProcessoCP: TIdeProcessoCPCollection;
    FIdeProcessoIRRF: TIdeProcessoIRRFCollection;
    FIdeProcessoFGTS: TIdeProcessoFGTSCollection;
    FIdeProcessoSIND: TIdeProcessoSindCollection;

    function getIdeProcessoCP(): TIdeProcessoCPCollection;
    function getIdeProcessoIRRF(): TIdeProcessoIRRFCollection;
    function getIdeProcessoFGTS(): TIdeProcessoFGTSCollection;
    function getIdeProcessoSIND(): TIdeProcessoSindCollection;
  public
    constructor create;
    destructor Destroy; override;

    function ideProcessoCPInst(): Boolean;
    function ideProcessoIRRFInst(): Boolean;
    function ideProcessoFGTSInst(): Boolean;
    function ideProcessoSINDInst(): Boolean;

    property dscRubr: string read FDscRubr write FDscRubr;
    property natRubr: integer read FNatRubr write FNatRubr;
    property tpRubr: tpTpRubr read FTpRubr write FTpRubr;
    property codIncCP: tpCodIncCP read FCodIncCP write FCodIncCP;
    property codIncIRRF: tpCodIncIRRF read FCodIncIRRF write FCodIncIRRF;
    property codIncFGTS: tpCodIncFGTS read FCodIncFGTS write FCodIncFGTS;
    property codIncSIND: tpCodIncSIND read FCodIncSIND write FCodIncSIND;
    property observacao: string read FObservacao write FObservacao;
    property IdeProcessoCP: TIdeProcessoCPCollection read getIdeProcessoCP write FIdeProcessoCP;
    property IdeProcessoIRRF: TIdeProcessoIRRFCollection read getIdeProcessoIRRF write FIdeProcessoIRRF;
    property IdeProcessoFGTS: TIdeProcessoFGTSCollection read getIdeProcessoFGTS write FIdeProcessoFGTS;
    property IdeProcessoSIND: TIdeProcessoSindCollection read getIdeProcessoSIND write FIdeProcessoSIND;
  end;

  TIdeRubrica = class(TPersistent)
  private
    FCodRubr: string;
    FIdeTabRubr: string;
    FIniValid: string;
    FFimValid: string;
  public
    property CodRubr: string read FCodRubr write FCodRubr;
    property ideTabRubr: string read FIdeTabRubr write FIdeTabRubr;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TIdeProcessoCPCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TIdeProcessoCPCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeProcessoCPCollectionItem);
  public
    constructor create(); reintroduce;

    function Add: TIdeProcessoCPCollectionItem;
    property Items[Index: Integer]: TIdeProcessoCPCollectionItem read GetItem write SetItem;
  end;

  TIdeProcessoCPCollectionItem = class(TProcesso)
  private
    FtpProc: tpTpProc;
    FExtDecisao: TpExtDecisao;
  public
    constructor create; reintroduce;

    property tpProc: tpTpProc read FtpProc write FtpProc;
    property ExtDecisao: TpExtDecisao read FExtDecisao write FExtDecisao;
  end;

  TIdeProcessoIRRFCollection = class(TProcessoCollection)
  end;

  TIdeProcessoFGTSCollection = class(TProcessoCollection)
  end;

  TIdeProcessoSindCollection = class(TProcessoCollection)
  end;

implementation

uses
  eSocial_Tabelas;

{ TS1010Collection }

function TS1010Collection.Add: TS1010CollectionItem;
begin
  Result := TS1010CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS1010Collection.GetItem(Index: Integer): TS1010CollectionItem;
begin
  Result := TS1010CollectionItem(inherited GetItem(Index));
end;

procedure TS1010Collection.SetItem(Index: Integer;
  Value: TS1010CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS1010CollectionItem }

constructor TS1010CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS1010;
  FEvtTabRubrica := TEvtTabRubrica.Create(AOwner);
end;

destructor TS1010CollectionItem.Destroy;
begin
  FEvtTabRubrica.Free;

  inherited;
end;

procedure TS1010CollectionItem.setEvtTabRubrica(
  const Value: TEvtTabRubrica);
begin
  FEvtTabRubrica.Assign(Value);
end;

{ TEvtTabRubrica }

constructor TEvtTabRubrica.Create(AACBreSocial: TObject);
begin
  inherited;

  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeEvento := TIdeEvento.Create;
  FInfoRubrica := TInfoRubrica.Create;
end;

destructor TEvtTabRubrica.Destroy;
begin
  FIdeEmpregador.Free;
  FIdeEvento.Free;
  FInfoRubrica.Free;

  inherited;
end;

procedure TEvtTabRubrica.GerarDadosRubrica;
begin
  Gerador.wGrupo('dadosRubrica');

  Gerador.wCampo(tcStr, '', 'dscRubr',    1, 100, 1, InfoRubrica.dadosRubrica.dscRubr);
  Gerador.wCampo(tcInt, '', 'natRubr',    1,   4, 1, InfoRubrica.dadosRubrica.natRubr);
  Gerador.wCampo(tcStr, '', 'tpRubr',     1,   1, 1, eSTpRubrToStr(InfoRubrica.dadosRubrica.tpRubr));
  Gerador.wCampo(tcStr, '', 'codIncCP',   2,   2, 1, eSCodIncCPToStr(InfoRubrica.dadosRubrica.codIncCP));
  Gerador.wCampo(tcStr, '', 'codIncIRRF', 2,   2, 1, eSCodIncIRRFToStr(InfoRubrica.dadosRubrica.codIncIRRF));
  Gerador.wCampo(tcStr, '', 'codIncFGTS', 2,   2, 1, eSCodIncFGTSToStr(InfoRubrica.dadosRubrica.codIncFGTS));
  Gerador.wCampo(tcStr, '', 'codIncSIND', 2,   2, 1, eSCodIncSINDToStr(InfoRubrica.dadosRubrica.codIncSIND));
  Gerador.wCampo(tcStr, '', 'observacao', 0, 255, 0, InfoRubrica.dadosRubrica.observacao);

  GerarideProcessoCP;

  GerarProcessos('ideProcessoIRRF', InfoRubrica.dadosRubrica.IdeProcessoIRRF);
  GerarProcessos('ideProcessoFGTS', InfoRubrica.dadosRubrica.IdeProcessoFGTS);
  GerarProcessos('ideProcessoSIND', InfoRubrica.dadosRubrica.IdeProcessoSIND);

  Gerador.wGrupo('/dadosRubrica');
end;

procedure TEvtTabRubrica.GerarProcessos(pChave: String; pProcessoCollection: TProcessoCollection);
var
  i: Integer;
begin
  for i := 0 to pProcessoCollection.Count - 1 do
    GerarProcessoGenerico(pChave, pProcessoCollection[i]);

  if pProcessoCollection.Count > 99 then
    Gerador.wAlerta('', pChave, 'Lista de Processos: ' + pChave, ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtTabRubrica.GerarIdeProcessoCP;
var
  i: integer;
begin
  if (InfoRubrica.DadosRubrica.ideProcessoCPInst()) then
  begin
    for i := 0 to InfoRubrica.DadosRubrica.IdeProcessoCP.Count - 1 do
    begin
      Gerador.wGrupo('ideProcessoCP');

      Gerador.wCampo(tcStr, '', 'tpProc',     1,  1, 1, eSTpProcessoToStr(InfoRubrica.DadosRubrica.IdeProcessoCP.GetItem(i).tpProc));
      Gerador.wCampo(tcStr, '', 'nrProc',     1, 20, 1, InfoRubrica.DadosRubrica.IdeProcessoCP.GetItem(i).nrProc);
      Gerador.wCampo(tcStr, '', 'extDecisao', 1,  1, 1, eSExtDecisaoToStr(InfoRubrica.DadosRubrica.IdeProcessoCP.GetItem(i).extDecisao));

      if trim(InfoRubrica.DadosRubrica.IdeProcessoCP.GetItem(i).codSusp) <> '' then
        Gerador.wCampo(tcStr, '', 'codSusp', 1, 14, 1, InfoRubrica.DadosRubrica.IdeProcessoCP.GetItem(i).codSusp);

      Gerador.wGrupo('/ideProcessoCP');
    end;

    if InfoRubrica.DadosRubrica.IdeProcessoCP.Count > 99 then
      Gerador.wAlerta('', 'ideProcessoCP', 'Lista de Processos', ERR_MSG_MAIOR_MAXIMO + '99');
  end;
end;

procedure TEvtTabRubrica.GerarIdeRubrica;
begin
  Gerador.wGrupo('ideRubrica');

  Gerador.wCampo(tcStr, '', 'codRubr', 1, 30, 1, InfoRubrica.IdeRubrica.CodRubr);

  if (infoRubrica.ideRubrica.ideTabRubr <> '') then
    Gerador.wCampo(tcStr, '', 'ideTabRubr', 1, 8, 1, infoRubrica.ideRubrica.ideTabRubr);

  Gerador.wCampo(tcStr, '', 'iniValid', 7, 7, 1, infoRubrica.ideRubrica.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid', 7, 7, 0, infoRubrica.IdeRubrica.fimValid);

  Gerador.wGrupo('/ideRubrica');
end;

function TEvtTabRubrica.GerarXML: boolean;
begin
  try
    GerarCabecalho('evtTabRubrica');
    Gerador.wGrupo('evtTabRubrica Id="' + GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0) + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoRubrica');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeRubrica;

    if Self.ModoLancamento <> mlExclusao then
    begin
      GerarDadosRubrica;

      if (ModoLancamento = mlAlteracao) then
        if (InfoRubrica.novaValidadInst()) then
          GerarIdePeriodo(InfoRubrica.novaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoRubrica');
    Gerador.wGrupo('/evtTabRubrica');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabRubrica');

    Validar('evtTabRubrica');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

{ TInfoRubrica }

constructor TInfoRubrica.create;
begin
  FideRubrica := TideRubrica.Create;
  FDadosRubrica := nil;
  FnovaValidade := nil;
end;

function TInfoRubrica.dadosRubricaInst: Boolean;
begin
  Result := Assigned(FDadosRubrica);
end;

destructor TInfoRubrica.destroy;
begin
  FDadosRubrica.Free;
  FideRubrica.Free;
  FnovaValidade.Free;

  inherited;
end;

function TInfoRubrica.getDadosRubrica: TDadosRubrica;
begin
  if Not(Assigned(FDadosRubrica)) then
    FDadosRubrica := TDadosRubrica.create;

  Result := FDadosRubrica;
end;

function TInfoRubrica.getNovaValidade: TidePeriodo;
begin
  if Not(Assigned(FnovaValidade)) then
    FnovaValidade := TIdePeriodo.Create;

  Result := FnovaValidade;
end;

function TInfoRubrica.novaValidadInst: Boolean;
begin
  Result := Assigned(FnovaValidade);
end;

{ TIdeProcessoCPCollectionItem }

constructor TIdeProcessoCPCollectionItem.create;
begin

end;

{ TIdeProcessoCPCollection }

function TIdeProcessoCPCollection.Add: TIdeProcessoCPCollectionItem;
begin
  Result := TIdeProcessoCPCollectionItem(inherited Add());
  Result.Create;
end;

constructor TIdeProcessoCPCollection.create;
begin
  inherited create(TIdeProcessoCPCollectionItem);
end;

function TIdeProcessoCPCollection.GetItem(
  Index: Integer): TIdeProcessoCPCollectionItem;
begin
  Result := TIdeProcessoCPCollectionItem(Inherited GetItem(Index));
end;

procedure TIdeProcessoCPCollection.SetItem(Index: Integer;
  Value: TIdeProcessoCPCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

{ TDadosRubrica }

constructor TDadosRubrica.create;
begin
  FIdeProcessoCP := nil;
  FIdeProcessoIRRF := nil;
  FIdeProcessoFGTS := nil;
  FIdeProcessoSIND := nil;
end;

destructor TDadosRubrica.destroy;
begin
  FreeAndNil(FIdeProcessoCP);
  FreeAndNil(FIdeProcessoIRRF);
  FreeAndNil(FIdeProcessoFGTS);
  FreeAndNil(FIdeProcessoSIND);

  inherited;
end;

function TDadosRubrica.getIdeProcessoCP: TIdeProcessoCPCollection;
begin
  if Not(Assigned(FIdeProcessoCP)) then
    FIdeProcessoCP := TIdeProcessoCPCollection.Create;
  Result := FIdeProcessoCP;
end;

function TDadosRubrica.getIdeProcessoFGTS: TIdeProcessoFGTSCollection;
begin
  if Not(Assigned(FIdeProcessoFGTS)) then
    FIdeProcessoFGTS := TIdeProcessoFGTSCollection.Create(FIdeProcessoFGTS);
  Result := FIdeProcessoFGTS;
end;

function TDadosRubrica.getIdeProcessoIRRF: TIdeProcessoIRRFCollection;
begin
  if Not(Assigned(FIdeProcessoIRRF)) then
    FIdeProcessoIRRF := TIdeProcessoIRRFCollection.Create(FIdeProcessoIRRF);
  Result := FIdeProcessoIRRF;
end;

function TDadosRubrica.getIdeProcessoSIND: TIdeProcessoSindCollection;
begin
  if Not(Assigned(FIdeProcessoSIND)) then
    FIdeProcessoSIND := TIdeProcessoSINDCollection.Create(FIdeProcessoSIND);
  Result := FIdeProcessoSIND;
end;

function TDadosRubrica.ideProcessoCPInst: Boolean;
begin
  Result := Assigned(FIdeProcessoCP);
end;

function TDadosRubrica.ideProcessoFGTSInst: Boolean;
begin
  Result := Assigned(FIdeProcessoFGTS);
end;

function TDadosRubrica.ideProcessoIRRFInst: Boolean;
begin
  Result := Assigned(FIdeProcessoIRRF);
end;

function TDadosRubrica.ideProcessoSINDInst: Boolean;
begin
  Result := Assigned(FIdeProcessoSIND);
end;

{ TProcessoCollection }

function TProcessoCollection.Add: TProcesso;
begin
  Result := TProcesso(inherited Add());
  Result.Create;
end;

constructor TProcessoCollection.create(AOwner: TPersistent);
begin
  inherited create(TProcesso);
end;

function TProcessoCollection.GetItem(
  Index: Integer): TProcesso;
begin
  Result := TProcesso(Inherited GetItem(Index));
end;

procedure TProcessoCollection.SetItem(Index: Integer;
  Value: TProcesso);
begin
  Inherited SetItem(Index, Value);
end;

end.
