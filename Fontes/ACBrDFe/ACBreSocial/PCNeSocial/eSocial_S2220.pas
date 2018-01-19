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
|*  - Alterações para validação com o XSD
******************************************************************************}
{$I ACBr.inc}

unit eSocial_S2220;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  eSocial_Common, eSocial_Conversao, eSocial_Consts, eSocial_Gerador;

type
  TS2220Collection = class;
  TS2220CollectionItem = class;
  TEvtASO = class;
  TAso = class;
  TExameColecaoItem = class;
  TExameColecao = class;
  TRespMonit = class;
  TMedico = class;
  TCrm = class;
  TIdeServSaude = class;
  
  TS2220Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2220CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2220CollectionItem);
  public
    function Add: TS2220CollectionItem;
    property Items[Index: Integer]: TS2220CollectionItem read GetItem write SetItem; default;
  end;

  TS2220CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtASO: TEvtASO;

    procedure setEvtASO(const Value: TEvtASO);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtASO: TEvtASO read FEvtASO write setEvtASO;
  end;

  TEvtASO = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FAso: TAso;

    procedure gerarExame;
    procedure gerarMedico;
    procedure gerarCRM;
    procedure gerarAso;
    procedure gerarIdeServSaude;
    procedure gerarRespMonit(pRespMonit: TRespMonit);
  public
    constructor Create(AACBreSocial: TObject); overload;
    destructor Destroy; override;

    function GerarXML: boolean; override;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property Aso: TAso read FAso write FAso;
  end;

  TAso = class(TPersistent)
  private
    FDtAso: TDateTime;
    FtpAso: tpTpAso;
    FResAso: tpResAso;

    FExame: TExameColecao;
    FIdeServSaude: TIdeServSaude;
  public
    constructor create;
    destructor destroy; override;

    property DtAso: TDateTime read FDtAso write FDtAso;
    property tpAso: tpTpAso read FtpAso write FtpAso;
    property ResAso: tpResAso read FResAso write FResAso;
    property Exame: TExameColecao read FExame write FExame;
    property IdeServSaude: TIdeServSaude read FIdeServSaude write FIdeServSaude;
  end;

  TExameColecaoItem = class(TCollectionItem)
  private
    FDtExm: TDateTime;
    FProcRealizado: integer;
    FObsProc: string;
    FInterprExm: tpInterprExm;
    FOrdExame: tpOrdExame;
    FDtIniMonit: TDateTime;
    FDtFimMonit: TDateTime;
    FIndResult: tpIndResult;
    FRespMonit: TRespMonit;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property DtExm: TDateTime read FDtExm write FDtExm;
    property ProcRealizado: integer read FProcRealizado write FProcRealizado;
    property obsProc: string read FObsProc write FObsProc;
    property interprExm: tpInterprExm read FInterprExm write FInterprExm;
    property ordExame: tpOrdExame read FOrdExame write FOrdExame;
    property dtIniMonit: TDateTime read FDtIniMonit write FDtIniMonit;
    property dtFimMonit: TDateTime read FDtFimMonit write FDtFimMonit;
    property indResult: tpIndResult read FIndResult write FIndResult;
    property respMonit: TRespMonit read FRespMonit write FRespMonit;
  end;

  TExameColecao = class(TCollection)
  private
    function GetItem(Index: Integer): TExameColecaoItem;
    procedure SetItem(Index: Integer; const Value: TExameColecaoItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TExameColecaoItem;
    property Items[Index: Integer]: TExameColecaoItem read GetItem write SetItem;
  end;

  TRespMonit = class
  private
    FNisResp: string;
    FNrConsClasse: string;
    FUfConsClasse: tpuf;
  public
    property NisResp: string read FNisResp write FNisResp;
    property NrConsClasse: string read FNrConsClasse write FNrConsClasse;
    property UfConsClasse: tpuf read FUfConsClasse write FUfConsClasse;
  end;

  TIdeServSaude = class
  private
    FCodCNES: string;
    FFrmCtt: string;
    FEmail: string;
    FMedico: TMedico;
  public
    constructor create;
    destructor destroy; override;
  published
    property CodCNES: string read FCodCNES write FCodCNES;
    property FrmCtt: string read FFrmCtt write FFrmCtt;
    property Email: string read FEmail write FEmail;
    property Medico: TMedico read FMedico write FMedico;
  end;

  TCrm = class
  private
    FNrCRM: string;
    FUfCRM: tpuf;
  published
    property NrCRM: string read FNrCRM write FNrCRM;
    property UfCRM: tpuf read FUfCRM write FUfCRM;
  end;


  TMedico = class
  private
    FNmMed: string;
    FCRM: TCRM;
  public
    constructor create;
    destructor destroy; override;
  public
    property NmMed: string read FNmMed write FNmMed;
    property CRM: TCRM read FCRM write FCRM;
  end;


implementation

uses
  eSocial_NaoPeriodicos;

{ TS2220Collection }

function TS2220Collection.Add: TS2220CollectionItem;
begin
  Result := TS2220CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2220Collection.GetItem(Index: Integer): TS2220CollectionItem;
begin
  Result := TS2220CollectionItem(inherited GetItem(Index));
end;

procedure TS2220Collection.SetItem(Index: Integer;
  Value: TS2220CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2220CollectionItem }

constructor TS2220CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS2220;
  FEvtASO     := TEvtASO.Create(AOwner);
end;

destructor TS2220CollectionItem.Destroy;
begin
  FEvtASO.Free;
  inherited;
end;

procedure TS2220CollectionItem.setEvtASO(const Value: TEvtASO);
begin
  FEvtASO.Assign(Value);
end;

{ TAso }

constructor TAso.create;
begin
  inherited;
  FExame := TExameColecao.Create(self);
  FIdeServSaude := TIdeServSaude.create;
end;

destructor TAso.destroy;
begin
  FExame.Free;
  FIdeServSaude.Free;
  inherited;
end;

{ TExameColecao }

function TExameColecao.Add: TExameColecaoItem;
begin
  Result := TExameColecaoItem(inherited Add);
  Result.Create;
end;

constructor TExameColecao.Create(AOwner: TPersistent);
begin
  inherited Create(TExameColecaoItem);
end;

function TExameColecao.GetItem(Index: Integer): TExameColecaoItem;
begin
  Result := TExameColecaoItem(inherited GetItem(Index));
end;

procedure TExameColecao.SetItem(Index: Integer;
  const Value: TExameColecaoItem);
begin
  inherited SetItem(Index, Value);
end;

{ TExameColecaoItem }

constructor TExameColecaoItem.Create;
begin
  FRespMonit := TRespMonit.Create;
end;

destructor TExameColecaoItem.Destroy;
begin
  FRespMonit.Free;
  inherited;
end;

{ TEvtASO }

constructor TEvtASO.Create(AACBreSocial: TObject);
begin
  inherited;
  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo := TIdeVinculo.Create;
  FAso := TAso.Create;
end;

destructor TEvtASO.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FAso.Free;
  inherited;
end;

procedure TEvtASO.gerarAso;
begin
  Gerador.wGrupo('aso');
    Gerador.wCampo(tcDat, '', 'dtAso', 0, 0, 0, self.Aso.DtAso);
    Gerador.wCampo(tcStr, '', 'tpAso', 0, 0, 0, eSTpAsoToStr(self.Aso.tpAso));
    Gerador.wCampo(tcStr, '', 'resAso', 0, 0, 0, eSResAsoToStr(self.Aso.ResAso));
    gerarExame;
    gerarIdeServSaude;
  Gerador.wGrupo('/aso');
end;

procedure TEvtASO.gerarCRM;
begin
  Gerador.wGrupo('crm');
    Gerador.wCampo(tcStr, '', 'nrCRM  ', 0, 0, 0, self.Aso.IdeServSaude.Medico.CRM.NrCRM);

    if (eSufToStr(self.Aso.IdeServSaude.Medico.CRM.UfCRM) <> '') then
      Gerador.wCampo(tcStr, '', 'ufCRM  ', 0, 0, 0, eSufToStr(self.Aso.IdeServSaude.Medico.CRM.UfCRM));
  Gerador.wGrupo('/crm');
end;

procedure TEvtASO.gerarExame;
var
  iContador: integer;
begin
  for iCOntador:= 0 to self.Aso.Exame.Count-1 do
  begin
    Gerador.wGrupo('exame');
      Gerador.wCampo(tcDat, '', 'dtExm', 0, 0, 0, self.Aso.Exame.Items[iContador].dtExm);
      Gerador.wCampo(tcStr, '', 'procRealizado', 0, 0, 0, self.Aso.Exame.Items[iContador].procRealizado);
      Gerador.wCampo(tcStr, '', 'obsProc', 0, 0, 0, self.Aso.Exame.Items[iContador].obsProc);
      Gerador.wCampo(tcInt, '', 'interprExm', 0, 0, 0, eSInterprExmToStr(self.Aso.Exame.Items[iContador].interprExm));
      Gerador.wCampo(tcInt, '', 'ordExame', 0, 0, 0, eSOrdExameToStr(self.Aso.Exame.Items[iContador].ordExame));
      Gerador.wCampo(tcDat, '', 'dtIniMonit', 0, 0, 0, self.Aso.Exame.Items[iContador].dtIniMonit);
      Gerador.wCampo(tcDat, '', 'dtFimMonit', 0, 0, 0, self.Aso.Exame.Items[iContador].dtFimMonit);
      Gerador.wCampo(tcInt, '', 'indResult', 0, 0, 0, eSIndResultToStr(self.Aso.Exame.Items[iContador].indResult));
      gerarRespMonit(self.Aso.Exame.Items[iContador].respMonit);
    Gerador.wGrupo('/exame');
  end;
end;

procedure TEvtASO.gerarIdeServSaude;
begin
  Gerador.wGrupo('ideServSaude');
    if (self.Aso.IdeServSaude.CodCNES <> '') then
      Gerador.wCampo(tcStr, '', 'codCNES  ', 0, 0, 0, self.Aso.IdeServSaude.CodCNES);

    Gerador.wCampo(tcStr, '', 'frmCtt  ', 0, 0, 0, self.Aso.IdeServSaude.FrmCtt);

    if (self.Aso.IdeServSaude.Email <> '') then
      Gerador.wCampo(tcStr, '', 'email  ', 0, 0, 0, self.Aso.IdeServSaude.Email);
    gerarMedico;
  Gerador.wGrupo('/ideServSaude');
end;

procedure TEvtASO.gerarMedico;
begin
  Gerador.wGrupo('medico');
    Gerador.wCampo(tcStr, '', 'nmMed', 0, 0, 0, self.Aso.IdeServSaude.Medico.NmMed);
    gerarCRM;
  Gerador.wGrupo('/medico');
end;

procedure TEvtASO.gerarRespMonit(pRespMonit: TRespMonit);
begin
  Gerador.wGrupo('respMonit');
    Gerador.wCampo(tcStr, '', 'nisResp', 0, 0, 0, pRespMonit.nisResp);
    Gerador.wCampo(tcStr, '', 'nrConsClasse', 0, 0, 0, pRespMonit.NrConsClasse);

    if (eSufToStr(pRespMonit.UfConsClasse) <> '') then
      Gerador.wCampo(tcStr, '', 'ufConsClasse', 0, 0, 0, eSufToStr(pRespMonit.UfConsClasse));
  Gerador.wGrupo('/respMonit');
end;

function TEvtASO.GerarXML: boolean;
begin
  try
    GerarCabecalho('evtMonit');
      Gerador.wGrupo('evtMonit Id="'+GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0)+'"');
        //gerarIdVersao(self);
        gerarIdeEvento2(self.IdeEvento);
        gerarIdeEmpregador(self.IdeEmpregador);
        gerarIdeVinculo(self.IdeVinculo);
        gerarAso;
      Gerador.wGrupo('/evtMonit');
    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtMonit');
    Validar('evtMonit');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

{ TMedico }

constructor TMedico.create;
begin
  FCRM := TCRM.Create;
end;

destructor TMedico.destroy;
begin
  FCRM.Free;
  inherited;
end;

{ TIdeServSaude }

constructor TIdeServSaude.create;
begin
  FMedico := TMedico.create;
end;

destructor TIdeServSaude.destroy;
begin
  FMedico.Free;
  inherited;
end;

end.
 
