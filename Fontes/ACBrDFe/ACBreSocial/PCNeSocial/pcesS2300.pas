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

unit pcesS2300;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2300Collection = class;
  TS2300CollectionItem = class;
  TEvtTSVInicio = class;
  TinfoTSVInicio = class;
  TinfoComplementares = class;
  TinfoDirSind = class;
  TinfoTrabCedido = class;
  TTermino = class;

  TS2300Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2300CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2300CollectionItem);
  public
    function Add: TS2300CollectionItem;
    property Items[Index: Integer]: TS2300CollectionItem read GetItem write SetItem; default;
  end;

  TS2300CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtTSVInicio: TEvtTSVInicio;
    procedure setEvtTSVInicio(const Value: TEvtTSVInicio);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor  Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTSVInicio: TEvtTSVInicio read FEvtTSVInicio write setEvtTSVInicio;
  end;

  TEvtTSVInicio = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FTrabalhador: TTrabalhador;
    FinfoTSVInicio : TinfoTSVInicio;

    procedure GerarInfoTSVInicio(obj : TinfoTSVInicio);
    procedure GerarInfoComplementares(obj: TinfoComplementares);
    procedure GerarCargoFuncao(obj: TcargoFuncao);
    procedure GerarRemuneracao(obj: TRemuneracao);
    procedure GerarFGTS(obj: TFGTS);
    procedure GerarinfoDirSind(obj: TinfoDirSind);
    procedure GerarinfoTrabCedido(obj: TinfoTrabCedido);
    procedure GerarinfoEstagiario(obj: TinfoEstagiario);
    procedure GerarInstEnsino(obj: TinstEnsino);
    procedure GerarageIntegracao(obj: TageIntegracao);
    procedure GerarsupervisorEstagio(obj: TsupervisorEstagio);
    procedure GerarTermino(obj: TTermino);
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor  Destroy; override;

    function GerarXML: boolean; override;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property Trabalhador: TTrabalhador read FTrabalhador write FTrabalhador;
    property infoTSVInicio : TinfoTSVInicio read FinfoTSVInicio write FInfoTSVInicio;
  end;

  TinfoTSVInicio = class(TPersistent)
  private
    FcadIni: tpSimNao;
    FcodCateg : Integer;
    FdtInicio : TDateTime;
    FnatAtividade : tpNatAtividade;
    FinfoComplementares : TinfoComplementares;
    Fafastamento: TAfastamento;
    Ftermino: TTermino;
  public
    constructor Create;
    destructor  Destroy; override;

    property cadIni: tpSimNao read FcadIni write FcadIni;
    property codCateg : Integer read FcodCateg write FcodCateg;
    property dtInicio : TDateTime read FdtInicio write FdtInicio;
    property natAtivididade : tpNatAtividade read FnatAtividade write FnatAtividade;
    property infoComplementares : TinfoComplementares read FinfoComplementares write FinfoComplementares;
    property afastamento: TAfastamento read Fafastamento write Fafastamento;
    property termino: TTermino read Ftermino write Ftermino;
  end;

  TinfoComplementares = class(TPersistent)
  private
    FcargoFuncao : TcargoFuncao;
    FRemuneracao : TRemuneracao;
    FFgts        : TFGTS;
    FinfoDirSind : TinfoDirSind;
    FinfoTrabCedido : TinfoTrabCedido;
    FinfoEstagiario : TinfoEstagiario;
  public
    constructor Create;
    destructor  Destroy; override;

    property cargoFuncao: TcargoFuncao read FcargoFuncao write FcargoFuncao;
    property Remuneracao: TRemuneracao read FRemuneracao write FRemuneracao;
    property Fgts : TFGTS read FFgts write FFgts;
    property infoDirSind: TinfoDirSind read FinfoDirSind write FinfoDirSind;
    property infoTrabCedido: TinfoTrabCedido read FinfoTrabCedido write FinfoTrabCedido;
    property infoEstagiario: TinfoEstagiario read FinfoEstagiario write FinfoEstagiario;
  end;

  TinfoDirSind = class(TPersistent)
  private
    FcategOrig : Integer;
    FcnpjOrigem : String;
    FdtAdmOrig : TDateTime;
    FmatricOrig : String;
  public
    property categOrig: Integer read FcategOrig write FcategOrig;
    property cnpjOrigem: String read FcnpjOrigem write FcnpjOrigem;
    property dtAdmOrig: TDateTime read FdtAdmOrig write FdtAdmOrig;
    property matricOrig: String read FmatricOrig write FmatricOrig;
  end;

  TinfoTrabCedido = class(TPersistent)
  private
    FcategOrig : Integer;
    FcnpjCednt : String;
    FmatricCed : String;
    FdtAdmCed : TDateTime;
    FTpRegTrab: tpTpRegTrab;
    FTpRegPrev: tpTpRegPrev;
    FinfOnus : tpInfOnus;
  public
    property categOrig: Integer read FcategOrig write FcategOrig;
    property cnpjCednt: String read FcnpjCednt write FcnpjCednt;
    property matricCed: String read FmatricCed write FmatricCed;
    property dtAdmCed: TDateTime read FdtAdmCed write FdtAdmCed;
    property tpRegTrab: tpTpRegTrab read FTpRegTrab write FTpRegTrab;
    property tpRegPrev: tpTpRegPrev read FTpRegPrev write FTpRegPrev;
    property infOnus: tpInfOnus read FinfOnus write FinfOnus;
  end;

  TTermino = class(TPersistent)
  private
    FdtTerm: TDateTime;
  public
    property dtTerm: TDateTime read FdtTerm write FdtTerm;
  end;
  
implementation

{ TS2300Collection }

function TS2300Collection.Add: TS2300CollectionItem;
begin
  Result := TS2300CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2300Collection.GetItem(Index: Integer): TS2300CollectionItem;
begin
  Result := TS2300CollectionItem(inherited GetItem(Index));
end;

procedure TS2300Collection.SetItem(Index: Integer; Value: TS2300CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2300CollectionItem }

constructor TS2300CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS2300;
  FEvtTSVInicio := TEvtTSVInicio.Create(AOwner);
end;

destructor TS2300CollectionItem.Destroy;
begin
  FEvtTSVInicio.Free;

  inherited;
end;

procedure TS2300CollectionItem.setEvtTSVInicio(const Value: TEvtTSVInicio);
begin
  FEvtTSVInicio.Assign(Value);
end;

{ TinfoTSVInicio }

constructor TinfoTSVInicio.Create;
begin
  inherited;

  FinfoComplementares := TinfoComplementares.Create;
  Fafastamento := TAfastamento.Create;
  Ftermino := TTermino.Create;
end;

destructor TinfoTSVInicio.Destroy;
begin
  FinfoComplementares.Free;
  Fafastamento.Free;
  Ftermino.Free;

  inherited;
end;

{ TinfoComplementares }

constructor TinfoComplementares.Create;
begin
  inherited;

  FcargoFuncao := TcargoFuncao.Create;
  FRemuneracao := TRemuneracao.Create;
  FFgts        := TFGTS.Create;
  FinfoDirSind := TinfoDirSind.Create;
  FinfoTrabCedido := TinfoTrabCedido.Create;
  FinfoEstagiario := TinfoEstagiario.Create;
end;

destructor TinfoComplementares.Destroy;
begin
  FcargoFuncao.Free;
  FRemuneracao.Free;
  FFgts.Free;
  FinfoDirSind.Free;
  FinfoTrabCedido.Free;
  FinfoEstagiario.Free;

  inherited;
end;

{ TEvtTSVInicio }

constructor TEvtTSVInicio.Create(AACBreSocial: TObject);
begin
  inherited;

  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FTrabalhador := TTrabalhador.Create;
  FinfoTSVInicio := TinfoTSVInicio.Create;
end;

destructor TEvtTSVInicio.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FTrabalhador.Free;
  FinfoTSVInicio.Free;

  inherited;
end;

procedure TEvtTSVInicio.GerarageIntegracao(obj: TageIntegracao);
begin
  Gerador.wGrupo('ageIntegracao');

  Gerador.wCampo(tcStr, '', 'cnpjAgntInteg', 14,  14, 1, obj.cnpjAgntInteg);
  Gerador.wCampo(tcStr, '', 'nmRazao',        1, 100, 1, obj.nmRazao);
  Gerador.wCampo(tcStr, '', 'dscLograd',      1,  80, 1, obj.dscLograd);
  Gerador.wCampo(tcStr, '', 'nrLograd',       1,  10, 1, obj.nrLograd);
  Gerador.wCampo(tcStr, '', 'bairro',         1,  60, 0, obj.bairro);
  Gerador.wCampo(tcStr, '', 'cep',            1,   8, 1, obj.cep);
  Gerador.wCampo(tcStr, '', 'codMunic',       7,   7, 0, obj.codMunic);
  Gerador.wCampo(tcStr, '', 'uf',             2,   2, 1, eSufToStr(obj.uf));

  Gerador.wGrupo('/ageIntegracao');
end;

procedure TEvtTSVInicio.GerarCargoFuncao(obj: TcargoFuncao);
begin
  if obj.codCargo <> EmptyStr then
  begin
    Gerador.wGrupo('cargoFuncao');

    Gerador.wCampo(tcStr, '', 'codCargo',  1, 30, 1, obj.codCargo);
    Gerador.wCampo(tcStr, '', 'codFuncao', 1, 30, 0, obj.codFuncao);

    Gerador.wGrupo('/cargoFuncao');
  end;
end;

procedure TEvtTSVInicio.GerarFGTS(obj: TFGTS);
begin
  if obj.dtOpcFGTS > 0 then
  begin
    Gerador.wGrupo('fgts');

    Gerador.wCampo(tcStr, '', 'opcFGTS',    1,  1, 1, obj.opcFGTS);
    Gerador.wCampo(tcDat, '', 'dtOpcFGTS', 10, 10, 0, obj.dtOpcFGTS);

    Gerador.wGrupo('/fgts');
  end;
end;

procedure TEvtTSVInicio.GerarInfoComplementares(obj: TinfoComplementares);
begin
  if (obj.cargoFuncao.codCargo <> EmptyStr) or (obj.Remuneracao.VrSalFx > 0) or
     (obj.FGTS.DtOpcFGTS> 0) or (obj.infoDirSind.dtAdmOrig > 0) or
     (obj.infoTrabCedido.dtAdmCed>0) or (obj.infoEstagiario.dtPrevTerm > 0) then
  begin
    Gerador.wGrupo('infoComplementares');

    GerarCargoFuncao(obj.cargoFuncao);
    GerarRemuneracao(obj.Remuneracao);
    GerarFGTS(obj.FGTS);
    GerarinfoDirSind(obj.infoDirSind);
    GerarinfoTrabCedido(obj.infoTrabCedido);
    GerarinfoEstagiario(obj.infoEstagiario);

    Gerador.wGrupo('/infoComplementares');
  end;
end;

procedure TEvtTSVInicio.GerarinfoDirSind(obj: TinfoDirSind);
begin
  if obj.dtAdmOrig > 0 then
  begin
    Gerador.wGrupo('infoDirigenteSindical');

    Gerador.wCampo(tcStr, '', 'categOrig',   1,  3, 1, obj.categOrig);
    Gerador.wCampo(tcStr, '', 'cnpjOrigem', 14, 14, 0, obj.cnpjOrigem);
    Gerador.wCampo(tcDat, '', 'dtAdmOrig',  10, 10, 0, obj.dtAdmOrig);
    Gerador.wCampo(tcStr, '', 'matricOrig',  1, 30, 0, obj.matricOrig);

    Gerador.wGrupo('/infoDirigenteSindical');
  end;
end;

procedure TEvtTSVInicio.GerarinfoEstagiario(obj: TinfoEstagiario);
begin
  if obj.dtPrevTerm > 0 then
  begin
    Gerador.wGrupo('infoEstagiario');

    Gerador.wCampo(tcStr, '', 'natEstagio',   1,  1, 1, obj.natEstagio);
    Gerador.wCampo(tcStr, '', 'nivEstagio',   1,  1, 1, obj.nivEstagio);
    Gerador.wCampo(tcStr, '', 'areaAtuacao',  1, 50, 0, obj.areaAtuacao);
    Gerador.wCampo(tcStr, '', 'nrApol',       1, 30, 0, obj.nrApol);
    Gerador.wCampo(tcDe2, '', 'vlrBolsa',     1, 14, 0, obj.vlrBolsa);
    Gerador.wCampo(tcDat, '', 'dtPrevTerm',  10, 10, 1, obj.dtPrevTerm);

    GerarInstEnsino(obj.instEnsino);
    GerarageIntegracao(obj.ageIntegracao);
    GerarsupervisorEstagio(obj.supervisorEstagio);

    Gerador.wGrupo('/infoEstagiario');
  end;
end;

procedure TEvtTSVInicio.GerarinfoTrabCedido(obj: TinfoTrabCedido);
begin
  if obj.dtAdmCed > 0 then
  begin
    Gerador.wGrupo('infoTrabCedido');

    Gerador.wCampo(tcStr, '', 'categOrig',  1,  3, 1, obj.categOrig);
    Gerador.wCampo(tcStr, '', 'cnpjCednt', 14, 14, 1, obj.cnpjCednt);
    Gerador.wCampo(tcStr, '', 'matricCed',  1, 30, 1, obj.matricCed);
    Gerador.wCampo(tcDat, '', 'dtAdmCed',  10, 10, 1, obj.dtAdmCed);
    Gerador.wCampo(tcInt, '', 'tpRegTrab',  1,  1, 1, eSTpRegTrabToStr(obj.tpRegTrab));
    Gerador.wCampo(tcInt, '', 'tpRegPrev',  1,  1, 1, eSTpRegPrevToStr(obj.tpRegPrev));
    Gerador.wCampo(tcStr, '', 'infOnus',    1,  1, 1, obj.infOnus);

    Gerador.wGrupo('/infoTrabCedido');
  end;
end;

procedure TEvtTSVInicio.GerarInfoTSVInicio(obj: TinfoTSVInicio);
begin
  Gerador.wGrupo('infoTSVInicio');

  Gerador.wCampo(tcStr, '', 'cadIni',        1,  1, 1, eSSimNaoToStr(obj.cadIni));
  Gerador.wCampo(tcStr, '', 'codCateg',      0,  3, 1, obj.codCateg);
  Gerador.wCampo(tcDat, '', 'dtInicio',     10, 10, 1, obj.dtInicio);
  Gerador.wCampo(tcStr, '', 'natAtividade',  1,  1, 0, ord(obj.natAtivididade) + 1);

  GerarInfoComplementares(obj.InfoComplementares);

  GerarAfastamento(obj.afastamento);
  GerarTermino(obj.termino);

  Gerador.wGrupo('/infoTSVInicio');
end;

procedure TEvtTSVInicio.GerarInstEnsino(obj: TinstEnsino);
begin
  Gerador.wGrupo('instEnsino');

  Gerador.wCampo(tcStr, '', 'cnpjInstEnsino', 14,  14, 0, obj.cnpjInstEnsino);
  Gerador.wCampo(tcStr, '', 'nmRazao',         1, 100, 1, obj.nmRazao);
  Gerador.wCampo(tcStr, '', 'dscLograd',       1,  80, 0, obj.dscLograd);
  Gerador.wCampo(tcStr, '', 'nrLograd',        1,  10, 0, obj.nrLograd);
  Gerador.wCampo(tcStr, '', 'bairro',          1,  60, 0, obj.bairro);
  Gerador.wCampo(tcStr, '', 'cep',             1,   8, 0, obj.cep);
  Gerador.wCampo(tcStr, '', 'codMunic',        7,   7, 0, obj.codMunic);
  Gerador.wCampo(tcStr, '', 'uf',              2,   2, 0, eSufToStr(obj.uf));

  Gerador.wGrupo('/instEnsino');
end;

procedure TEvtTSVInicio.GerarRemuneracao(obj: TRemuneracao);
begin
  if obj.vrSalFx > 0 then
  begin
    Gerador.wGrupo('remuneracao');

    Gerador.wCampo(tcDe2, '', 'vrSalFx',    1,  14, 1, obj.vrSalFx);
    Gerador.wCampo(tcStr, '', 'undSalFixo', 1,   1, 1, obj.undSalFixo);
    Gerador.wCampo(tcStr, '', 'dscSalVar',  1, 255, 0, obj.dscSalVar);

    Gerador.wGrupo('/remuneracao');
  end;
end;

procedure TEvtTSVInicio.GerarsupervisorEstagio(obj: TsupervisorEstagio);
begin
  if obj.cpfSupervisor <> EmptyStr then
  begin
    Gerador.wGrupo('supervisorEstagio');

    Gerador.wCampo(tcStr, '', 'cpfSupervisor', 11, 11, 1, obj.cpfSupervisor);
    Gerador.wCampo(tcStr, '', 'nmSuperv',       1, 70, 1, obj.nmSuperv);

    Gerador.wGrupo('/supervisorEstagio');
  end;
end;

procedure TEvtTSVInicio.GerarTermino(obj: TTermino);
begin
  if obj.dtTerm > 0 then
  begin
    Gerador.wGrupo('termino');

    Gerador.wCampo(tcDat, '', 'dtTerm', 10, 10, 1, obj.dtTerm);

    Gerador.wGrupo('/termino');
  end;
end;

function TEvtTSVInicio.GerarXML: boolean;
begin
  try
    GerarCabecalho('evtTSVInicio');
    Gerador.wGrupo('evtTSVInicio Id="' + GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0) + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    GerarTrabalhador(self.Trabalhador, 'trabalhador', 3);
    GerarInfoTSVInicio(self.infoTSVInicio);

    Gerador.wGrupo('/evtTSVInicio');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTSVInicio');

    Validar('evtTSVInicio');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

end.
