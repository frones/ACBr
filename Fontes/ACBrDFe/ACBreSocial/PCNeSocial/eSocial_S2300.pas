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

unit eSocial_S2300;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  eSocial_Common, eSocial_Conversao, eSocial_Gerador;

type
  TS2300Collection = class;
  TS2300CollectionItem = class;
  TEvtTSVInicio = class;
  TinfoTSVInicio = class;
  TinfoComplementares = class;
  TinfoDirSind = class;
  TinfoTrabCedido = class;

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

    procedure gerarInfoTSVInicio(obj : TinfoTSVInicio);
    procedure gerarInfoComplementares(obj: TinfoComplementares);
    procedure gerarCargoFuncao(obj: TcargoFuncao);
    procedure gerarRemuneracao(obj: TRemuneracao);
    procedure gerarFGTS(obj: TFGTS);
    procedure gerarinfoDirSind(obj: TinfoDirSind);
    procedure gerarinfoTrabCedido(obj: TinfoTrabCedido);
    procedure gerarinfoEstagiario(obj: TinfoEstagiario);
    procedure gerarInstEnsino(obj: TinstEnsino);
    procedure gerarageIntegracao(obj: TageIntegracao);
    procedure gerarsupervisorEstagio(obj: TsupervisorEstagio);
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
      FcodCateg : Integer;
      FdtInicio : TDateTime;
      FnatAtividade : tpNatAtividade;
      FinfoComplementares : TinfoComplementares;
    public
      constructor Create;
      destructor  Destroy; override;

      property codCateg : Integer read FcodCateg write FcodCateg;
      property dtInicio : TDateTime read FdtInicio write FdtInicio;
      property natAtivididade : tpNatAtividade read FnatAtividade write FnatAtividade;
      property infoComplementares : TinfoComplementares read FinfoComplementares write FinfoComplementares;
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


implementation

uses
  eSocial_NaoPeriodicos;

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
end;

destructor TinfoTSVInicio.Destroy;
begin
  FinfoComplementares.Free;
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

procedure TEvtTSVInicio.gerarageIntegracao(obj: TageIntegracao);
begin
  Gerador.wGrupo('ageIntegracao');
    Gerador.wCampo(tcStr, '', 'cnpjAgntInteg', 0,0,0, obj.cnpjAgntInteg);
    Gerador.wCampo(tcStr, '', 'nmRazao', 0,0,0, obj.nmRazao);
    Gerador.wCampo(tcStr, '', 'dscLograd', 0,0,0, obj.dscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd', 0,0,0, obj.nrLograd);
    Gerador.wCampo(tcStr, '', 'bairro', 0,0,0, obj.bairro);
    Gerador.wCampo(tcStr, '', 'cep', 0,0,0, obj.cep);
    Gerador.wCampo(tcStr, '', 'codMunic', 0,0,0, obj.codMunic);
    Gerador.wCampo(tcStr, '', 'uf', 0,0,0, eSufToStr(obj.uf));
  Gerador.wGrupo('/ageIntegracao');
end;

procedure TEvtTSVInicio.gerarCargoFuncao(obj: TcargoFuncao);
begin
  if obj.codCargo <> EmptyStr then
  begin
    Gerador.wGrupo('cargoFuncao');
      Gerador.wCampo(tcStr, '', 'codCargo', 0,0,0, obj.codCargo);
      Gerador.wCampo(tcStr, '', 'codFuncao', 0,0,0, obj.codFuncao);
    Gerador.wGrupo('/cargoFuncao');
  end;
end;

procedure TEvtTSVInicio.gerarFGTS(obj: TFGTS);
begin
  if obj.dtOpcFGTS > 0 then
  begin
    Gerador.wGrupo('fgts');
      Gerador.wCampo(tcStr, '', 'opcFGTS', 0,0,0, obj.opcFGTS);
      Gerador.wCampo(tcDat, '', 'dtOpcFGTS', 0,0,0, obj.dtOpcFGTS);
    Gerador.wGrupo('/fgts');
  end;
end;

procedure TEvtTSVInicio.gerarInfoComplementares(obj: TinfoComplementares);
begin
  if (obj.cargoFuncao.codCargo <> EmptyStr) or (obj.Remuneracao.VrSalFx > 0) or (obj.FGTS.DtOpcFGTS> 0) or (obj.infoDirSind.dtAdmOrig > 0) or
  (obj.infoTrabCedido.dtAdmCed>0) or (obj.infoEstagiario.dtPrevTerm > 0) then
  begin
    Gerador.wGrupo('infoComplementares');
      gerarCargoFuncao(obj.cargoFuncao);
      gerarRemuneracao(obj.Remuneracao);
      gerarFGTS(obj.FGTS);
      gerarinfoDirSind(obj.infoDirSind);
      gerarinfoTrabCedido(obj.infoTrabCedido);
      gerarinfoEstagiario(obj.infoEstagiario);
    Gerador.wGrupo('/infoComplementares');
  end;
end;

procedure TEvtTSVInicio.gerarinfoDirSind(obj: TinfoDirSind);
begin
  if obj.dtAdmOrig > 0 then
  begin
    Gerador.wGrupo('infoDirigenteSindical');
      Gerador.wCampo(tcStr, '', 'categOrig', 0,0,0, obj.categOrig);
      Gerador.wCampo(tcStr, '', 'cnpjOrigem', 0,0,0, obj.cnpjOrigem);
      Gerador.wCampo(tcDat, '', 'dtAdmOrig', 0,0,0, obj.dtAdmOrig);
      Gerador.wCampo(tcStr, '', 'matricOrig', 0,0,0, obj.matricOrig);
    Gerador.wGrupo('/infoDirigenteSindical');
  end;
end;

procedure TEvtTSVInicio.gerarinfoEstagiario(obj: TinfoEstagiario);
begin
  if obj.dtPrevTerm > 0 then
  begin
    Gerador.wGrupo('infoEstagiario');
      Gerador.wCampo(tcStr, '', 'natEstagio', 0,0,0, obj.natEstagio);
      Gerador.wCampo(tcStr, '', 'nivEstagio', 0,0,0, obj.nivEstagio);
      Gerador.wCampo(tcStr, '', 'areaAtuacao', 0,0,0, obj.areaAtuacao);
      Gerador.wCampo(tcStr, '', 'nrApol', 0,0,0, obj.nrApol);
      Gerador.wCampo(tcDe2, '', 'vlrBolsa', 0,0,0, obj.vlrBolsa);
      Gerador.wCampo(tcDat, '', 'dtPrevTerm', 0,0,0, obj.dtPrevTerm);
      gerarInstEnsino(obj.instEnsino);
      gerarageIntegracao(obj.ageIntegracao);
      gerarsupervisorEstagio(obj.supervisorEstagio);
    Gerador.wGrupo('/infoEstagiario');
  end;
end;

procedure TEvtTSVInicio.gerarinfoTrabCedido(obj: TinfoTrabCedido);
begin
  if obj.dtAdmCed > 0 then
  begin
    Gerador.wGrupo('infoTrabCedido');
      Gerador.wCampo(tcStr, '', 'categOrig', 0,0,0, obj.categOrig);
      Gerador.wCampo(tcStr, '', 'cnpjCednt', 0,0,0, obj.cnpjCednt);
      Gerador.wCampo(tcStr, '', 'matricCed', 0,0,0, obj.matricCed);
      Gerador.wCampo(tcDat, '', 'dtAdmCed', 0,0,0, obj.dtAdmCed);
      Gerador.wCampo(tcInt, '', 'tpRegTrab', 0,0,0, eSTpRegTrabToStr(obj.tpRegTrab));
      Gerador.wCampo(tcInt, '', 'tpRegPrev', 0,0,0, eSTpRegPrevToStr(obj.tpRegPrev));
      Gerador.wCampo(tcStr, '', 'infOnus', 0,0,0, obj.infOnus);
    Gerador.wGrupo('/infoTrabCedido');
  end;
end;

procedure TEvtTSVInicio.gerarInfoTSVInicio(obj: TinfoTSVInicio);
begin
  Gerador.wGrupo('infoTSVInicio');
    Gerador.wCampo(tcStr, '', 'codCateg', 0,0,0, obj.codCateg);
    Gerador.wCampo(tcDat, '', 'dtInicio', 0,0,0, obj.dtInicio);
    Gerador.wCampo(tcStr, '', 'natAtividade', 0,0,0, ord(obj.natAtivididade) + 1);
    gerarInfoComplementares(obj.InfoComplementares);
  Gerador.wGrupo('/infoTSVInicio');
end;

procedure TEvtTSVInicio.gerarInstEnsino(obj: TinstEnsino);
begin
  Gerador.wGrupo('instEnsino');
    Gerador.wCampo(tcStr, '', 'cnpjInstEnsino', 0,0,0, obj.cnpjInstEnsino);
    Gerador.wCampo(tcStr, '', 'nmRazao', 0,0,0, obj.nmRazao);
    Gerador.wCampo(tcStr, '', 'dscLograd', 0,0,0, obj.dscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd', 0,0,0, obj.nrLograd);
    Gerador.wCampo(tcStr, '', 'bairro', 0,0,0, obj.bairro);
    Gerador.wCampo(tcStr, '', 'cep', 0,0,0, obj.cep);
    Gerador.wCampo(tcStr, '', 'codMunic', 0,0,0, obj.codMunic);
    Gerador.wCampo(tcStr, '', 'uf', 0,0,0, eSufToStr(obj.uf));
  Gerador.wGrupo('/instEnsino');
end;

procedure TEvtTSVInicio.gerarRemuneracao(obj: TRemuneracao);
begin
  if obj.vrSalFx > 0 then
  begin
    Gerador.wGrupo('remuneracao');
      Gerador.wCampo(tcDe2, '', 'vrSalFx', 0,0,0, obj.vrSalFx);
      Gerador.wCampo(tcStr, '', 'undSalFixo', 0,0,0, obj.undSalFixo);
      Gerador.wCampo(tcStr, '', 'dscSalVar', 0,0,0, obj.dscSalVar);
    Gerador.wGrupo('/remuneracao');
  end;
end;

procedure TEvtTSVInicio.gerarsupervisorEstagio(obj: TsupervisorEstagio);
begin
  if obj.cpfSupervisor <> EmptyStr then
  begin
    Gerador.wGrupo('supervisorEstagio');
      Gerador.wCampo(tcStr, '', 'cpfSupervisor', 0,0,0, obj.cpfSupervisor);
      Gerador.wCampo(tcStr, '', 'nmSuperv', 0,0,0, obj.nmSuperv);
    Gerador.wGrupo('/supervisorEstagio');
  end;
end;

function TEvtTSVInicio.GerarXML: boolean;
begin
  try
    GerarCabecalho('evtTSVInicio');
      Gerador.wGrupo('evtTSVInicio Id="'+GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0)+'"');// versao="'+self.versao+'"
        gerarIdeEvento2(self.IdeEvento);
        gerarIdeEmpregador(self.IdeEmpregador);
        gerarTrabalhador(self.Trabalhador,'trabalhador',3);
        gerarInfoTSVInicio(self.infoTSVInicio);
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
