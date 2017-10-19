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
******************************************************************************}
{$I ACBr.inc}

unit eSocial_S2305;

interface

uses
  SysUtils, Classes,
  eSocial_Common, eSocial_Conversao,
  pcnConversao,
  ACBreSocialGerador;

type
  TS2305Collection = class;
  TS2305CollectionItem = class;
  TEvtTSVAltContr = class;
  TinfoTSVAlteracao = class;
  TinfoComplementares = class;


  TS2305Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2305CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2305CollectionItem);
  public
    function Add: TS2305CollectionItem;
    property Items[Index: Integer]: TS2305CollectionItem read GetItem write SetItem; default;
  end;

  TS2305CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtTSVAltContr: TEvtTSVAltContr;
    procedure setEvtTSVAltContr(const Value: TEvtTSVAltContr);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor  Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTSVAltContr: TEvtTSVAltContr read FEvtTSVAltContr write setEvtTSVAltContr;
  end;

  TEvtTSVAltContr = class(TeSocialEvento)
    private
      FIdeEvento: TIdeEvento2;
      FIdeEmpregador: TIdeEmpregador;
      FideTrabSemVinc: TideTrabSemVinc;
      FinfoTSVAlteracao : TinfoTSVAlteracao;

      procedure gerarideTrabSemVinc(obj : TideTrabSemVinc);
      procedure gerarInfoTSVAlteracao(obj: TinfoTSVAlteracao);
      procedure gerarinfoComplementares(obj: TinfoComplementares);
      procedure gerarinfoEstagiario(obj: TinfoEstagiario);
      procedure gerarInstEnsino(obj: TinstEnsino);
      procedure gerarageIntegracao(obj: TageIntegracao);
      procedure gerarsupervisorEstagio(obj: TsupervisorEstagio);
      procedure gerarcargoFuncao(obj: TcargoFuncao);
    public
      constructor Create(AACBreSocial: TObject);overload;
      destructor  Destroy;

      function GerarXML: boolean; override;

      property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
      property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
      property IdeTrabSemVinc: TideTrabSemVinc read FideTrabSemVinc write FideTrabSemVinc;
      property infoTSVAlteracao : TinfoTSVAlteracao read FinfoTSVAlteracao write FinfoTSVAlteracao;
  end;

  TinfoTSVAlteracao = class(TPersistent)
    private
      FdtAlteracao : TDateTime;
      FnatAtividade: tpNatAtividade;
      FinfoComplementares : TinfoComplementares;
    public
      constructor Create;
      destructor  Destroy; override;
      property dtAlteracao : TDateTime read FdtAlteracao write FdtAlteracao;
      property natAtivididade : tpNatAtividade read FnatAtividade write FnatAtividade;
      property infoComplementares : TinfoComplementares read FinfoComplementares write FinfoComplementares;
  end;

  TinfoComplementares = class(TPersistent)
    private
      FcargoFuncao : TcargoFuncao;
      FRemuneracao : TRemuneracao;
      FinfoEstagiario : TinfoEstagiario;
    public
      constructor Create;
      destructor  Destroy; override;

      property cargoFuncao: TcargoFuncao read FcargoFuncao write FcargoFuncao;
      property Remuneracao: TRemuneracao read FRemuneracao write FRemuneracao;
      property infoEstagiario: TinfoEstagiario read FinfoEstagiario write FinfoEstagiario;
  end;

implementation

uses
  eSocial_NaoPeriodicos;

{ TS2305Collection }

function TS2305Collection.Add: TS2305CollectionItem;
begin
  Result := TS2305CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2305Collection.GetItem(Index: Integer): TS2305CollectionItem;
begin
  Result := TS2305CollectionItem(inherited GetItem(Index));
end;

procedure TS2305Collection.SetItem(Index: Integer; Value: TS2305CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2305CollectionItem }

constructor TS2305CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS2305;
  FEvtTSVAltContr := TEvtTSVAltContr.Create(AOwner);
end;

destructor TS2305CollectionItem.Destroy;
begin
  FEvtTSVAltContr.Free;
  inherited;
end;

procedure TS2305CollectionItem.setEvtTSVAltContr(const Value: TEvtTSVAltContr);
begin
  FEvtTSVAltContr.Assign(Value);
end;

{ TEvtTSVAltContr }

constructor TEvtTSVAltContr.Create(AACBreSocial: TObject);
begin
  inherited;
  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FideTrabSemVinc := TideTrabSemVinc.Create;
  FinfoTSVAlteracao := TinfoTSVAlteracao.Create;
end;

destructor TEvtTSVAltContr.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrabSemVinc.Free;
  FinfoTSVAlteracao.Free;
  inherited;
end;

procedure TEvtTSVAltContr.gerarageIntegracao(obj: TageIntegracao);
begin
  Gerador.wGrupo('ageIntegracao');
    Gerador.wCampo(tcStr, '', 'cnpjAgntInteg', 0,0,0, obj.cnpjAgntInteg);
    Gerador.wCampo(tcStr, '', 'nmRazao', 0,0,0, obj.nmRazao);
    Gerador.wCampo(tcStr, '', 'dscLograd', 0,0,0, obj.dscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd', 0,0,0, obj.nrLograd);
    Gerador.wCampo(tcStr, '', 'bairro', 0,0,0, obj.bairro);
    Gerador.wCampo(tcStr, '', 'cep', 0,0,0, obj.cep);
    Gerador.wCampo(tcStr, '', 'codMunic', 0,0,0, obj.codMunic);
    Gerador.wCampo(tcStr, '', 'uf', 0,0,0, obj.uf);
  Gerador.wGrupo('/ageIntegracao');
end;

procedure TEvtTSVAltContr.gerarcargoFuncao(obj: TcargoFuncao);
begin
  Gerador.wGrupo('cargoFuncao');
    Gerador.wCampo(tcStr, '', 'codCargo', 0,0,0, obj.codCargo);
    Gerador.wCampo(tcStr, '', 'codFuncao', 0,0,0, obj.codFuncao);
  Gerador.wGrupo('/cargoFuncao');
end;

procedure TEvtTSVAltContr.gerarideTrabSemVinc(obj: TideTrabSemVinc);
begin
  Gerador.wGrupo('ideTrabSemVinculo');
    Gerador.wCampo(tcStr, '', 'cpfTrab', 0,0,0, obj.cpfTrab);
    Gerador.wCampo(tcStr, '', 'nisTrab', 0,0,0, obj.nisTrab);
    Gerador.wCampo(tcStr, '', 'codCateg', 0,0,0, obj.codCateg);
  Gerador.wGrupo('/ideTrabSemVinculo');
end;

procedure TEvtTSVAltContr.gerarinfoComplementares(obj: TinfoComplementares);
begin
  Gerador.wGrupo('infoComplementares');
    gerarcargoFuncao(obj.cargoFuncao);
    GerarRemuneracao(obj.Remuneracao);
    gerarinfoEstagiario(obj.infoEstagiario);
  Gerador.wGrupo('/infoComplementares');
end;

procedure TEvtTSVAltContr.gerarinfoEstagiario(obj: TinfoEstagiario);
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

procedure TEvtTSVAltContr.gerarInfoTSVAlteracao(obj: TinfoTSVAlteracao);
begin
  Gerador.wGrupo('infoTSVAlteracao');
    Gerador.wCampo(tcDat, '', 'dtAlteracao', 0,0,0, obj.dtAlteracao);
    Gerador.wCampo(tcStr, '', 'natAtividade', 0,0,0, obj.natAtivididade);
    gerarinfoComplementares(obj.infoComplementares);
  Gerador.wGrupo('/infoTSVAlteracao');
end;

procedure TEvtTSVAltContr.gerarInstEnsino(obj: TinstEnsino);
begin
  Gerador.wGrupo('instEnsino');
    Gerador.wCampo(tcStr, '', 'cnpjInstEnsino', 0,0,0, obj.cnpjInstEnsino);
    Gerador.wCampo(tcStr, '', 'nmRazao', 0,0,0, obj.nmRazao);
    Gerador.wCampo(tcStr, '', 'dscLograd', 0,0,0, obj.dscLograd);
    Gerador.wCampo(tcStr, '', 'nrLograd', 0,0,0, obj.nrLograd);
    Gerador.wCampo(tcStr, '', 'bairro', 0,0,0, obj.bairro);
    Gerador.wCampo(tcStr, '', 'cep', 0,0,0, obj.cep);
    Gerador.wCampo(tcStr, '', 'codMunic', 0,0,0, obj.codMunic);
    Gerador.wCampo(tcStr, '', 'uf', 0,0,0, obj.uf);
  Gerador.wGrupo('/instEnsino');
end;

procedure TEvtTSVAltContr.gerarsupervisorEstagio(obj: TsupervisorEstagio);
begin
  Gerador.wGrupo('supervisorEstagio');
    Gerador.wCampo(tcStr, '', 'cpfSupervisor', 0,0,0, obj.cpfSupervisor);
    Gerador.wCampo(tcStr, '', 'nmSuperv', 0,0,0, obj.nmSuperv);
  Gerador.wGrupo('/supervisorEstagio');
end;

function TEvtTSVAltContr.GerarXML: boolean;
begin
  try
    GerarCabecalho('');
      Gerador.wGrupo('evtTSVAlteracao Id="'+GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0)+'"');//versao="'+self.versao+'"
        gerarIdeEvento2(self.IdeEvento);
        gerarIdeEmpregador(self.IdeEmpregador);
        gerarideTrabSemVinc(self.IdeTrabSemVinc);
        gerarInfoTSVAlteracao(self.infoTSVAlteracao);
      Gerador.wGrupo('/evtTSVAlteracao');
    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTSVAlteracao');
    Validar('evtTSVAlteracao');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

{ TinfoComplementares }

constructor TinfoComplementares.Create;
begin
  inherited;
  FcargoFuncao := TcargoFuncao.Create;
  FRemuneracao := TRemuneracao.Create;
  FinfoEstagiario := TinfoEstagiario.Create;
end;

destructor TinfoComplementares.Destroy;
begin
  FcargoFuncao.Free;
  FRemuneracao.Free;
  FinfoEstagiario.Free;
  inherited;
end;

{ TinfoTSVAlteracao }

constructor TinfoTSVAlteracao.Create;
begin
  inherited;
  FinfoComplementares := TinfoComplementares.Create;
end;

destructor TinfoTSVAlteracao.Destroy;
begin
  FinfoComplementares.Free;
  inherited;
end;

end.
