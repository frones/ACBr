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

unit pcesRetornoClass;

interface

uses
  SysUtils, Classes,
  pcnAuxiliar, pcnConversao, pcnLeitor,
  pcesCommon, pcesConversaoeSocial;

type
  TOcorrenciasCollection = class;
  TOcorrenciasCollectionItem = class;
  TStatus = class;
  TOcorrenciasProcCollection = class;
  TOcorrenciasProcCollectionItem = class;
  TProcessamento = class;
  TTrabalhadorConsulta = class;
  TInfoDeficienciaConsulta = class;
  TVinculoConsulta = class;
  TInfoCeletistaConsulta = class;
  TInfoEstatutarioConsulta = class;
  TCargo = class;
  TFuncao = class;
  TinfoContratoConsulta = class;
  TLocalTrabGeralConsulta = class;
  THorarioConsultaCollectionItem = class;
  THorarioConsultaCollection = class;
  THorContratualConsulta = class;
  TContrato = class;
  TRecibo = class;

  TOcorrenciasCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TOcorrenciasCollectionItem;
    procedure SetItem(Index: Integer; Value: TOcorrenciasCollectionItem);
  public
    constructor create(AOwner: TStatus);

    function Add: TOcorrenciasCollectionItem;
    property Items[Index: Integer]: TOcorrenciasCollectionItem read GetItem write SetItem;
  end;

  TOcorrenciasCollectionItem = class(TCollectionItem)
  private
    FCodigo: Integer;
    FDescricao: String;
    FTipo: Byte;
    FLocalizacao: String;
  public
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
    property Tipo: Byte read FTipo write FTipo;
    property Localizacao: String read FLocalizacao write FLocalizacao;
  end;

  TStatus = class
  private
    FcdResposta: Integer;
    FdescResposta: string;
    FtempoEstimadoConclusao: Integer;
    FOcorrencias: TOcorrenciasCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property cdResposta: Integer read FcdResposta write FcdResposta;
    property descResposta: string read FdescResposta write FdescResposta;
    property tempoEstimadoConclusao: Integer read FtempoEstimadoConclusao write FtempoEstimadoConclusao;
    property Ocorrencias: TOcorrenciasCollection read FOcorrencias write FOcorrencias;
  end;

  TDadosRecepcaoLote = class
  private
    FdhRecepcao: TDateTime;
    FversaoAplicRecepcao: String;
    FProtocolo: String;
  public
    property dhRecepcao: TDateTime read FdhRecepcao write FdhRecepcao;
    property versaoAplicRecepcao: String read FversaoAplicRecepcao write FversaoAplicRecepcao;
    property Protocolo: String read FProtocolo write FProtocolo;
  end;

  TdadosProcLote = class
  private
    FversaoAplicProcLote: String;
  public
    property versaoAplicProcLote: String read FversaoAplicProcLote write FversaoAplicProcLote;
  end;

  TRecepcao = class
  private
    FtpAmb: TptpAmb;
    FdhRecepcao: TDateTime;
    FversaoAplicRecepcao: String;
    FProtocolo: String;
  public
    property tpAmb: TptpAmb read FtpAmb write FtpAmb;
    property dhRecepcao: TDateTime read FdhRecepcao write FdhRecepcao;
    property versaoAplicRecepcao: String read FversaoAplicRecepcao write FversaoAplicRecepcao;
    property Protocolo: String read FProtocolo write FProtocolo;
  end;

  TOcorrenciasProcCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TOcorrenciasProcCollectionItem;
    procedure SetItem(Index: Integer; Value: TOcorrenciasProcCollectionItem);
  public
    constructor create(AOwner: TProcessamento);

    function Add: TOcorrenciasProcCollectionItem;
    property Items[Index: Integer]: TOcorrenciasProcCollectionItem read GetItem write SetItem;
  end;

  TOcorrenciasProcCollectionItem = class(TCollectionItem)
  private
    FCodigo: Integer;
    FDescricao: String;
    FTipo: Byte;
    FLocalizacao: String;
  public
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
    property Tipo: Byte read FTipo write FTipo;
    property Localizacao: String read FLocalizacao write FLocalizacao;
  end;

  TProcessamento = class
  private
    FcdResposta: Integer;
    FdescResposta: string;
    FversaoAplicProcLote: string;
    FdhProcessamento: TDateTime;
    FOcorrencias: TOcorrenciasProcCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property cdResposta: Integer read FcdResposta write FcdResposta;
    property descResposta: string read FdescResposta write FdescResposta;
    property versaoAplicProcLote: string read FversaoAplicProcLote write FversaoAplicProcLote;
    property dhProcessamento: TDateTime read FdhProcessamento write FdhProcessamento;
    property Ocorrencias: TOcorrenciasProcCollection read FOcorrencias write FOcorrencias;
  end;

  TTrabalhadorConsulta = class
  private
    FCpfTrab: string;
    FNisTrab: string;
    FNmTrab: string;
  public
    property CpfTrab: string read FCpfTrab write FCpfTrab;
    property NisTrab: string read FNisTrab write FNisTrab;
    property NmTrab: string read FNmTrab write FNmTrab;
  end;

  TInfoDeficienciaConsulta = class
  private
    FinfoCota: string;
  public
    property InfoCota: string read FinfoCota write FinfoCota;
  end;

  TVinculoConsulta = class
  private
    FMatricula: string;
  public
    property Matricula: string read FMatricula write FMatricula;
  end;

  TInfoCeletistaConsulta = class
  private
    FDtAdm: TDateTime;
    FTpRegJor: tpTpRegJor;
    FdtBase: Integer;
    FcnpjSindCategProf: string;
  public
    property DtAdm: TDateTime read FDtAdm write FDtAdm;
    property TpRegJor: tpTpRegJor read FTpRegJor write FTpRegJor;
    property dtBase: Integer read FdtBase write FdtBase;
    property  cnpjSindCategProf: string read FcnpjSindCategProf write FcnpjSindCategProf;
  end;

  TInfoEstatutarioConsulta = class
  private
    FDtPosse: TDateTime;
    FDtExercicio: TDateTime;
  public
    property DtPosse: TDateTime read FDtPosse write FDtPosse;
    property DtExercicio: TDateTime read FDtExercicio write FDtExercicio;
  end;

  TCargo = class
  private
    FcodCargo: string;
    FnmCargo: string;
    FcodCBO: string;
  public
    property codCargo: string read FcodCargo write FcodCargo;
    property nmCargo: string read FnmCargo write FnmCargo;
    property codCBO: string read FcodCBO write FcodCBO;
  end;

  TFuncao = class
  private
    FcodFuncao: string;
    FnmFuncao: string;
    FcodCBO: string;
  public
    property codFuncao: string read FcodFuncao write FcodFuncao;
    property nmFuncao: string read FnmFuncao write FnmFuncao;
    property codCBO: string read FcodCBO write FcodCBO;
  end;

  TinfoContratoConsulta = class
  private
    FCargo: TCargo;
    FFuncao: TFuncao;
    FcodCateg: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Cargo: TCargo read FCargo write FCargo;
    property Funcao: TFuncao read FFuncao write FFuncao;
    property codCateg: string read FcodCateg write FcodCateg;
  end;

  TLocalTrabGeralConsulta = class
  private
    FTpInsc: tpTpInsc;
    FNrInsc: string;
    FCnae: string;
  public
    property TpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property NrInsc: string read FNrInsc write FNrInsc;
    property Cnae: string read FCnae write FCnae;
  end;

  THorarioConsultaCollectionItem = class(TCollectionItem)
  private
    Fdia: tpTpDia;
    FcodHorContrat: string;
    FhrEntr: string;
    FhrSaida: string;
    FdurJornada: integer;
    FperHorFlexivel: string;
    FhorarioIntervalo: THorarioIntervaloCollection;
  public
    constructor create; reintroduce;
    destructor Destroy; override;

    property dia: tpTpDia read Fdia write Fdia;
    property codHorContrat: string read FcodHorContrat write FcodHorContrat;
    property hrEntr: string read FhrEntr write FhrEntr;
    property hrSaida: string read FhrSaida write FhrSaida;
    property durJornada: integer read FdurJornada write FdurJornada;
    property perHorFlexivel: string read FperHorFlexivel write FperHorFlexivel;
    property horarioIntervalo: THorarioIntervaloCollection read FhorarioIntervalo;
  end;

  THorarioConsultaCollection = class(TCollection)
  private
    function GetItem(Index: Integer): THorarioConsultaCollectionItem;
    procedure SetItem(Index: Integer; Value: THorarioConsultaCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: THorarioConsultaCollectionItem;
    property Items[Index: Integer]: THorarioConsultaCollectionItem read GetItem write SetItem; default;
  end;

  THorContratualConsulta = class(TPersistent)
  private
    FQtdHrsSem: integer;
    FTpJornada: tpTpJornada;
    FDscTpJorn: string;
    FTMPParc: tpTmpParc;
    FHorario: THorarioConsultaCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property QtdHrsSem: integer read FQtdHrsSem write FQtdHrsSem;
    property TpJornada: tpTpJornada read FTpJornada write FTpJornada;
    property DscTpJorn: string read FDscTpJorn write FDscTpJorn;
    property tmpParc: tpTmpParc read FTMPParc write FTMPParc;
    property horario: THorarioConsultaCollection read FHorario write FHorario;
  end;

  TContrato = class
  private
    FideEmpregador: TInscricao;
    Ftrabalhador: TTrabalhadorConsulta;
    FInfoDeficiencia: TInfoDeficienciaConsulta;
    Fvinculo: TVinculoConsulta;
    FInfoCeletista: TInfoCeletistaConsulta;
    FInfoEstatutario: TInfoEstatutarioConsulta;
    FInfoContrato: TinfoContratoConsulta;
    FRemuneracao: TRemuneracao;
    FDuracao: TDuracao;
    FLocalTrabGeralConsulta: TLocalTrabGeralConsulta;
    FHorContratual: THorContratualConsulta;
  public
    constructor Create;
    destructor Destroy; override;

    property IdeEmpregador: TInscricao read FideEmpregador write FideEmpregador;
    property Trabalhador: TTrabalhadorConsulta read Ftrabalhador write Ftrabalhador;
    property InfoDeficiencia: TInfoDeficienciaConsulta read FInfoDeficiencia write FInfoDeficiencia;
    property Vinculo: TVinculoConsulta read Fvinculo write Fvinculo;
    property InfoCeletista: TInfoCeletistaConsulta read FInfoCeletista write FInfoCeletista;
    property InfoEstatutario: TInfoEstatutarioConsulta read FInfoEstatutario write FInfoEstatutario;
    property InfoContrato: TinfoContratoConsulta read FInfoContrato write FInfoContrato;
    property Remuneracao: TRemuneracao read FRemuneracao write FRemuneracao;
    property Duracao: TDuracao read FDuracao write FDuracao;
    property LocalTrabGeral: TLocalTrabGeralConsulta read FLocalTrabGeralConsulta write FLocalTrabGeralConsulta;
    property HorContratual: THorContratualConsulta read FHorContratual write FHorContratual;
  end;

  TRecibo = class
  private
    FnrRecibo: String;
    FHash: String;
    FContrato: TContrato;
  public
    constructor Create;
    destructor Destroy; override;

    property nrRecibo: string read FnrRecibo write FnrRecibo;
    property Hash: string read FHash write FHash;
    property Contrato: TContrato read FContrato write FContrato;
  end;

  //////////////////////// Classes a serem checadas

  TStatusRetorno = class
  private
    FcdResposta: Integer;
    FdescResposta: String;
  public
    property cdResposta: Integer read FcdResposta;
    property descResposta: String read FdescResposta;
  end;

  TStatusEnvLote = class(TStatusRetorno);

  TStatusProcLote = class(TStatusRetorno)
  private
    FTmpConclusao: Integer;
  public
    property TmpConclusao: Integer read FTmpConclusao;
  end;

////////////////////////////////////////////////////////////////////////////////

implementation

{ TOcorrenciasCollection }

function TOcorrenciasCollection.Add: TOcorrenciasCollectionItem;
begin
  Result := TOcorrenciasCollectionItem(inherited Add());
end;

constructor TOcorrenciasCollection.create(AOwner: TStatus);
begin
  inherited create(TOcorrenciasCollectionItem);
end;

function TOcorrenciasCollection.GetItem(
  Index: Integer): TOcorrenciasCollectionItem;
begin
  Result := TOcorrenciasCollectionItem(Inherited GetItem(Index));
end;

procedure TOcorrenciasCollection.SetItem(Index: Integer;
  Value: TOcorrenciasCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

{ TStatus }

constructor TStatus.Create;
begin
  FOcorrencias := TOcorrenciasCollection.create(Self);
end;

destructor TStatus.Destroy;
begin
  FOcorrencias.Free;

  inherited;
end;

{ TOcorrenciasProcCollection }

function TOcorrenciasProcCollection.Add: TOcorrenciasProcCollectionItem;
begin
  Result := TOcorrenciasProcCollectionItem(inherited Add());
end;

constructor TOcorrenciasProcCollection.create(AOwner: TProcessamento);
begin
  inherited create(TOcorrenciasProcCollectionItem);
end;

function TOcorrenciasProcCollection.GetItem(
  Index: Integer): TOcorrenciasProcCollectionItem;
begin
  Result := TOcorrenciasProcCollectionItem(Inherited GetItem(Index));
end;

procedure TOcorrenciasProcCollection.SetItem(Index: Integer;
  Value: TOcorrenciasProcCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

{ TProcessamento }

constructor TProcessamento.Create;
begin
  FOcorrencias := TOcorrenciasProcCollection.create(Self);
end;

destructor TProcessamento.Destroy;
begin
  FOcorrencias.Free;
  
  inherited;
end;

{ TinfoContratoConsulta }
constructor TinfoContratoConsulta.Create;
begin
  inherited;

  FCargo    := TCargo.Create;
  FFuncao   := TFuncao.Create;
  FcodCateg := '';
end;

destructor TinfoContratoConsulta.Destroy;
begin
  FCargo.Free;
  FFuncao.Free;

  inherited;
end;

{ THorarioConsultaCollectionItem }

constructor THorarioConsultaCollectionItem.create;
begin
//  inherited;

  FhorarioIntervalo := THorarioIntervaloCollection.Create;
end;

destructor THorarioConsultaCollectionItem.destroy;
begin
  FreeAndNil(FhorarioIntervalo);

  inherited;
end;

{ THorarioConsultaCollection }
function THorarioConsultaCollection.Add: THorarioConsultaCollectionItem;
begin
  Result := THorarioConsultaCollectionItem(inherited Add);
  Result.Create;
end;

constructor THorarioConsultaCollection.Create(AOwner: TPersistent);
begin
  inherited Create(THorarioConsultaCollectionItem);
end;

function THorarioConsultaCollection.GetItem(Index: Integer): THorarioConsultaCollectionItem;
begin
  Result := THorarioConsultaCollectionItem(inherited GetItem(Index));
end;

procedure THorarioConsultaCollection.SetItem(Index: Integer;
  Value: THorarioConsultaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ THorContratualConsulta }
constructor THorContratualConsulta.Create;
begin
  inherited;

  FHorario := THorarioConsultaCollection.Create(self);
end;

destructor THorContratualConsulta.Destroy;
begin
  FHorario.Free;

  inherited;
end;

{ TContrato }
constructor TContrato.Create;
begin
  inherited;

  FideEmpregador := TInscricao.Create;
  Ftrabalhador := TTrabalhadorConsulta.Create;
  FInfoDeficiencia := TInfoDeficienciaConsulta.Create;
  Fvinculo := TVinculoConsulta.Create;
  FInfoCeletista := TInfoCeletistaConsulta.Create;
  FInfoEstatutario := TInfoEstatutarioConsulta.Create;
  FInfoContrato := TinfoContratoConsulta.Create;
  FRemuneracao := TRemuneracao.Create;
  FDuracao := TDuracao.Create;
  FLocalTrabGeralConsulta := TLocalTrabGeralConsulta.Create;
  FHorContratual := THorContratualConsulta.Create;
end;

destructor TContrato.Destroy;
begin
  FideEmpregador.Free;
  Ftrabalhador.Free;
  FInfoDeficiencia.Free;
  Fvinculo.Free;
  FInfoCeletista.Free;
  FInfoEstatutario.Free;
  FInfoContrato.Free;
  FRemuneracao.Free;
  FDuracao.Free;
  FLocalTrabGeralConsulta.Free;;
  FHorContratual.Free;

  inherited;
end;

{ TRecibo }
constructor TRecibo.Create;
begin
 inherited;

 FContrato := TContrato.Create;
end;

destructor TRecibo.Destroy;
begin
  FContrato.Free;

  inherited;
end;

end.

