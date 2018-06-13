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
|*  - Alterado alguns atributos e nome de tag
******************************************************************************}
{$I ACBr.inc}

unit pcesS2210;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2210Collection = class;
  TS2210CollectionItem = class;
  TEvtCAT = class;
  TCat = class;
  TCatOrigem = class;
  TAtestado = class;
  TAgenteCausadorColecao = class;
  TAgenteCausadorItem = class;
  TParteAtingidaColecao = class;
  TParteAtingidaItem = class;
  TLocalAcidente = class;
  TIdeRegistrador = class;

  TS2210Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2210CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2210CollectionItem);
  public
    function Add: TS2210CollectionItem;
    property Items[Index: Integer]: TS2210CollectionItem read GetItem write SetItem; default;
  end;

  TS2210CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtCAT: TEvtCAT;

    procedure setEvtCAT(const Value: TEvtCAT);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtCAT: TEvtCAT read FEvtCAT write setEvtCAT;
  end;

  TEvtCAT = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeRegistrador: TIdeRegistrador;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabalhador: TideTrabalhador2;
    FCat: TCat;
    FACBreSocial: TObject;

    procedure GerarIdeRegistrador;
    procedure GerarCAT;
    procedure GerarLocalAcidente;
    procedure GerarParteAtingida;
    procedure GerarAgenteCausador;
    procedure GerarAtestado;
    procedure GerarCatOrigem;
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeRegistrador: TIdeRegistrador read FIdeRegistrador write FIdeRegistrador;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTrabalhador: TideTrabalhador2 read FIdeTrabalhador write FIdeTrabalhador;
    property Cat: TCat read FCat write FCat;
  end;

  TIdeRegistrador = class
  private
    FtpRegistrador: tpTpRegistrador;
    FtpInsc: tpTpInsc;
    FnrInsc: string;
  public
    property tpRegistrador: tpTpRegistrador read FtpRegistrador write FtpRegistrador;
    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: string read FnrInsc write FnrInsc;
  end;

  TCat = class(TPersistent)
  private
    FdtAcid: TDateTime;
    FTpAcid: string;
    FhrAcid: string;
    FhrsTrabAntesAcid: string;
    FtpCat: tpTpCat;
    FindCatObito: tpSimNao;
    FDtObito: TDateTime;
    FindComunPolicia: tpSimNao;
    FcodSitGeradora: integer;
    FiniciatCAT: tpIniciatCAT;
    Fobservacao: string;
    FLocalAcidente: TLocalAcidente;
    FParteAtingida: TParteAtingidaColecao;
    FAgenteCausador: TAgenteCausadorColecao;
    FAtestado: TAtestado;
    FCatOrigem: TCatOrigem;
  public
    constructor create;
    destructor Destroy; override;

    property dtAcid: TDateTime read FdtAcid write FdtAcid;
    property TpAcid: string read FTpAcid write FTpAcid;
    property hrAcid: string read FhrAcid write FhrAcid;
    property hrsTrabAntesAcid: string read FhrsTrabAntesAcid write FhrsTrabAntesAcid;
    property tpCat: tpTpCat read FtpCat write FtpCat;
    property indCatObito: tpSimNao read FindCatObito write FindCatObito;
    property dtOBito: TDateTime read FDtObito write FDtObito;
    property indComunPolicia: tpSimNao read FindComunPolicia write FindComunPolicia;
    property codSitGeradora: integer read FcodSitGeradora write FcodSitGeradora;
    property iniciatCAT: tpIniciatCAT read FiniciatCAT write FiniciatCAT;
    property observacao: string read Fobservacao write Fobservacao;
    property LocalAcidente: TLocalAcidente read FLocalAcidente write FLocalAcidente;
    property ParteAtingida: TParteAtingidaColecao read FParteAtingida write FParteAtingida;
    property AgenteCausador: TAgenteCausadorColecao read FAgenteCausador write FAgenteCausador;
    property Atestado: TAtestado read FAtestado write FAtestado;
    property CatOrigem: TCatOrigem read FCatOrigem write FCatOrigem;
  end;

  TAtestado = class
  private
    FcodCNES: String;
    FdtAtendimento: TDateTime;
    FhrAtendimento: string;
    FindInternacao: tpSimNao;
    FdurTrat: integer;
    FindAfast: tpSimNao;
    FdscLesao: integer;
    FdscCompLesao: string;
    FdiagProvavel: string;
    FcodCID: string;
    Fobservacao: string;
    FEmitente: TEmitente;
  public
    constructor create;
    destructor Destroy; override;

    property codCNES: String read FcodCNES write FcodCNES;
    property dtAtendimento: TDateTime read FdtAtendimento write FdtAtendimento;
    property hrAtendimento: string read FhrAtendimento write FhrAtendimento;
    property indInternacao: tpSimNao read FindInternacao write FindInternacao;
    property durTrat: integer read FdurTrat write FdurTrat;
    property indAfast: tpSimNao read FindAfast write FindAfast;
    property dscLesao: integer read FdscLesao write FdscLesao;
    property dscCompLesao: string read FdscCompLesao write FdscCompLesao;
    property diagProvavel: string read FdiagProvavel write FdiagProvavel;
    property codCID: string read FcodCID write FcodCID;
    property observacao: string read Fobservacao write Fobservacao;
    property Emitente: TEmitente read FEmitente write FEmitente;
  end;

  TCatOrigem = class
  private
    FdtCatOrig: TDateTime;
    FnrCatOrig: string;
  public
    property dtCatOrig: TDateTime read FdtCatOrig write FdtCatOrig;
    property nrCatOrig: string read FnrCatOrig write FnrCatOrig;
  end;

  TAgenteCausadorItem = class(TCollectionItem)
  private
    FcodAgntCausador: Integer;
  published
    property codAgntCausador: Integer read FcodAgntCausador write FcodAgntCausador;
  end;

  TAgenteCausadorColecao = class(TCollection)
  private
    function GetItem(Index: Integer): TAgenteCausadorItem;
    procedure SetItem(Index: Integer; const Value: TAgenteCausadorItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TAgenteCausadorItem;
    property Items[Index: Integer]: TAgenteCausadorItem read GetItem write SetItem;
  end;

  TParteAtingidaItem = class(TCollectionItem)
  private
    FcodParteAting: Integer;
    Flateralidade: tpLateralidade;
  published
    property codParteAting: Integer read FcodParteAting write FcodParteAting;
    property lateralidade: tpLateralidade read Flateralidade write Flateralidade;
  end;

  TParteAtingidaColecao = class(TCollection)
  private
    function GetItem(Index: Integer): TParteAtingidaItem;
    procedure SetItem(Index: Integer; const Value: TParteAtingidaItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TParteAtingidaItem;
    property Items[Index: Integer]: TParteAtingidaItem read GetItem write SetItem;
  end;

  TLocalAcidente = class
  private
    FtpLocal: tpTpLocal;
    FdscLocal: string;
    FdscLograd: string;
    FnrLograd: string;
    FcodMunic: Integer;
    Fuf: tpuf;
    FcnpjLocalAcid: string;
    FPais: string;
    FCodPostal: string;
  public
    property tpLocal: tpTpLocal read FtpLocal write FtpLocal;
    property dscLocal: string read FdscLocal write FdscLocal;
    property dscLograd: string read FdscLograd write FdscLograd;
    property nrLograd: string read FnrLograd write FnrLograd;
    property codMunic: Integer read FcodMunic write FcodMunic;
    property uf: tpuf read Fuf write Fuf;
    property cnpjLocalAcid: string read FcnpjLocalAcid write FcnpjLocalAcid;
    property pais: string read FPais write FPais;
    property codPostal: string read FCodPostal write FCodPostal;
  end;

implementation

uses
  IniFiles,
  ACBreSocial, ACBrDFeUtil;

{ TS2210Collection }

function TS2210Collection.Add: TS2210CollectionItem;
begin
  Result := TS2210CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2210Collection.GetItem(Index: Integer): TS2210CollectionItem;
begin
  Result := TS2210CollectionItem(inherited GetItem(Index));
end;

procedure TS2210Collection.SetItem(Index: Integer;
  Value: TS2210CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2210CollectionItem }

constructor TS2210CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento      := teS2210;
  FEvtCAT := TEvtCAT.Create(AOwner);
end;

destructor TS2210CollectionItem.Destroy;
begin
  FEvtCAT.Free;

  inherited;
end;

procedure TS2210CollectionItem.setEvtCAT(const Value: TEvtCAT);
begin
  FEvtCAT.Assign(Value);
end;

{ TAtestado }

constructor TAtestado.create;
begin
  FEmitente := TEmitente.Create;
end;

destructor TAtestado.destroy;
begin
  FEmitente.Free;

  inherited;
end;

{ TAgenteCausadorColecao }

function TAgenteCausadorColecao.Add: TAgenteCausadorItem;
begin
  Result := TAgenteCausadorItem(inherited Add);
end;

constructor TAgenteCausadorColecao.Create(AOwner: TPersistent);
begin
  inherited Create(TAgenteCausadorItem);
end;

function TAgenteCausadorColecao.GetItem(
  Index: Integer): TAgenteCausadorItem;
begin
  Result := TAgenteCausadorItem(inherited GetItem(Index));
end;

procedure TAgenteCausadorColecao.SetItem(Index: Integer;
  const Value: TAgenteCausadorItem);
begin
  inherited SetItem(Index, Value);
end;

{ TParteAtingidaColecao }

function TParteAtingidaColecao.Add: TParteAtingidaItem;
begin
  Result := TParteAtingidaItem(inherited Add);
end;

constructor TParteAtingidaColecao.Create(AOwner: TPersistent);
begin
  inherited Create(TParteAtingidaItem);
end;

function TParteAtingidaColecao.GetItem(Index: Integer): TParteAtingidaItem;
begin
  Result := TParteAtingidaItem(inherited GetItem(Index));
end;

procedure TParteAtingidaColecao.SetItem(Index: Integer;
  const Value: TParteAtingidaItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCat }

constructor TCat.create;
begin
  inherited;

  FLocalAcidente := TLocalAcidente.Create;
  FParteAtingida := TParteAtingidaColecao.Create(self);
  FAgenteCausador := TAgenteCausadorColecao.Create(self);
  FAtestado := TAtestado.Create;
  FCatOrigem := TCatOrigem.Create;
end;

destructor TCat.destroy;
begin
  FLocalAcidente.Free;
  FParteAtingida.Free;
  FAgenteCausador.Free;
  FAtestado.Free;
  FCatOrigem.Free;

  inherited;
end;

{ TEvtCAT }

constructor TEvtCAT.Create(AACBreSocial: TObject);
begin
  inherited;

  FACBreSocial := AACBreSocial;
  FIdeEvento := TIdeEvento2.Create;
  FIdeRegistrador := TIdeRegistrador.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeTrabalhador := TideTrabalhador2.Create;
  FCat := TCat.Create;
end;

destructor TEvtCAT.destroy;
begin
  FIdeEvento.Free;
  FIdeRegistrador.Free;
  FIdeEmpregador.Free;
  FIdeTrabalhador.Free;
  FCat.Free;

  inherited;
end;

procedure TEvtCAT.GerarAgenteCausador;
var
  i: integer;
begin
  for i:= 0 to self.Cat.AgenteCausador.Count-1 do
  begin
    Gerador.wGrupo('agenteCausador');

    Gerador.wCampo(tcStr, '', 'codAgntCausador', 1, 9, 1, self.Cat.AgenteCausador.Items[i].codAgntCausador);

    Gerador.wGrupo('/agenteCausador');
  end;

  if self.Cat.AgenteCausador.Count > 99 then
    Gerador.wAlerta('', 'agenteCausador', 'Lista de Agentes Causadores', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtCAT.GerarAtestado;
begin
  if self.Cat.Atestado.dtAtendimento > 0 then
  begin
    Gerador.wGrupo('atestado');

    Gerador.wCampo(tcStr, '', 'codCNES',        1,   7, 0, self.Cat.Atestado.codCNES);
    Gerador.wCampo(tcDat, '', 'dtAtendimento', 10,  10, 1, self.Cat.Atestado.dtAtendimento);
    Gerador.wCampo(tcStr, '', 'hrAtendimento',  4,   4, 1, self.Cat.Atestado.hrAtendimento);
    Gerador.wCampo(tcStr, '', 'indInternacao',  1,   1, 1, eSSimNaoToStr(self.Cat.Atestado.indInternacao));
    Gerador.wCampo(tcStr, '', 'durTrat',        1,   4, 1, self.Cat.Atestado.durTrat);
    Gerador.wCampo(tcStr, '', 'indAfast',       1,   1, 1, eSSimNaoToStr(self.Cat.Atestado.indAfast));
    Gerador.wCampo(tcStr, '', 'dscLesao',       1,   9, 0, self.Cat.Atestado.dscLesao);
    Gerador.wCampo(tcStr, '', 'dscCompLesao',   1, 200, 0, self.Cat.Atestado.dscCompLesao);
    Gerador.wCampo(tcStr, '', 'diagProvavel',   1, 100, 0, self.Cat.Atestado.diagProvavel);
    Gerador.wCampo(tcStr, '', 'codCID',         1,   4, 1, self.Cat.Atestado.codCID);
    Gerador.wCampo(tcStr, '', 'observacao',     1, 255, 0, self.Cat.Atestado.observacao);

    GerarEmitente(self.Cat.Atestado.Emitente);

    Gerador.wGrupo('/atestado');
  end;
end;

procedure TEvtCAT.GerarCAT;
begin
  Gerador.wGrupo('cat');

  Gerador.wCampo(tcDat, '', 'dtAcid',           10,  10, 1, self.Cat.dtAcid);
  Gerador.wCampo(tcStr, '', 'tpAcid',            1,   6, 1, self.Cat.tpAcid);
  Gerador.wCampo(tcStr, '', 'hrAcid',            4,   4, 1, self.Cat.hrAcid);
  Gerador.wCampo(tcStr, '', 'hrsTrabAntesAcid',  4,   4, 1, self.Cat.hrsTrabAntesAcid);
  Gerador.wCampo(tcStr, '', 'tpCat',             1,   1, 1, eSTpCatToStr(self.Cat.tpCat));
  Gerador.wCampo(tcStr, '', 'indCatObito',       1,   1, 1, eSSimNaoToStr(self.Cat.indCatObito));
  Gerador.wCampo(tcDat, '', 'dtObito',          10,  10, 0, self.Cat.dtOBito);
  Gerador.wCampo(tcStr, '', 'indComunPolicia',   1,   1, 1, eSSimNaoToStr(self.Cat.indComunPolicia));
  Gerador.wCampo(tcStr, '', 'codSitGeradora',    1,   9, 0, self.Cat.codSitGeradora);
  Gerador.wCampo(tcStr, '', 'iniciatCAT',        1,   1, 1, eSIniciatCATToStr(self.Cat.iniciatCAT));
  Gerador.wCampo(tcStr, '', 'observacao',        1, 255, 0, self.Cat.observacao);

  GerarLocalAcidente;
  GerarParteAtingida;
  GerarAgenteCausador;
  GerarAtestado;
  GerarCatOrigem;

  Gerador.wGrupo('/cat');
end;

procedure TEvtCAT.GerarCatOrigem;
begin
  if self.Cat.CatOrigem.dtCatOrig > 0 then
  begin
    Gerador.wGrupo('catOrigem');

    Gerador.wCampo(tcDat, '', 'dtCatOrig', 10, 10, 1, self.Cat.CatOrigem.dtCatOrig);
    Gerador.wCampo(tcStr, '', 'nrCatOrig',  1, 40, 0, self.Cat.CatOrigem.nrCatOrig);

    Gerador.wGrupo('/catOrigem');
  end;
end;

procedure TEvtCAT.GerarIdeRegistrador;
begin
  Gerador.wGrupo('ideRegistrador');

  Gerador.wCampo(tcStr, '', 'tpRegistrador', 1,  2, 1, eSTpRegistradorToStr(self.ideRegistrador.tpRegistrador));
  Gerador.wCampo(tcStr, '', 'tpInsc',        1,  1, 1, eSTpInscricaoToStr(self.ideRegistrador.tpInsc));
  Gerador.wCampo(tcStr, '', 'nrInsc',        1, 15, 0, self.ideRegistrador.nrInsc);

  Gerador.wGrupo('/ideRegistrador');
end;

procedure TEvtCAT.GerarLocalAcidente;
begin
  Gerador.wGrupo('localAcidente');

  Gerador.wCampo(tcStr, '', 'tpLocal',        1,  1, 1, eSTpLocalToStr(self.Cat.LocalAcidente.tpLocal));
  Gerador.wCampo(tcStr, '', 'dscLocal',       1, 80, 0, self.Cat.LocalAcidente.dscLocal);
  Gerador.wCampo(tcStr, '', 'dscLograd',      1, 80, 0, self.Cat.LocalAcidente.dscLograd);
  Gerador.wCampo(tcStr, '', 'nrLograd',       1, 10, 0, self.Cat.LocalAcidente.nrLograd);
  Gerador.wCampo(tcStr, '', 'codMunic',       7,  7, 0, self.Cat.LocalAcidente.codMunic);
  Gerador.wCampo(tcStr, '', 'uf',             2,  2, 0, eSufToStr(self.Cat.LocalAcidente.uf));
  Gerador.wCampo(tcStr, '', 'cnpjLocalAcid', 14, 14, 0, self.Cat.LocalAcidente.cnpjLocalAcid);
  Gerador.wCampo(tcStr, '', 'pais',           1,  3, 0, self.Cat.LocalAcidente.pais);
  Gerador.wCampo(tcStr, '', 'codPostal',      1, 12, 0, self.Cat.LocalAcidente.codPostal);

  Gerador.wGrupo('/localAcidente');
end;

procedure TEvtCAT.GerarParteAtingida;
var
  i: integer;
begin
  for i:= 0 to self.Cat.ParteAtingida.Count-1 do
  begin
    Gerador.wGrupo('parteAtingida');

    Gerador.wCampo(tcStr, '', 'codParteAting', 1, 9, 1, self.Cat.ParteAtingida.Items[i].codParteAting);
    Gerador.wCampo(tcStr, '', 'lateralidade',  1, 1, 1, eSLateralidadeToStr(self.Cat.ParteAtingida.Items[i].lateralidade));

    Gerador.wGrupo('/parteAtingida');
  end;

  if self.Cat.ParteAtingida.Count > 99 then
    Gerador.wAlerta('', 'parteAtingida', 'Lista de Partes Atingidas', ERR_MSG_MAIOR_MAXIMO + '99');
end;

function TEvtCAT.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtCAT');
    Gerador.wGrupo('evtCAT Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeRegistrador;
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeTrabalhador2(self.IdeTrabalhador, True);
    GerarCAT;

    Gerador.wGrupo('/evtCAT');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtCAT');

    Validar(schevtCAT);
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtCAT.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I: Integer;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtCAT';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.TpAmb       := eSStrTotpAmb(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideRegistrador';
      ideRegistrador.tpRegistrador := eSStrToTpRegistrador(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideRegistrador.TpInsc        := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideRegistrador.NrInsc        := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideTrabalhador';
      ideTrabalhador.CpfTrab    := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      ideTrabalhador.NisTrab    := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);

      sSecao := 'cat';
      cat.dtAcid           := StringToDateTime(INIRec.ReadString(sSecao, 'dtAcid', '0'));
      cat.TpAcid           := INIRec.ReadString(sSecao, 'tpAcid', EmptyStr);
      cat.hrAcid           := INIRec.ReadString(sSecao, 'hrAcid', EmptyStr);
      cat.hrsTrabAntesAcid := INIRec.ReadString(sSecao, 'hrsTrabAntesAcid', EmptyStr);
      cat.tpCat            := eSStrToTpCat(Ok, INIRec.ReadString(sSecao, 'tpCat', '1'));
      cat.indCatObito      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indCatObito', 'S'));
      cat.dtOBito          := StringToDateTime(INIRec.ReadString(sSecao, 'dtObito', '0'));
      cat.indComunPolicia  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indComunPolicia', 'S'));
      cat.codSitGeradora   := INIRec.ReadInteger(sSecao, 'codSitGeradora', 0);
      cat.iniciatCAT       := eSStrToIniciatCAT(Ok, INIRec.ReadString(sSecao, 'iniciatCAT', '1'));
      cat.observacao       := INIRec.ReadString(sSecao, 'observacao', EmptyStr);

      sSecao := 'localAcidente';
      cat.localAcidente.tpLocal       := eSStrToTpLocal(Ok, INIRec.ReadString(sSecao, 'tpLocal', '1'));
      cat.localAcidente.dscLocal      := INIRec.ReadString(sSecao, 'dscLocal', EmptyStr);
      cat.localAcidente.dscLograd     := INIRec.ReadString(sSecao, 'dscLograd', EmptyStr);
      cat.localAcidente.nrLograd      := INIRec.ReadString(sSecao, 'nrLograd', EmptyStr);
      cat.localAcidente.codMunic      := INIRec.ReadInteger(sSecao, 'codMunic', 0);
      cat.localAcidente.uf            := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'uf', 'SP'));
      cat.localAcidente.cnpjLocalAcid := INIRec.ReadString(sSecao, 'cnpjLocalAcid', EmptyStr);
      cat.localAcidente.pais          := INIRec.ReadString(sSecao, 'pais', EmptyStr);
      cat.localAcidente.codPostal     := INIRec.ReadString(sSecao, 'codPostal', EmptyStr);

      I := 1;
      while true do
      begin
        // de 01 até 99
        sSecao := 'parteAtingida' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'codParteAting', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with cat.parteAtingida.Add do
        begin
          codParteAting := StrToInt(sFim);
          lateralidade  := eSStrToLateralidade(Ok, INIRec.ReadString(sSecao, 'lateralidade', '1'));
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        // de 01 até 99
        sSecao := 'agenteCausador' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'codAgntCausador', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with cat.agenteCausador.Add do
        begin
          codAgntCausador := StrToInt(sFim);
        end;

        Inc(I);
      end;

      sSecao := 'atestado';
      if INIRec.ReadString(sSecao, 'dtAtendimento', '') <> '' then
      begin
        cat.atestado.codCNES       := INIRec.ReadString(sSecao, 'codCNES', EmptyStr);
        cat.atestado.dtAtendimento := StringToDateTime(INIRec.ReadString(sSecao, 'dtAtendimento', '0'));
        cat.atestado.hrAtendimento := INIRec.ReadString(sSecao, 'hrAtendimento', EmptyStr);
        cat.atestado.indInternacao := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indInternacao', 'S'));
        cat.atestado.durTrat       := INIRec.ReadInteger(sSecao, 'durTrat', 0);
        cat.atestado.indAfast      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indAfast', 'S'));
        cat.atestado.dscLesao      := INIRec.ReadInteger(sSecao, 'dscLesao', 0);
        cat.atestado.dscCompLesao  := INIRec.ReadString(sSecao, 'dscCompLesao', EmptyStr);
        cat.atestado.diagProvavel  := INIRec.ReadString(sSecao, 'diagProvavel', EmptyStr);
        cat.atestado.codCID        := INIRec.ReadString(sSecao, 'codCID', EmptyStr);
        cat.atestado.observacao    := INIRec.ReadString(sSecao, 'observacao', EmptyStr);

        sSecao := 'emitente';
        cat.atestado.Emitente.nmEmit := INIRec.ReadString(sSecao, 'nmEmit', EmptyStr);
        cat.atestado.Emitente.ideOC  := eSStrToIdeOC(Ok, INIRec.ReadString(sSecao, 'ideOC', '1'));
        cat.atestado.Emitente.nrOc   := INIRec.ReadString(sSecao, 'nrOc', EmptyStr);
        cat.atestado.Emitente.ufOC   := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'ufOC', 'SP'));
      end;

      sSecao := 'catOrigem';
      if INIRec.ReadString(sSecao, 'dtCatOrig', '') <> '' then
      begin
        cat.catOrigem.dtCatOrig := StringToDateTime(INIRec.ReadString(sSecao, 'dtCatOrig', '0'));
        cat.catOrigem.nrCatOrig := INIRec.ReadString(sSecao, 'nrCatOrig', '');
      end;
    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

end.
