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

unit pcesS2240;

interface

uses
  SysUtils, Classes, Contnrs,
  pcnConversao, pcnGerador, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2240Collection = class;
  TS2240CollectionItem = class;
  TEvtExpRisco = class;
  TinfoExpRisco = class;
  TInfoAmbCollection = class;
  TInfoAmbCollectionItem = class;
  TInfoAtiv = class;
  TAtivPericInsalCollection = class;
  TAtivPericInsalCollectionItem = class;
  TFatRiscoCollection = class;
  TFatRiscoCollectionItem = class;
  TEpcEpi = class;
  TEpiCollection = class;
  TEpiCollectionItem = class;
  TRespRegCollection = class;
  TRespRegCollectionItem = class;
  TObs = class;

  TS2240Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2240CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2240CollectionItem);
  public
    function Add: TS2240CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2240CollectionItem;
    property Items[Index: Integer]: TS2240CollectionItem read GetItem write SetItem; default;
  end;

  TS2240CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtExpRisco: TEvtExpRisco;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtExpRisco: TEvtExpRisco read FEvtExpRisco write FEvtExpRisco;
  end;

  TEvtExpRisco = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FinfoExpRisco: TinfoExpRisco;

    { Geradores da classe }
    procedure GerarInfoExpRisco(objInfoExpRisco: TInfoExpRisco);
    procedure GerarInfoAmb(objInfoAmb: TinfoAmbCollection);
    procedure GerarInfoAtiv(objInfoAtiv: TInfoAtiv);
    procedure GerarFatRisco(objFatRisco: TFatRiscoCollection);
    procedure GerarEpcEpi(pEpcEpi: TEpcEpi);
    procedure GerarEPI(objEPI: TEpiCollection);
    procedure GerarRespReg(pRespReg: TRespRegCollection);
    procedure GerarObs(pObs: TObs);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property infoExpRisco: TinfoExpRisco read FinfoExpRisco write FinfoExpRisco;
  end;

  TRespRegCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRespRegCollectionItem;
    procedure SetItem(Index: Integer; Value: TRespRegCollectionItem);
  public
    function Add: TRespRegCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRespRegCollectionItem;
    property Items[Index: Integer]: TRespRegCollectionItem read GetItem write SetItem; default;
  end;

  TRespRegCollectionItem = class(TObject)
  private
    FcpfResp: String;
    FNisResp: String;
    FNmResp: String;
    FIdeOC: tpIdeOC;
    FdscOC: String;
    FNrOc: String;
    FUfOC: tpUf;
  public
    property cpfResp: string read FcpfResp write FcpfResp;
    property nisResp: string read FNisResp write FNisResp;
    property nmResp: string read FNmResp write FNmResp;
    property ideOC: tpIdeOC read FIdeOC write FIdeOC;
    property dscOC: string read FdscOC write FdscOC;
    property nrOC: string read FNrOc write FNrOc;
    property ufOC: tpuf read FUfOC write FUfOC;
  end;

  TinfoExpRisco = class(TObject)
  private
    FdtIniCondicao: TDateTime;
    FInfoAmb: TInfoAmbCollection;
    FInfoAtiv: TInfoAtiv;
    FFatRisco: TFatRiscoCollection;
    FRespReg: TRespRegCollection;
    FObs: TObs;
    function getRespReg: TRespRegCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property dtIniCondicao: TDateTime read FdtIniCondicao write FdtIniCondicao;
    property InfoAmb: TInfoAmbCollection read FInfoAmb write FInfoAmb;
    property infoAtiv: TInfoAtiv read FInfoAtiv write FInfoAtiv;
    property fatRisco: TFatRiscoCollection read FFatRisco write FFatRisco;
    property respReg: TRespRegCollection read getRespReg write FRespReg;
    property obs: TObs read FObs write FObs;
  end;

  TInfoAmbCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TInfoAmbCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoAmbCollectionItem);
  public
    function Add: TInfoAmbCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoAmbCollectionItem;
    property Items[Index: Integer]: TInfoAmbCollectionItem read GetItem write SetItem;
  end;

  TInfoAmbCollectionItem = class(TObject)
  private
    FcodAmb: String;
  public
    property codAmb: String read FcodAmb write FcodAmb;
  end;

  TInfoAtiv = class(TObject)
  private
    FdscAtivDes: String;
    FativPericInsal: TAtivPericInsalCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    property dscAtivDes: String read FdscAtivDes write FdscAtivDes;
    property ativPericInsal: TAtivPericInsalCollection read FativPericInsal write FativPericInsal;
  end;

  TAtivPericInsalCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TAtivPericInsalCollectionItem;
    procedure SetItem(Index: Integer; Value: TAtivPericInsalCollectionItem);
  public
    function Add: TAtivPericInsalCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TAtivPericInsalCollectionItem;
    property Items[Index: Integer]: TAtivPericInsalCollectionItem read GetItem write SetItem;
  end;

  TAtivPericInsalCollectionItem = class(TObject)
  private
    FcodAtiv: String;
  public
    property codAtiv: String read FcodAtiv write FcodAtiv;
  end;

  TFatRiscoCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TFatRiscoCollectionItem;
    procedure SetItem(Index: Integer; Value: TFatRiscoCollectionItem);
  public
    function Add: TFatRiscoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TFatRiscoCollectionItem;
    property Items[Index: Integer]: TFatRiscoCollectionItem read GetItem write SetItem;
  end;

  TFatRiscoCollectionItem = class(TObject)
  private
    FcodFatRis: String;
    FtpAval: tptpAval;
    FintConc: Double;
    FlimTol: Double;
    FunMed: Integer;
    FtecMedicao: String;
    Finsalubridade: tpSimNao;
    Fpericulosidade: tpSimNao;
    FaposentEsp: tpSimNao;
    FEpcEpi: TEpcEpi;
    function getEpcEpi: TEpcEpi;
  public
    constructor Create;
    destructor Destroy; override;

    property codFatRis: String read FcodFatRis write FcodFatRis;
    property tpAval: tptpAval read FtpAval write FtpAval;
    property intConc: Double read FintConc write FintConc;
    property limTol: Double read FlimTol write FlimTol;
    property unMed: Integer read FunMed write FunMed;
    property tecMedicao: String read FtecMedicao write FtecMedicao;
    property insalubridade: tpSimNao read Finsalubridade write Finsalubridade;
    property periculosidade: tpSimNao read Fpericulosidade write Fpericulosidade;
    property aposentEsp: tpSimNao read FaposentEsp write FaposentEsp;
    property epcEpi: TEpcEpi read getEpcEpi write FEpcEpi;
  end;

  TEpcEpi = class(TObject)
  private
    FUtilizEPC: tpUtilizEPC;
    FEficEpc: tpSimNao;
    FUtilizEPI: tpUtilizEPI;
    FEpi: TEpiCollection;

    function getEpi: TEpiCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function epiInst: boolean;

    property utilizEPC: tpUtilizEPC read FUtilizEPC write FUtilizEPC;
    property eficEpc: tpSimNao read FEficEpc write FEficEpc;
    property utilizEPI: tpUtilizEPI read FUtilizEPI write FUtilizEPI;
    property epi: TEpiCollection read getEpi write FEpi;
  end;

  TEpiCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TEpiCollectionItem;
    procedure SetItem(Index: Integer; Value: TEpiCollectionItem);
  public
    function Add: TEpiCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TEpiCollectionItem;
    property Items[Index: Integer]: TEpiCollectionItem read GetItem write SetItem; default;
  end;

  TEpiCollectionItem = class(TObject)
  private
    FcaEPI: String;
    FdscEPI: String;
    FeficEpi: tpSimNao;
    FperiodicTroca: tpSimNao;
    FcondFuncto: tpSimNao;
    FusoInint: tpSimNao;
    Fhigienizacao: tpSimNao;
    FmedProtecao: tpSimNao;
    FprzValid: tpSimNao;
  public
    property caEPI: string read FcaEPI write FcaEPI;
    property dscEPI: string read FdscEPI write FdscEPI;
    property eficEpi : tpSimNao read FeficEpi write FeficEpi;
    property medProtecao: tpSimNao read FmedProtecao write FmedProtecao;
    property condFuncto: tpSimNao read FcondFuncto write FcondFuncto;
    property usoInint: tpSimNao read FusoInint write FusoInint;
    property przValid: tpSimNao read FprzValid write FprzValid;
    property periodicTroca: tpSimNao read FperiodicTroca write FperiodicTroca;
    property higienizacao: tpSimNao read Fhigienizacao write Fhigienizacao;
  end;

  TObs = class(TObject)
  private
    FmetErg: String;
    FobsCompl: String;
  public
    property metErg: String read FmetErg write FmetErg;
    property obsCompl: String read FobsCompl write FobsCompl;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS2240Collection }

function TS2240Collection.Add: TS2240CollectionItem;
begin
  Result := Self.New;
end;

function TS2240Collection.GetItem(Index: Integer): TS2240CollectionItem;
begin
  Result := TS2240CollectionItem(inherited GetItem(Index));
end;

procedure TS2240Collection.SetItem(Index: Integer; Value: TS2240CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TS2240Collection.New: TS2240CollectionItem;
begin
  Result := TS2240CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2240CollectionItem }

constructor TS2240CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento  := teS2240;
  FEvtExpRisco := TEvtExpRisco.Create(AOwner);
end;

destructor TS2240CollectionItem.Destroy;
begin
  FEvtExpRisco.Free;

  inherited;
end;

{ TEvtAltContratual }

constructor TEvtExpRisco.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo    := TIdeVinculo.Create;
  FInfoExpRisco  := TInfoExpRisco.Create;
end;

destructor TEvtExpRisco.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FInfoExpRisco.Free;

  inherited;
end;

procedure TEvtExpRisco.GerarEPI(objEPI: TEpiCollection);
var
  i: integer;
begin
  for i := 0 to objEPI.Count -1 do
  begin
    Gerador.wGrupo('epi');

    Gerador.wCampo(tcStr, '', 'caEPI',         1,  20, 0, objEPI[i].caEPI);
    Gerador.wCampo(tcStr, '', 'dscEPI',        1, 999, 0, objEPI[i].dscEPI);
    Gerador.wCampo(tcStr, '', 'eficEpi',       1,   1, 1, eSSimNaoToStr(objEPI[i].eficEpi));
    Gerador.wCampo(tcStr, '', 'medProtecao',   1,   1, 1, eSSimNaoToStr(objEPI[i].medProtecao));
    Gerador.wCampo(tcStr, '', 'condFuncto',    1,   1, 1, eSSimNaoToStr(objEPI[i].condFuncto));
    Gerador.wCampo(tcStr, '', 'usoInint',      1,   1, 1, eSSimNaoToStr(objEPI[i].usoInint));
    Gerador.wCampo(tcStr, '', 'przValid',      1,   1, 1, eSSimNaoToStr(objEPI[i].przValid));
    Gerador.wCampo(tcStr, '', 'periodicTroca', 1,   1, 1, eSSimNaoToStr(objEPI[i].periodicTroca));
    Gerador.wCampo(tcStr, '', 'higienizacao',  1,   1, 1, eSSimNaoToStr(objEPI[i].higienizacao));

    Gerador.wGrupo('/epi');
  end;

  if objEPI.Count > 50 then
    Gerador.wAlerta('', 'epi', 'Lista de EPI', ERR_MSG_MAIOR_MAXIMO + '50');
end;

procedure TEvtExpRisco.GerarEpcEpi(pEpcEpi: TEpcEpi);
begin
  Gerador.wGrupo('epcEpi');

  Gerador.wCampo(tcInt, '', 'utilizEPC', 1, 1, 1, eStpUtilizEPCToStr(pEpcEpi.utilizEPC));
  if pEpcEpi.utilizEPC = uEPCImplementa then
    Gerador.wCampo(tcStr, '', 'eficEpc',   1, 1, 1, eSSimNaoToStr(pEpcEpi.eficEpc));
  Gerador.wCampo(tcInt, '', 'utilizEPI', 1, 1, 1, eStpUtilizEPIToStr(pEpcEpi.utilizEPI));

  if pEpcEpi.epiInst then
    GerarEPI(pEpcEpi.epi);

  Gerador.wGrupo('/epcEpi');
end;

procedure TEvtExpRisco.GerarFatRisco(objFatRisco: TFatRiscoCollection);
var
  i: Integer;
begin
  for I := 0 to objFatRisco.Count - 1 do
  begin
    Gerador.wGrupo('fatRisco');

    Gerador.wCampo(tcStr, '', 'codFatRis',      1, 10, 1, objFatRisco.Items[i].codFatRis);
    Gerador.wCampo(tcStr, '', 'tpAval',         1,  1, 1, tpAvalToStr(objFatRisco.Items[i].tpAval));
    Gerador.wCampo(tcDe4, '', 'intConc',        1, 10, 0, objFatRisco.Items[i].intConc);
    Gerador.wCampo(tcDe4, '', 'limTol',         1, 10, 0, objFatRisco.Items[i].limTol);
    Gerador.wCampo(tcInt, '', 'unMed',          1,  2, 0, objFatRisco.Items[i].unMed);
    Gerador.wCampo(tcStr, '', 'tecMedicao',     1, 40, 0, objFatRisco.Items[i].tecMedicao);
    Gerador.wCampo(tcStr, '', 'insalubridade',  1,  1, 1, eSSimNaoToStr(objFatRisco.Items[i].insalubridade));
    Gerador.wCampo(tcStr, '', 'periculosidade', 1,  1, 1, eSSimNaoToStr(objFatRisco.Items[i].periculosidade));
    Gerador.wCampo(tcStr, '', 'aposentEsp',     1,  1, 1, eSSimNaoToStr(objFatRisco.Items[i].aposentEsp));

    GerarEpcEpi(objFatRisco.Items[i].epcEpi);

    Gerador.wGrupo('/fatRisco');
  end;

  if objFatRisco.Count > 999 then
    Gerador.wAlerta('', 'fatRisco', 'Lista de Fatores de Riscos', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtExpRisco.GerarInfoAmb(objInfoAmb: TinfoAmbCollection);
var
  j: integer;
begin
  for j := 0 to objInfoAmb.Count - 1 do
  begin
    Gerador.wGrupo('infoAmb');
     Gerador.wCampo(tcStr, '', 'codAmb', 1, 30, 1, objInfoAmb.items[j].codAmb);
    Gerador.wGrupo('/infoAmb');
  end;

  if objInfoAmb.Count > 99 then
    Gerador.wAlerta('', 'infoAmb', 'Lista de Informações Ambientais', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtExpRisco.GerarInfoAtiv(objInfoAtiv: TInfoAtiv);
var
  j: integer;
begin
  Gerador.wGrupo('infoAtiv');

  Gerador.wCampo(tcStr, '', 'dscAtivDes', 1, 999, 1, objInfoAtiv.dscAtivDes);

  for j := 0 to objInfoAtiv.ativPericInsal.Count - 1 do
  begin
    Gerador.wGrupo('ativPericInsal');
     Gerador.wCampo(tcStr, '', 'codAtiv', 1, 6, 1, objInfoAtiv.ativPericInsal.Items[j].codAtiv);
    Gerador.wGrupo('/ativPericInsal');
  end;

  Gerador.wGrupo('/infoAtiv');

  if objInfoAtiv.ativPericInsal.Count > 99 then
    Gerador.wAlerta('', 'ativPericInsal', 'Lista de Informação da(s) atividade(s) perigosa(s), insalubre(s) ou especial(is) desempenhada(s)', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtExpRisco.GerarRespReg(pRespReg: TRespRegCollection);
var
  i: integer;
begin
  for i := 0 to pRespReg.Count - 1 do
  begin
    Gerador.wGrupo('respReg');

    Gerador.wCampo(tcStr, '', 'cpfResp', 1, 11, 1, pRespReg[i].cpfResp);
    Gerador.wCampo(tcStr, '', 'nisResp', 1, 11, 1, pRespReg[i].nisResp);
    Gerador.wCampo(tcStr, '', 'nmResp',  1, 70, 1, pRespReg[i].nmResp);
    Gerador.wCampo(tcStr, '', 'ideOC',   1,  1, 1, eSIdeOCToStr(pRespReg[i].ideOC));
    if pRespReg[i].ideOC = idOutros then
      Gerador.wCampo(tcStr, '', 'dscOC',   1, 20, 1, pRespReg[i].dscOC);
    Gerador.wCampo(tcStr, '', 'nrOC',    1, 14, 1, pRespReg[i].nrOc);
    Gerador.wCampo(tcStr, '', 'ufOC',    2,  2, 0, eSufToStr(pRespReg[i].ufOC));

    Gerador.wGrupo('/respReg');
  end;

  if pRespReg.Count > 9 then
    Gerador.wAlerta('', 'respReg', 'Lista de Responsáveis pelo registro', ERR_MSG_MAIOR_MAXIMO + '9');
end;

procedure TEvtExpRisco.GerarInfoExpRisco(objInfoExpRisco: TInfoExpRisco);
begin
  Gerador.wGrupo('infoExpRisco');

  Gerador.wCampo(tcDat, '', 'dtIniCondicao',   10, 10, 1, objInfoExpRisco.dtIniCondicao);
  GerarInfoAmb(objInfoExpRisco.InfoAmb);
  GerarInfoAtiv(objInfoExpRisco.infoAtiv);
  GerarFatRisco(objInfoExpRisco.fatRisco);
  GerarRespReg(objInfoExpRisco.respReg);
  GerarObs(objInfoExpRisco.obs);

  Gerador.wGrupo('/infoExpRisco');
end;

procedure TEvtExpRisco.GerarObs(pObs: TObs);
begin
  if (pObs.metErg <> '') or (pObs.obsCompl <> '') then
  begin
     Gerador.wGrupo('obs');
     if (pObs.metErg <> '') then
       Gerador.wCampo(tcStr, '', 'metErg',   1, 999, 0, pObs.metErg);
     if (pObs.obsCompl <> '') then
       Gerador.wCampo(tcStr, '', 'obsCompl', 1, 999, 0, pObs.obsCompl);
     Gerador.wGrupo('/obs');
  end;
end;

function TEvtExpRisco.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtExpRisco');
    Gerador.wGrupo('evtExpRisco Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(self.IdeVinculo);
    GerarInfoExpRisco(self.InfoExpRisco);

    Gerador.wGrupo('/evtExpRisco');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtExpRisco');

    Validar(schevtExpRisco);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

{ TinfoExpRisco }

constructor TinfoExpRisco.Create;
begin
  inherited;

  FInfoAmb  := TInfoAmbCollection.Create;
  FInfoAtiv := TInfoAtiv.Create;
  FFatRisco := TFatRiscoCollection.Create;
  FRespReg  := TRespRegCollection.Create;;
  FObs      := TObs.Create;
end;

destructor TinfoExpRisco.destroy;
begin
  FinfoAmb.Free;
  FInfoAtiv.Free;
  FFatRisco.Free;
  FRespReg.Free;
  FObs.Free;

  inherited;
end;

function TinfoExpRisco.getRespReg: TRespRegCollection;
begin
  if not Assigned(FRespReg) then
    FRespReg := TRespRegCollection.Create;
  Result := FRespReg;
end;

{ TRespRegCollection }

function TRespRegCollection.Add: TRespRegCollectionItem;
begin
  Result := Self.New;
end;

function TRespRegCollection.GetItem(Index: Integer): TRespRegCollectionItem;
begin
  Result := TRespRegCollectionItem(inherited GetItem(Index));
end;

procedure TRespRegCollection.SetItem(Index: Integer; Value: TRespRegCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TEvtExpRisco.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J, K: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtExpRisco';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideVinculo';
      ideVinculo.CpfTrab   := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      ideVinculo.NisTrab   := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);
      ideVinculo.Matricula := INIRec.ReadString(sSecao, 'matricula', EmptyStr);

      sSecao := 'iniExpRisco';
      if INIRec.ReadString(sSecao, 'dtIniCondicao', '') <> '' then
      begin
        infoExpRisco.dtIniCondicao := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniCondicao', '0'));

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoAmb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'codAmb', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoExpRisco.infoAmb.New do
            codAmb := sFim;

          with infoExpRisco do
          begin
            sSecao := 'infoAtiv' + IntToStrZero(I, 2);
            infoAtiv.dscAtivDes := INIRec.ReadString(sSecao, 'dscAtivDes', EmptyStr);

            J := 1;
            while true do
            begin
              // de 01 até 99
              sSecao := 'ativPericInsal' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
              sFim   := INIRec.ReadString(sSecao, 'codAtiv', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with infoAtiv.ativPericInsal.New do
              begin
                codAtiv  := sFim;
              end;

              Inc(J);
            end;

            J := 1;
            while true do
            begin
              // de 001 até 999
              sSecao := 'fatRisco' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'codFatRis', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with fatRisco.New do
              begin
                codFatRis  := sFim;
                tpAval     := StrTotpAval(Ok, INIRec.ReadString(sSecao, 'tpAval', '0'));
                intConc    := StringToFloatDef(INIRec.ReadString(sSecao, 'intConc', EmptyStr), 0);
                limTol     := StringToFloatDef(INIRec.ReadString(sSecao, 'limTol', EmptyStr), 0);
                unMed      := INIRec.ReadInteger(sSecao, 'unMed', 0);
                tecMedicao := INIRec.ReadString(sSecao, 'tecMedicao', EmptyStr);
                insalubridade  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'insalubridade', '0'));
                periculosidade := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'periculosidade', '0'));
                aposentEsp     := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'aposentEsp', '0'));

                epcEpi.utilizEPC := eSStrTotpUtilizEPC(Ok, INIRec.ReadString(sSecao, 'utilizEPC', '0'));
                epcEpi.eficEpc := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'eficEpc', '0'));
                epcEpi.utilizEPI := eSStrTotpUtilizEPI(Ok, INIRec.ReadString(sSecao, 'utilizEPI', '0'));

                {
                Não consta mais na nova versão
                K := 1;
                while true do
                begin
                  // de 00 até 50
                  sSecao := 'epc' + IntToStrZero(I, 2) + IntToStrZero(J, 3) +
                               IntToStrZero(K, 2);
                  sFim   := INIRec.ReadString(sSecao, 'dscEpc', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with epcEpi.epc.Add do
                  begin
                    dscEpc  := sFim;
                    eficEpc := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'eficEpc', 'S'));
                  end;

                  Inc(K);
                end;
                }
                K := 1;
                while true do
                begin
                  // de 00 até 50
                  sSecao := 'epi' + IntToStrZero(I, 2) + IntToStrZero(J, 3) +
                               IntToStrZero(K, 2);
                  sFim   := INIRec.ReadString(sSecao, 'caEPI', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with epcEpi.epi.New do
                  begin
                    caEPI         := sFim;
                    dscEPI        := INIRec.ReadString(sSecao, 'dscEPI', EmptyStr);
                    eficEpi       := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'eficEpi', 'S'));
                    medProtecao   := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'medProtecao', 'S'));
                    condFuncto    := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'condFuncto', 'S'));
                    usoInint      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'usoInint', 'S'));
                    przValid      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'przValid', 'S'));
                    periodicTroca := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'periodicTroca', 'S'));
                    higienizacao  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'higienizacao', 'S'));
                  end;

                  Inc(K);
                end;

              end;

              Inc(J);
            end;
          end;

          Inc(I);
        end;
      end;

      I := 1;
      while true do
      begin
        // de 1 até 9
        sSecao := 'respReg' + IntToStrZero(I, 1);
        sFim   := INIRec.ReadString(sSecao, 'dtIni', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infoExpRisco.respReg.New do
        begin
            // Não consta mais na nova versão
//          dtIni   := StringToDateTime(sFim);
//          dtFim   := StringToDateTime(INIRec.ReadString(sSecao, 'dtFim', '0'));
          cpfResp := INIRec.ReadString(sSecao, 'cpfResp', EmptyStr);
          nisResp := INIRec.ReadString(sSecao, 'nisResp', EmptyStr);
          nmResp  := INIRec.ReadString(sSecao, 'nmResp', EmptyStr);
          ideOC   := eSStrToIdeOC(Ok, INIRec.ReadString(sSecao, 'ideOC', EmptyStr));
          dscOC   := INIRec.ReadString(sSecao, 'dscOC', EmptyStr);
          nrOC    := INIRec.ReadString(sSecao, 'nrOc', EmptyStr);
          ufOC    := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'ufOC', 'SP'));
        end;

        Inc(I);
      end;
    end;

    GerarXML;
  finally
     INIRec.Free;
  end;
end;

function TRespRegCollection.New: TRespRegCollectionItem;
begin
  Result := TRespRegCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoAmbCollection }

function TInfoAmbCollection.Add: TInfoAmbCollectionItem;
begin
  Result := Self.New;
end;

function TInfoAmbCollection.GetItem(Index: Integer): TInfoAmbCollectionItem;
begin
  Result := TInfoAmbCollectionItem(inherited GetItem(index));
end;

procedure TInfoAmbCollection.SetItem(Index: Integer; Value: TInfoAmbCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TInfoAmbCollection.New: TInfoAmbCollectionItem;
begin
  Result := TInfoAmbCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoAtiv }

constructor TInfoAtiv.Create;
begin
  inherited;

  FativPericInsal := TAtivPericInsalCollection.Create;
end;

destructor TInfoAtiv.Destroy;
begin
  FativPericInsal.Free;
  
  inherited;
end;

{ TAtivPericInsalCollection }

function TAtivPericInsalCollection.Add: TAtivPericInsalCollectionItem;
begin
  Result := Self.New;
end;

function TAtivPericInsalCollection.GetItem(Index: Integer): TAtivPericInsalCollectionItem;
begin
  Result := TAtivPericInsalCollectionItem(inherited GetItem(index))
end;

procedure TAtivPericInsalCollection.SetItem(Index: Integer; Value: TAtivPericInsalCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TAtivPericInsalCollection.New: TAtivPericInsalCollectionItem;
begin
  Result := TAtivPericInsalCollectionItem.Create;
  Self.Add(Result);
end;

{ TFatRiscoCollection }

function TFatRiscoCollection.Add: TFatRiscoCollectionItem;
begin
  Result := Self.New;
end;

function TFatRiscoCollection.GetItem(Index: Integer): TFatRiscoCollectionItem;
begin
  Result := TFatRiscoCollectionItem(inherited GetItem(index))
end;

procedure TFatRiscoCollection.SetItem(Index: Integer; Value: TFatRiscoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TFatRiscoCollection.New: TFatRiscoCollectionItem;
begin
  Result := TFatRiscoCollectionItem.Create;
  Self.Add(Result);
end;

{ TFatRiscoCollectionItem }

constructor TFatRiscoCollectionItem.Create;
begin
  inherited Create;
  FEpcEpi := TEpcEpi.Create;
end;

destructor TFatRiscoCollectionItem.Destroy;
begin
  FEpcEpi.Free;
  
  inherited;
end;

function TFatRiscoCollectionItem.getEpcEpi: TEpcEpi;
begin
  if not Assigned(FEpcEpi) then
    FEpcEpi := TEpcEpi.Create;
  result := FEpcEpi;
end;

{ TEpcEpi }

constructor TEpcEpi.Create;
begin
  inherited;

  FEpi := nil;
end;

destructor TEpcEpi.Destroy;
begin
  FreeAndNil(FEpi);

  inherited;
end;

function TEpcEpi.getEpi: TEpiCollection;
begin
  if not Assigned(FEpi) then
    FEpi := TEpiCollection.Create;
  result := FEpi;
end;

function TEpcEpi.epiInst: boolean;
begin
  result := Assigned(FEpi);
end;

{ TEpiCollection }
function TEpiCollection.Add: TEpiCollectionItem;
begin
  Result := Self.New;
end;

function TEpiCollection.GetItem(Index: Integer): TEpiCollectionItem;
begin
  Result := TEpiCollectionItem(inherited GetItem(Index));
end;

procedure TEpiCollection.SetItem(Index: Integer; Value: TEpiCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TEpiCollection.New: TEpiCollectionItem;
begin
  Result := TEpiCollectionItem.Create;
  Self.Add(Result);
end;

end.
