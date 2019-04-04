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
|* 29/02/2016: Guilherme Costa
|*  - Alterado os atributos que não estavam de acordo com o leiaute/xsd
******************************************************************************}
{$I ACBr.inc}

unit pcesS1202;

interface

uses
  SysUtils, Classes, Dialogs, Controls, Contnrs,
  pcnConversao, pcnGerador, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type

  TRemunPer1202Collection = class;
  TRemunPer1202CollectionItem = class;
  TIdeEstabCollection = class;
  TIdeEstabCollectionItem = class;
  TIdePeriodoCollectionItem = class;
  TIdePeriodoCollection = class;
  TIdeADCCollectionItem = class;
  TIdeADCCollection = class;
  TInfoPerAnt = class;
  TInfoPerApur = class;
  TeS1202IdeTrabalhador = class;
  TEvtRemunRPPS = class;
  TS1202CollectionItem = class;
  TDMDevCollection = class;
  TDMDevCollectionItem = class;

  TS1202Collection = class(TeSocialCollection)
  private
    function GetItem(Index: integer): TS1202CollectionItem;
    procedure SetItem(Index: integer; Value: TS1202CollectionItem);
  public
    function Add: TS1202CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1202CollectionItem;
    property Items[Index: integer]: TS1202CollectionItem read GetItem write SetItem;
      default;
  end;

  TS1202CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtRmnRPPS: TEvtRemunRPPS;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtRmnRPPS: TEvtRemunRPPS read FEvtRmnRPPS write FEvtRmnRPPS;
  end;

  TDMDevCollection = class(TObjectList)
  private
    function GetItem(Index: integer): TDMDevCollectionItem;
    procedure SetItem(Index: integer; Value: TDMDevCollectionItem);
  public
    function Add: TDMDevCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDMDevCollectionItem;
    property Items[Index: integer]: TDMDevCollectionItem read GetItem write SetItem;
      default;
  end;

  TDMDevCollectionItem = class(TObject)
  private
    FIdeDmDev: string;
    FInfoPerApur: TInfoPerApur;
    FInfoPerAnt: TInfoPerAnt;

    function getInfoPerApur: TInfoPerApur;
    function getInfoPerAnt: TInfoPerAnt;
  public
    constructor Create;
    destructor Destroy; override;

    function infoPerApurInst(): boolean;
    function infoPerAntInst(): boolean;
    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property infoPerApur: TInfoPerApur read getInfoPerApur write FInfoPerApur;
    property infoPerAnt: TInfoPerAnt read getInfoPerAnt write FInfoPerAnt;
  end;

  TEvtRemunRPPS = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabalhador: TeS1202IdeTrabalhador;
    FDMDev: TDMDevCollection;

    {Geradores específicos desta classe}
    procedure GerarIdeEstab(objIdeEstab: TIdeEstabCollection;
      const nomeRemunPer: string = 'remunPerApur');
    procedure GerarRemunPer(objRemunPer: TRemunPer1202Collection;
      const nomeRemunPer: string = 'remunPerApur');
    procedure GerarIdePeriodo(objIdePeriodo: TIdePeriodoCollection);
    procedure GerarIdeADC(objIdeADC: TideADCCollection);

    procedure GerarIdeTrabalhador;
    procedure GerarDmDev;
    procedure GerarInfoPerApur(pInfoPerApur: TInfoPerApur);
    procedure GerarInfoPerAnt(pInfoPerAnt: TInfoPerAnt);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento3 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property ideTrabalhador: TeS1202IdeTrabalhador
      read FIdeTrabalhador write FIdeTrabalhador;
    property dmDev: TDMDevCollection read FDMDev write FDMDev;
  end;

  TRemunPer1202Collection = class(TObjectList)
  private
    FNomeGrupoXML: string;

    function GetItem(Index: integer): TRemunPer1202CollectionItem;
    procedure SetItem(Index: integer; Value: TRemunPer1202CollectionItem);
  public
    function Add: TRemunPer1202CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRemunPer1202CollectionItem;
    property Items[Index: integer]: TRemunPer1202CollectionItem read GetItem write SetItem;
    property grupoXML: string read FNomeGrupoXML;
  end;

  TRemunPer1202CollectionItem = class(TRemunPerCollectionItem)
  private
    FCodCateg: Integer;
  public
    property codCateg: Integer read FCodCateg write FCodCateg;
  end;

  TIdeEstabCollection = class(TObjectList)
  private
    function GetItem(Index: integer): TIdeEstabCollectionItem;
    procedure SetItem(Index: integer; Value: TIdeEstabCollectionItem);
  public
    function Add: TIdeEstabCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeEstabCollectionItem;
    property Items[Index: integer]: TIdeEstabCollectionItem
      read GetItem write SetItem;
  end;

  TIdeEstabCollectionItem = class(TObject)
  private
    FTpInsc: TpTpInsc;
    FNrInsc: string;
    FRemunPerApur: TRemunPer1202Collection;
    FRemunPerAnt: TRemunPer1202Collection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInsc: TpTPInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property remunPerApur: TRemunPer1202Collection read FRemunPerApur write FRemunPerApur;
    property remunPerAnt: TRemunPer1202Collection read FRemunPerAnt write FRemunPerAnt;
  end;

  TIdePeriodoCollection = class(TObjectList)
  private
    function GetItem(Index: integer): TIdePeriodoCollectionItem;
    procedure SetItem(Index: integer; Value: TIdePeriodoCollectionItem);
  public
    function Add: TIdePeriodoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdePeriodoCollectionItem;
    property Items[Index: integer]: TIdePeriodoCollectionItem read GetItem write SetItem;
  end;

  TIdePeriodoCollectionItem = class(TObject)
  private
    FPerRef: string;
    FIdeEstab: TIdeEstabCollection;
  public
    constructor Create;
    destructor Destroy; override;
    
    property perRef: string read FPerRef write FPerRef;
    property ideEstab: TIdeEstabCollection read FIdeEstab write FIdeEstab;
  end;

  TIdeADCCollection = class(TObjectList)
  private
    function GetItem(Index: integer): TIdeADCCollectionItem;
    procedure SetItem(Index: integer; Value: TIdeADCCollectionItem);
  public
    function Add: TIdeADCCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeADCCollectionItem;
    property Items[Index: integer]: TIdeADCCollectionItem read GetItem write SetItem;
  end;

  TIdeADCCollectionItem = class(TObject)
  private
    FDtLei: TDate;
    FNrLei: string;
    FDtEf: TDate;
    FIdePeriodo: TIdePeriodoCollection;
  public
    constructor Create;

    property DtLei: TDate read FDtLei write FDtLei;
    property nrLei: string read FNrLei write FNrLei;
    property DtEf: TDate read FDtEf write FDtEf;
    property idePeriodo: TIdePeriodoCollection read FIdePeriodo write FIdePeriodo;
  end;

  TInfoPerAnt = class(TObject)
  private
    FIdeADC: TIdeADCCollection;
  public
    constructor Create;
    destructor Destroy; override;
    
    property ideADC: TIdeADCCollection read FIdeADC write FIdeADC;
  end;

  TInfoPerApur = class(TObject)
  private
    FIdeEstab: TIdeEstabCollection;
  public
    constructor Create;
    destructor Destroy; override;
    
    property ideEstab: TIdeEstabCollection read FIdeEstab write FIdeEstab;
  end;

  TeS1202IdeTrabalhador = class(TideTrabalhador2)
  private
    FQtdDepFP: integer;
    FProcJudTrab: TProcJudTrabCollection;

    function getProcJudTrab: TProcJudTrabCollection;
  public
    constructor Create;
    destructor Destroy; override;
    
    function procJudTrabInst: boolean;

    property qtdDepFP: Integer read FQtdDepFP write FQtdDepFP;
    property procJudTrab: TProcJudTrabCollection read getProcJudTrab write FProcJudTrab;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TRemunPer1202Collection }

function TRemunPer1202Collection.Add: TRemunPer1202CollectionItem;
begin
  Result := Self.New;
end;

function TRemunPer1202Collection.GetItem(Index: integer): TRemunPer1202CollectionItem;
begin
  Result := TRemunPer1202CollectionItem(inherited GetItem(Index));
end;

procedure TRemunPer1202Collection.SetItem(Index: integer; Value: TRemunPer1202CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TRemunPer1202Collection.New: TRemunPer1202CollectionItem;
begin
  Result := TRemunPer1202CollectionItem.Create;
  Self.Add(Result);
end;

{ TIdeEstabCollectionItem }

constructor TIdeEstabCollectionItem.Create;
begin
  inherited Create;
  FRemunPerApur := TRemunPer1202Collection.Create;
  FRemunPerAnt  := TRemunPer1202Collection.Create;
end;

destructor TIdeEstabCollectionItem.Destroy;
begin
  FRemunPerApur.Free;
  FRemunPerAnt.Free;

  inherited;
end;

{ TIdeEstabCollection }
function TIdeEstabCollection.Add: TIdeEstabCollectionItem;
begin
  Result := Self.New;
end;

function TIdeEstabCollection.GetItem(Index: integer): TIdeEstabCollectionItem;
begin
  Result := TIdeEstabCollectionItem(inherited GetItem(Index));
end;

procedure TIdeEstabCollection.SetItem(Index: integer;
  Value: TIdeEstabCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TIdeEstabCollection.New: TIdeEstabCollectionItem;
begin
  Result := TIdeEstabCollectionItem.Create;
  Self.Add(Result);
end;

{ TIdePeriodoCollectionItem }
constructor TIdePeriodoCollectionItem.Create;
begin
  inherited Create;
  FIdeEstab := TIdeEstabCollection.Create;
end;

destructor TIdePeriodoCollectionItem.Destroy;
begin
  FIdeEstab.Free;

  inherited;
end;

{ TIdePeriodoCollection }
function TIdePeriodoCollection.Add: TIdePeriodoCollectionItem;
begin
  Result := Self.New;
end;

function TIdePeriodoCollection.GetItem(Index: integer): TIdePeriodoCollectionItem;
begin
  Result := TIdePeriodoCollectionItem(inherited GetItem(Index));
end;

procedure TIdePeriodoCollection.SetItem(Index: integer;
  Value: TIdePeriodoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TIdePeriodoCollection.New: TIdePeriodoCollectionItem;
begin
  Result := TIdePeriodoCollectionItem.Create;
  Self.Add(Result);
end;

{ TIdeADCCollectionItem }
constructor TIdeADCCollectionItem.Create;
begin
  inherited Create;
  FIdePeriodo := TIdePeriodoCollection.Create;
end;

{ TIdeADCCollection }
function TIdeADCCollection.add: TIdeADCCollectionItem;
begin
  Result := Self.New;
end;

function TIdeADCCollection.GetItem(Index: integer): TIdeADCCollectionItem;
begin
  Result := TIdeADCCollectionItem(inherited GetItem(Index));
end;

procedure TIdeADCCollection.SetItem(Index: integer; Value: TIdeADCCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TIdeADCCollection.New: TIdeADCCollectionItem;
begin
  Result := TIdeADCCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoPerAnt }
constructor TInfoPerAnt.Create;
begin
  inherited;

  FIdeADC := TIdeADCCollection.Create;
end;

destructor TInfoPerAnt.Destroy;
begin
  FIdeADC.Free;

  inherited;
end;

{ TInfoPerApur }
constructor TInfoPerApur.Create;
begin
  inherited;

  FIdeEstab := TIdeEstabCollection.Create;
end;

destructor TInfoPerApur.Destroy;
begin
  FIdeEstab.Free;

  inherited;
end;

{ TideTrabalhador }
constructor TeS1202IdeTrabalhador.Create;
begin
  inherited Create;
  FProcJudTrab := nil;
end;

destructor TeS1202IdeTrabalhador.Destroy;
begin
  FreeAndNil(FProcJudTrab);

  inherited;
end;

function TeS1202IdeTrabalhador.getProcJudTrab: TProcJudTrabCollection;
begin
  if not Assigned(FProcJudTrab) then
    FProcJudTrab := TProcJudTrabCollection.Create;
  Result := FProcJudTrab;
end;

function TeS1202IdeTrabalhador.procJudTrabInst: boolean;
begin
  result := Assigned(FProcJudTrab);
end;

{ TDMDevCollection }

function TDMDevCollection.Add: TDMDevCollectionItem;
begin
  Result := Self.New;
end;

function TDMDevCollection.GetItem(Index: integer): TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem(inherited GetItem(Index));
end;

procedure TDMDevCollection.SetItem(Index: integer; Value: TDMDevCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TDMDevCollection.New: TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem.Create;
  Self.Add(Result);
end;

{ TDMDevCollectionItem }

constructor TDMDevCollectionItem.Create;
begin
  inherited Create;
  FInfoPerApur := nil;
  FInfoPerAnt  := nil;
end;

destructor TDMDevCollectionItem.Destroy;
begin
  FreeAndNil(FInfoPerApur);
  FreeAndNil(FInfoPerAnt);

  inherited;
end;

function TDMDevCollectionItem.getInfoPerApur: TInfoPerApur;
begin
  if not (Assigned(FInfoPerApur)) then
    FInfoPerApur := TInfoPerApur.Create;
  Result := FInfoPerApur;
end;

function TDMDevCollectionItem.infoPerApurInst: boolean;
begin
  Result := Assigned(FInfoPerApur);
end;

function TDMDevCollectionItem.getInfoPerAnt: TInfoPerAnt;
begin
  if not (Assigned(FInfoPerAnt)) then
    FInfoPerAnt := TInfoPerAnt.Create;
  Result := FInfoPerAnt;
end;

function TDMDevCollectionItem.infoPerAntInst: boolean;
begin
  Result := Assigned(FInfoPerAnt);
end;

{ TEvtRemunRPPS }
constructor TEvtRemunRPPS.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento      := TIdeEvento3.Create;
  FIdeEmpregador  := TIdeEmpregador.Create;
  FIdeTrabalhador := TeS1202IdeTrabalhador.Create;
  FDMDev          := TDMDevCollection.Create;
end;

destructor TEvtRemunRPPS.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrabalhador.Free;
  FDMDev.Free;

  inherited;
end;

procedure TEvtRemunRPPS.GerarIdeADC(objIdeADC: TideADCCollection);
var
  i: integer;
begin
  for i := 0 to objIdeADC.Count - 1 do
  begin
    Gerador.wGrupo('ideADC');

    Gerador.wCampo(tcDat, '', 'dtLei', 10, 10, 1, objIdeADC.Items[i].DtLei);
    Gerador.wCampo(tcStr, '', 'nrLei',  1, 12, 1, objIdeADC.Items[i].nrLei);
    Gerador.wCampo(tcDat, '', 'dtEf',  10, 10, 0, objIdeADC.Items[i].DtEf);

    GerarIdePeriodo(objIdeADC.Items[i].idePeriodo);

    Gerador.wGrupo('/ideADC');
  end;

  if objIdeADC.Count > 8 then
    Gerador.wAlerta('', 'ideADC', 'Lista de Leis', ERR_MSG_MAIOR_MAXIMO + '8');
end;

procedure TEvtRemunRPPS.GerarIdeEstab(objIdeEstab: TIdeEstabCollection;
  const nomeRemunPer: string = 'remunPerApur');
var
  i: integer;
begin
  for i := 0 to objIdeEstab.Count - 1 do
  begin
    Gerador.wGrupo('ideEstab');

    Gerador.wCampo(tcInt, '', 'tpInsc', 1,  1, 1, eSTpInscricaoToStr(objIdeEstab.Items[i].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc', 1, 15, 1, objIdeEstab.Items[i].nrInsc);

    if nomeRemunPer = 'remunPerApur' then
      GerarRemunPer(objIdeEstab.Items[i].remunPerApur, nomeRemunPer)
    else
      GerarRemunPer(objIdeEstab.Items[i].remunPerAnt, nomeRemunPer);

    Gerador.wGrupo('/ideEstab');
  end;

  if objIdeEstab.Count > 24 then
    Gerador.wAlerta('', 'ideEstab', 'Lista de ' + nomeRemunPer, ERR_MSG_MAIOR_MAXIMO + '24');
end;

procedure TEvtRemunRPPS.GerarIdePeriodo(objIdePeriodo: TIdePeriodoCollection);
var
  i: integer;
begin
  for i := 0 to objIdePeriodo.Count - 1 do
  begin
    Gerador.wGrupo('idePeriodo');
    Gerador.wCampo(tcStr, '', 'perRef', 7, 7, 1, objIdePeriodo.Items[i].perRef);

    GerarIdeEstab(objIdePeriodo.Items[i].ideEstab, 'remunPerAnt');

    Gerador.wGrupo('/idePeriodo');
  end;

  if objIdePeriodo.Count > 200 then
    Gerador.wAlerta('', 'idePeriodo', 'Lista de Periodos', ERR_MSG_MAIOR_MAXIMO + '200');
end;

procedure TEvtRemunRPPS.GerarIdeTrabalhador;
begin
  Gerador.wGrupo('ideTrabalhador');

  Gerador.wCampo(tcStr, '', 'cpfTrab',  11, 11, 1, ideTrabalhador.cpfTrab);
  Gerador.wCampo(tcStr, '', 'nisTrab',   1, 11, 0, ideTrabalhador.nisTrab);
  Gerador.wCampo(tcInt, '', 'qtdDepFP',  1,  2, 0, ideTrabalhador.qtdDepFP);

  if ideTrabalhador.procJudTrabInst() then
    GerarProcJudTrab(ideTrabalhador.procJudTrab);

  Gerador.wGrupo('/ideTrabalhador');
end;

procedure TEvtRemunRPPS.GerarInfoPerAnt(pInfoPerAnt: TInfoPerAnt);
begin
  Gerador.wGrupo('infoPerAnt');

  GerarIdeADC(pInfoPerAnt.ideADC);

  Gerador.wGrupo('/infoPerAnt');
end;

procedure TEvtRemunRPPS.GerarDmDev;
var
  i: integer;
begin
  for i := 0 to dmDev.Count - 1 do
  begin
    Gerador.wGrupo('dmDev');

    Gerador.wCampo(tcStr, '', 'ideDmDev', 1, 30, 1, dmDev[i].ideDmDev);

    if (dmDev[i].infoPerApurInst()) then
      GerarInfoPerApur(dmDev[i].infoPerApur);

    if (dmDev[i].infoPerAntInst()) then
      GerarInfoPerAnt(dmDev[i].infoPerAnt);

    Gerador.wGrupo('/dmDev');
  end;

  if dmDev.Count > 99 then
    Gerador.wAlerta('', 'dmDev', 'Lista de Identificação de Demostrativos', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtRemunRPPS.GerarInfoPerApur(pInfoPerApur: TInfoPerApur);
begin
  Gerador.wGrupo('infoPerApur');

  GerarIdeEstab(pInfoPerApur.ideEstab);

  Gerador.wGrupo('/infoPerApur');
end;

procedure TEvtRemunRPPS.GerarRemunPer(objRemunPer: TRemunPer1202Collection;
  const nomeRemunPer: string = 'remunPerApur');
var
  i: integer;
begin
  for i := 0 to objRemunPer.Count - 1 do
  begin
    Gerador.wGrupo(nomeRemunPer);

    Gerador.wCampo(tcStr, '', 'matricula', 1, 30, 0, objRemunPer.Items[i].matricula);
    Gerador.wCampo(tcInt, '', 'codCateg',  1,  3, 1, objRemunPer.Items[i].codCateg);

    GerarItensRemun(objRemunPer.Items[i].itensRemun, 'itensRemun');

    if (nomeRemunPer = 'remunPerApur') then
    begin
      if objRemunPer.Items[i].infoSaudeColetInst() then
        GerarInfoSaudeColet(objRemunPer.Items[i].infoSaudeColet);
    end;

    Gerador.wGrupo('/' + nomeRemunPer);
  end;

  if objRemunPer.Count > 10 then
    Gerador.wAlerta('', nomeRemunPer, 'Lista de ' + nomeRemunPer, ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TEvtRemunRPPS.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtRmnRPPS');
    Gerador.wGrupo('evtRmnRPPS Id="' + Self.Id + '"');

    GerarIdeEvento3(Self.IdeEvento);
    GerarIdeEmpregador(Self.ideEmpregador);
    GerarIdeTrabalhador;
    GerarDmDev;

    Gerador.wGrupo('/evtRmnRPPS');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtRmnRPPS');

    Validar(schevtRmnRPPS);
  except on e: Exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TEvtRemunRPPS.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J, K, L, M, N: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtRmnRPPS';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.IndApuracao := eSStrToIndApuracao(Ok, INIRec.ReadString(sSecao, 'indApuracao', '1'));
      ideEvento.perApur     := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideTrabalhador';
      ideTrabalhador.cpfTrab  := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      ideTrabalhador.nisTrab  := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);
      ideTrabalhador.qtdDepFP := INIRec.ReadInteger(sSecao, 'qtdDepFP', 0);

      I := 1;
      while true do
      begin
        // de 01 até 99
        sSecao := 'procJudTrab' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'nrProcJud', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with ideTrabalhador.procJudTrab.New do
        begin
          tpTrib    := eSStrToTpTributo(Ok, INIRec.ReadString(sSecao, 'tpTrib', '1'));
          nrProcJud := sFim;
          codSusp   := INIRec.ReadString(sSecao, 'codSusp', '');
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        // de 01 até 99
        sSecao := 'dmDev' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'ideDmDev', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with dmDev.New do
        begin
          ideDmDev := sFim;

          J := 1;
          while true do
          begin
            // de 01 até 24
            sSecao := 'ideEstab' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoPerApur.ideEstab.New do
            begin
              tpInsc     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
              nrInsc     := sFim;

              K := 1;
              while true do
              begin
                // de 01 até 10
                sSecao := 'remunPerApur' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                     IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'matricula', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with remunPerApur.New do
                begin
                  matricula := sFim;
                  codCateg  := INIRec.ReadInteger(sSecao, 'codCateg', 0);

                  L := 1;
                  while true do
                  begin
                    // de 001 até 200
                    sSecao := 'itensRemun' + IntToStrZero(I, 2) +
                                IntToStrZero(J, 2) + IntToStrZero(K, 2) +
                                IntToStrZero(L, 3);
                    sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with itensRemun.New do
                    begin
                      codRubr    := sFim;
                      ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', '');
                      qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                      fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                      vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                      vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
                    end;

                    Inc(L);
                  end;

                  L := 1;
                  while true do
                  begin
                    // de 01 até 99
                    sSecao := 'detOper' + IntToStrZero(I, 2) +
                                IntToStrZero(J, 2) + IntToStrZero(K, 2) +
                                IntToStrZero(L, 2);
                    sFim   := INIRec.ReadString(sSecao, 'cnpjOper', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with infoSaudeColet.detOper.New do
                    begin
                      cnpjOper := sFim;
                      regANS   := INIRec.ReadString(sSecao, 'regANS', '');
                      vrPgTit  := StringToFloatDef(INIRec.ReadString(sSecao, 'vrPgTit', ''), 0);

                      M := 1;
                      while true do
                      begin
                        // de 01 até 99
                        sSecao := 'detPlano' + IntToStrZero(I, 2) +
                                    IntToStrZero(J, 2) + IntToStrZero(K, 2) +
                                    IntToStrZero(L, 2) + IntToStrZero(M, 2);
                        sFim   := INIRec.ReadString(sSecao, 'cpfDep', 'FIM');

                        if (sFim = 'FIM') or (Length(sFim) <= 0) then
                          break;

                        with detPlano.New do
                         begin
                          tpDep    := eSStrToTpDep(Ok, INIRec.ReadString(sSecao, 'tpDep', '00'));
                          cpfDep   := sFim;
                          nmDep    := INIRec.ReadString(sSecao, 'nmDep', '');
                          dtNascto := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
                          vlrPgDep := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPgDep', ''), 0);
                        end;

                        Inc(M);
                      end;

                    end;

                    Inc(L);
                  end;

                end;

                Inc(K);
              end;

            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 1 até 8
            sSecao := 'ideADC' + IntToStrZero(I, 2) + IntToStrZero(J, 1);
            sFim   := INIRec.ReadString(sSecao, 'dtLei', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoPerAnt.ideADC.New do
            begin
              dtLei := StringToDateTime(sFim);
              nrLei := INIRec.ReadString(sSecao, 'nrLei', '');
              dtEf  := StringToDateTime(INIRec.ReadString(sSecao, 'dtEf', '0'));

              K := 1;
              while true do
              begin
                // de 001 até 200
                sSecao := 'idePeriodo' + IntToStrZero(I, 2) + IntToStrZero(J, 1) +
                   IntToStrZero(K, 3);
                sFim   := INIRec.ReadString(sSecao, 'perRef', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with idePeriodo.New do
                begin
                  perRef := sFim;

                  L := 1;
                  while true do
                  begin
                    // de 01 até 24
                    sSecao := 'ideEstab' + IntToStrZero(I, 2) + IntToStrZero(J, 1) +
                       IntToStrZero(K, 3) + IntToStrZero(L, 2);
                    sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with ideEstab.New do
                    begin
                      tpInsc     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
                      nrInsc     := sFim;

                      M := 1;
                      while true do
                      begin
                        // de 01 até 10
                        sSecao := 'remunPerAnt' + IntToStrZero(I, 2) + IntToStrZero(J, 1) +
                           IntToStrZero(K, 3) + IntToStrZero(L, 2) + IntToStrZero(M, 2);
                        sFim   := INIRec.ReadString(sSecao, 'matricula', 'FIM');

                        if (sFim = 'FIM') or (Length(sFim) <= 0) then
                          break;

                        with remunPerAnt.New do
                        begin
                          matricula := sFim;
                          codCateg  := INIRec.ReadInteger(sSecao, 'codCateg', 0);

                          N := 1;
                          while true do
                          begin
                            // de 001 até 200
                            sSecao := 'itensRemun' + IntToStrZero(I, 2) +
                                        IntToStrZero(J, 1) + IntToStrZero(K, 3) +
                                        IntToStrZero(L, 2) + IntToStrZero(M, 2) +
                                        IntToStrZero(N, 3);
                            sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                            if (sFim = 'FIM') or (Length(sFim) <= 0) then
                              break;

                            with itensRemun.New do
                            begin
                              codRubr    := sFim;
                              ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', '');
                              qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                              fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                              vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                              vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
                            end;

                            Inc(N);
                          end;

                        end;

                        Inc(M);
                      end;

                    end;

                    Inc(L);
                  end;

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

    GerarXML;
  finally
     INIRec.Free;
  end;
end;

{ TS1202CollectionItem }
constructor TS1202CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS1202;
  FEvtRmnRPPS := TEvtRemunRPPS.Create(AOwner);
end;

destructor TS1202CollectionItem.Destroy;
begin
  FEvtRmnRPPS.Free;

  inherited;
end;

{ TS1202Collection }
function TS1202Collection.Add: TS1202CollectionItem;
begin
  Result := Self.New;
end;

function TS1202Collection.GetItem(Index: integer): TS1202CollectionItem;
begin
  Result := TS1202CollectionItem(inherited GetItem(Index));
end;

procedure TS1202Collection.SetItem(Index: integer; Value: TS1202CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TS1202Collection.New: TS1202CollectionItem;
begin
  Result := TS1202CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

end.
