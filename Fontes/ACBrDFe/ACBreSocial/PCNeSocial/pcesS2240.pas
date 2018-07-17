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
  SysUtils, Classes,
  pcnConversao, pcnGerador, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2240Collection = class;
  TS2240CollectionItem = class;
  TEvtExpRisco = class;
  TinfoExpRisco = class;
  TiniExpRisco = class;
  TaltExpRisco = class;
  TfimExpRisco = class;
  TExpRisco = class;
  TRespRegCollection = class;
  TRespRegItem = class;

  TS2240Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2240CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2240CollectionItem);
  public
    function Add: TS2240CollectionItem;
    property Items[Index: Integer]: TS2240CollectionItem read GetItem write SetItem; default;
  end;

  TS2240CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtExpRisco: TEvtExpRisco;
    procedure setEvtExpRisco(const Value: TEvtExpRisco);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtExpRisco: TEvtExpRisco read FEvtExpRisco write setEvtExpRisco;
  end;

  TEvtExpRisco = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FinfoExpRisco: TinfoExpRisco;
    FACBreSocial: TObject;

    procedure GerarInfoExpRisco(objInfoExpRisco: TInfoExpRisco);
    procedure GerariniExpRisco(objiniExpRisco: TiniExpRisco);
    procedure GeraraltExpRisco(objaltExpRisco: TaltExpRisco);
    procedure GerarfimExpRisco(objfimExpRisco: TfimExpRisco);
    procedure GerarInfoAmb(objInfoAmb : TinfoAmbCollection);
    procedure GerarFatRisco(objFatRisco: TFatRiscoCollection);
    procedure GerarEpcEpi(pEpcEpi: TEpcEpi);
    procedure GerarEpc(pEpc: TEpcCollection);
    procedure GerarEPI(objEPI: TEpiCollection);
    procedure GerarRespReg(pRespReg: TRespRegCollection);
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property infoExpRisco: TinfoExpRisco read FinfoExpRisco write FinfoExpRisco;
  end;

  TRespRegCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRespRegItem;
    procedure SetItem(Index: Integer; Value: TRespRegItem);
  public
    constructor create; reintroduce;

    function Add: TRespRegItem;
    property Items[Index: Integer]: TRespRegItem read GetItem write SetItem; default;
  end;

  TRespRegItem = class(TCollectionItem)
  private
    FDtIni: TDateTime;
    FDtFim: TDateTime;
    FNisResp: string;
    FNrOc: string;
    FUfOC: tpuf;
  public
    property dtIni: TDateTime read FDtIni write FDtIni;
    property dtFim: TDateTime read FDtFim write FDtFim;
    property nisResp: string read FNisResp write FNisResp;
    property nrOc: string read FNrOc write FNrOc;
    property ufOC: tpuf read FUfOC write FUfOC;
  end;

  TinfoExpRisco = class
  private
    FiniExpRisco : TiniExpRisco;
    FaltExpRisco : TaltExpRisco;
    FfimExpRisco : TfimExpRisco;
    FRespReg: TRespRegCollection;
  public
    constructor Create;
    destructor  Destroy; override;

    property iniExpRisco: TiniExpRisco read FiniExpRisco write FiniExpRisco;
    property altExpRisco: TaltExpRisco read FaltExpRisco write FaltExpRisco;
    property fimExpRisco: TfimExpRisco read FfimExpRisco write FfimExpRisco;
    property respReg: TRespRegCollection read FRespReg write FRespReg;
  end;

  TExpRisco = class(TPersistent)
  private
    FdtCondicao : TDateTime;
    FInfoAmb : TInfoAmbCollection;

    procedure SetInfoAmb(const Value: TInfoAmbCollection);
  public
    constructor Create;
    destructor  Destroy; override;

    property dtCondicao : TDateTime read FdtCondicao write FdtCondicao;
    property InfoAmb : TInfoAmbCollection read FInfoAmb write SetInfoAmb;
  end;

  TiniExpRisco = class(TExpRisco);

  TaltExpRisco = class(TExpRisco);

  TfimExpRisco = class
  private
    FinfoAmb: TInfoAmbCollection;
    FdtFimCondicao: TDateTime;
    procedure SetinfoAmb(const Value: TInfoAmbCollection);
  public
    constructor create;
    destructor  destroy; override;
    property dtFimCondicao : TDateTime read FdtFimCondicao write FdtFimCondicao;
    property infoAmb : TInfoAmbCollection read FinfoAmb write SetinfoAmb;
  end;

implementation

uses
  IniFiles,
  ACBreSocial, ACBrDFeUtil;

{ TS2240Collection }

function TS2240Collection.Add: TS2240CollectionItem;
begin
  Result := TS2240CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2240Collection.GetItem(Index: Integer): TS2240CollectionItem;
begin
  Result := TS2240CollectionItem(inherited GetItem(Index));
end;

procedure TS2240Collection.SetItem(Index: Integer;
  Value: TS2240CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2240CollectionItem }

constructor TS2240CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS2240;
  FEvtExpRisco := TEvtExpRisco.Create(AOwner);
end;

destructor TS2240CollectionItem.Destroy;
begin
  FEvtExpRisco.Free;

  inherited;
end;

procedure TS2240CollectionItem.setEvtExpRisco(const Value: TEvtExpRisco);
begin
  FEvtExpRisco.Assign(Value);
end;

{ TEvtAltContratual }

constructor TEvtExpRisco.Create(AACBreSocial: TObject);
begin
  inherited;

  FACBreSocial := AACBreSocial;
  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo := TIdeVinculo.Create;
  FInfoExpRisco := TInfoExpRisco.Create;
end;

destructor TEvtExpRisco.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FInfoExpRisco.Free;

  inherited;
end;

procedure TEvtExpRisco.GeraraltExpRisco(objaltExpRisco: TaltExpRisco);
begin
  Gerador.wGrupo('altExpRisco');

  Gerador.wCampo(tcDat, '', 'dtAltCondicao', 10, 10, 1, objAltExpRisco.dtCondicao);

  GerarInfoAmb(objaltExpRisco.InfoAmb);

  Gerador.wGrupo('/altExpRisco');
end;

procedure TEvtExpRisco.GerarEPI(objEPI: TEpiCollection);
var
  i: integer;
begin
  for i := 0 to objEPI.Count -1 do
  begin
    Gerador.wGrupo('epi');

    Gerador.wCampo(tcStr, '', 'caEPI',         1, 20, 0, objEPI[i].caEPI);
    Gerador.wCampo(tcStr, '', 'eficEpi',       1,  1, 1, eSSimNaoToStr(objEPI[i].eficEpi));
    Gerador.wCampo(tcStr, '', 'medProtecao',   1,  1, 1, eSSimNaoToStr(objEPI[i].medProtecao));
    Gerador.wCampo(tcStr, '', 'condFuncto',    1,  1, 1, eSSimNaoToStr(objEPI[i].condFuncto));
    Gerador.wCampo(tcStr, '', 'przValid',      1,  1, 1, eSSimNaoToStr(objEPI[i].przValid));
    Gerador.wCampo(tcStr, '', 'periodicTroca', 1,  1, 1, eSSimNaoToStr(objEPI[i].periodicTroca));
    Gerador.wCampo(tcStr, '', 'higienizacao',  1,  1, 1, eSSimNaoToStr(objEPI[i].higienizacao));

    Gerador.wGrupo('/epi');
  end;

  if objEPI.Count > 50 then
    Gerador.wAlerta('', 'epi', 'Lista de EPI', ERR_MSG_MAIOR_MAXIMO + '50');
end;

procedure TEvtExpRisco.GerarEpc(pEpc: TEpcCollection);
var
  i: integer;
begin
  for i := 0 to pEpc.Count - 1 do
  begin
    Gerador.wGrupo('epc');

    Gerador.wCampo(tcStr, '', 'dscEpc',  1, 70, 1, pEpc[i].dscEpc);
    Gerador.wCampo(tcStr, '', 'eficEpc', 1,  1, 0, eSSimNaoToStr(pEpc[i].eficEpc));

    Gerador.wGrupo('/epc');
  end;

  if pEpc.Count > 50 then
    Gerador.wAlerta('', 'epc', 'Lista de EPC', ERR_MSG_MAIOR_MAXIMO + '50');
end;

procedure TEvtExpRisco.GerarEpcEpi(pEpcEpi: TEpcEpi);
begin
  Gerador.wGrupo('epcEpi');

  Gerador.wCampo(tcInt, '', 'utilizEPC', 1, 1, 1, eStpUtilizEPCToStr(pEpcEpi.utilizEPC));
  Gerador.wCampo(tcInt, '', 'utilizEPI', 1, 1, 1, eStpUtilizEPIToStr(pEpcEpi.utilizEPI));

  if pEpcEpi.epcInst then
    GerarEpc(pEpcEpi.epc);

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

    Gerador.wCampo(tcStr, '', 'codFatRis',  1, 10, 1, objFatRisco.Items[i].codFatRis);
    Gerador.wCampo(tcStr, '', 'intConc',    1, 15, 0, objFatRisco.Items[i].intConc);
    Gerador.wCampo(tcStr, '', 'tecMedicao', 1, 40, 0, objFatRisco.Items[i].tecMedicao);

    GerarEpcEpi(objFatRisco.Items[i].epcEpi);

    Gerador.wGrupo('/fatRisco');
  end;

  if objFatRisco.Count > 999 then
    Gerador.wAlerta('', 'fatRisco', 'Lista de Fatores de Riscos', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtExpRisco.GerarfimExpRisco(objfimExpRisco: TfimExpRisco);
var
  i: Integer;
begin
  Gerador.wGrupo('fimExpRisco');

  Gerador.wCampo(tcDat, '', 'dtFimCondicao', 10, 10, 1, objfimExpRisco.dtFimCondicao);

  for i := 0 to objfimExpRisco.infoAmb.Count - 1 do
  begin
    Gerador.wGrupo('infoAmb');

    Gerador.wCampo(tcStr, '', 'codAmb', 1, 30, 0, objfimExpRisco.InfoAmb.items[i].codAmb);

    Gerador.wGrupo('/infoAmb');
  end;

  if objfimExpRisco.infoAmb.Count > 99 then
    Gerador.wAlerta('', 'infoAmb', 'Lista de Informações Ambientais', ERR_MSG_MAIOR_MAXIMO + '99');

  Gerador.wGrupo('/fimExpRisco');
end;

procedure TEvtExpRisco.GerarInfoAmb(objInfoAmb: TinfoAmbCollection);
var
  j: integer;
begin
  for j := 0 to objInfoAmb.Count - 1 do
  begin
    Gerador.wGrupo('infoAmb');

    Gerador.wCampo(tcStr, '', 'codAmb', 1, 30, 1, objInfoAmb.items[j].codAmb);

    Gerador.wGrupo('infoAtiv');

    Gerador.wCampo(tcStr, '', 'dscAtivDes', 1, 999, 1, objInfoAmb.items[j].InfoAtiv.dscAtivDes);

    Gerador.wGrupo('/infoAtiv');

    GerarFatRisco(objInfoAmb.items[j].FatRisco);

    Gerador.wGrupo('/infoAmb');
  end;

  if objInfoAmb.Count > 99 then
    Gerador.wAlerta('', 'infoAmb', 'Lista de Informações Ambientais', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtExpRisco.GerarRespReg(pRespReg: TRespRegCollection);
var
  i: integer;
begin
  for i := 0 to pRespReg.Count - 1 do
  begin
    Gerador.wGrupo('respReg');

    Gerador.wCampo(tcDat, '', 'dtIni',   10, 10, 1, pRespReg[i].dtIni);
    Gerador.wCampo(tcDat, '', 'dtFim',   10, 10, 0, pRespReg[i].dtFim);
    Gerador.wCampo(tcStr, '', 'nisResp',  1, 11, 1, pRespReg[i].nisResp);
    Gerador.wCampo(tcStr, '', 'nrOc',     1, 14, 1, pRespReg[i].nrOc);
    Gerador.wCampo(tcStr, '', 'ufOC',     2,  2, 0, eSufToStr(pRespReg[i].ufOC));

    Gerador.wGrupo('/respReg');
  end;

  if pRespReg.Count > 9 then
    Gerador.wAlerta('', 'respReg', 'Lista de Responsáveis pelo registro', ERR_MSG_MAIOR_MAXIMO + '9');
end;

procedure TEvtExpRisco.GerarInfoExpRisco(objInfoExpRisco: TInfoExpRisco);
begin
  Gerador.wGrupo('infoExpRisco');

  if (objInfoExpRisco.iniExpRisco.dtCondicao > 0) then
    GerariniExpRisco(objInfoExpRisco.iniExpRisco);

  if (objInfoExpRisco.altExpRisco.dtCondicao > 0) then
    GeraraltExpRisco(objInfoExpRisco.altExpRisco);

  if (objInfoExpRisco.fimExpRisco.dtFimCondicao > 0) then
    GerarfimExpRisco(objInfoExpRisco.fimExpRisco);

  GerarRespReg(objInfoExpRisco.respReg);

  Gerador.wGrupo('/infoExpRisco');
end;

procedure TEvtExpRisco.GerariniExpRisco(objiniExpRisco: TiniExpRisco);
begin
  Gerador.wGrupo('iniExpRisco');

  Gerador.wCampo(tcDat, '', 'dtIniCondicao', 10, 10, 1, objiniExpRisco.dtCondicao);

  GerarInfoAmb(objiniExpRisco.InfoAmb);

  Gerador.wGrupo('/iniExpRisco');
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

{ TExpRisco }

constructor TExpRisco.create;
begin
  inherited;

  FInfoAmb := TInfoAmbCollection.Create;
end;

destructor TExpRisco.destroy;
begin
  FinfoAmb.Free;

  inherited;
end;

procedure TExpRisco.SetInfoAmb(const Value: TInfoAmbCollection);
begin
  FInfoAmb.Assign(Value);
end;

{ TfimExpRisco }

constructor TfimExpRisco.create;
begin
  inherited;

  FinfoAmb := TInfoAmbCollection.Create;
end;

destructor TfimExpRisco.destroy;
begin
  FInfoAmb.Free;

  inherited;
end;

procedure TfimExpRisco.SetinfoAmb(const Value: TInfoAmbCollection);
begin
  FinfoAmb := Value;
end;

{ TinfoExpRisco }

constructor TinfoExpRisco.create;
begin
  inherited;

  FiniExpRisco := TiniExpRisco.Create;
  FaltExpRisco := TaltExpRisco.Create;
  FfimExpRisco := TfimExpRisco.Create;
  FRespReg := TRespRegCollection.Create;
end;

destructor TinfoExpRisco.destroy;
begin
  FiniExpRisco.Free;
  FaltExpRisco.Free;
  FfimExpRisco.Free;
  FRespReg.Free;

  inherited;
end;

{ TRespRegCollection }

constructor TRespRegCollection.Create;
begin
  Inherited create(TRespRegItem);
end;

function TRespRegCollection.Add: TRespRegItem;
begin
  Result := TRespRegItem(inherited Add);
//  Result.Create;
end;

function TRespRegCollection.GetItem(Index: Integer): TRespRegItem;
begin
  Result := TRespRegItem(inherited GetItem(Index));
end;

procedure TRespRegCollection.SetItem(Index: Integer;
  Value: TRespRegItem);
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
  Result := False;

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
      ideEvento.TpAmb       := eSStrTotpAmb(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
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
        infoExpRisco.iniExpRisco.dtCondicao     := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniCondicao', '0'));

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoAmb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'codAmb', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoExpRisco.iniExpRisco.infoAmb.Add do
          begin
            codAmb := sFim;

            sSecao := 'infoAtiv' + IntToStrZero(I, 2);
            infoAtiv.dscAtivDes := INIRec.ReadString(sSecao, 'dscAtivDes', EmptyStr);

            J := 1;
            while true do
            begin
              // de 001 até 999
              sSecao := 'fatRisco' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'codFatRis', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with fatRisco.Add do
              begin
                codFatRis  := sFim;
                intConc    := INIRec.ReadString(sSecao, 'intConc', EmptyStr);
                tecMedicao := INIRec.ReadString(sSecao, 'tecMedicao', EmptyStr);

                epcEpi.utilizEPC := eSStrTotpUtilizEPC(Ok, INIRec.ReadString(sSecao, 'utilizEPC', '0'));
                epcEpi.utilizEPI := eSStrTotpUtilizEPI(Ok, INIRec.ReadString(sSecao, 'utilizEPI', '0'));

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

                K := 1;
                while true do
                begin
                  // de 00 até 50
                  sSecao := 'epi' + IntToStrZero(I, 2) + IntToStrZero(J, 3) +
                               IntToStrZero(K, 2);
                  sFim   := INIRec.ReadString(sSecao, 'caEPI', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with epcEpi.epi.Add do
                  begin
                    caEPI         := sFim;
                    eficEpi       := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'eficEpi', 'S'));
                    medProtecao   := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'medProtecao', 'S'));
                    condFuncto    := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'condFuncto', 'S'));
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

      sSecao := 'altExpRisco';
      if INIRec.ReadString(sSecao, 'dtAltCondicao', '') <> '' then
      begin
        infoExpRisco.altExpRisco.dtCondicao := StringToDateTime(INIRec.ReadString(sSecao, 'dtAltCondicao', '0'));

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoAmb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'codAmb', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoExpRisco.iniExpRisco.infoAmb.Add do
          begin
            codAmb := sFim;

            sSecao := 'infoAtiv' + IntToStrZero(I, 2);
            infoAtiv.dscAtivDes := INIRec.ReadString(sSecao, 'dscAtivDes', EmptyStr);

            J := 1;
            while true do
            begin
              // de 001 até 999
              sSecao := 'fatRisco' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'codFatRis', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with fatRisco.Add do
              begin
                codFatRis  := sFim;
                intConc    := INIRec.ReadString(sSecao, 'intConc', EmptyStr);
                tecMedicao := INIRec.ReadString(sSecao, 'tecMedicao', EmptyStr);

                epcEpi.utilizEPC := eSStrTotpUtilizEPC(Ok, INIRec.ReadString(sSecao, 'utilizEPC', '0'));
                epcEpi.utilizEPI := eSStrTotpUtilizEPI(Ok, INIRec.ReadString(sSecao, 'utilizEPI', '0'));

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

                K := 1;
                while true do
                begin
                  // de 00 até 50
                  sSecao := 'epi' + IntToStrZero(I, 2) + IntToStrZero(J, 3) +
                               IntToStrZero(K, 2);
                  sFim   := INIRec.ReadString(sSecao, 'caEPI', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with epcEpi.epi.Add do
                  begin
                    caEPI         := sFim;
                    eficEpi       := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'eficEpi', 'S'));
                    medProtecao   := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'medProtecao', 'S'));
                    condFuncto    := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'condFuncto', 'S'));
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

      sSecao := 'fimExpRisco';
      if INIRec.ReadString(sSecao, 'dtFimCondicao', '') <> '' then
      begin
        infoExpRisco.fimExpRisco.dtFimCondicao := StringToDateTime(INIRec.ReadString(sSecao, 'dtFimCondicao', '0'));

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoAmb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'codAmb', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoExpRisco.fimExpRisco.infoAmb.Add do
          begin
            codAmb := sFim;
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

        with infoExpRisco.respReg.Add do
        begin
          dtIni   := StringToDateTime(sFim);
          dtFim   := StringToDateTime(INIRec.ReadString(sSecao, 'dtFim', '0'));
          nisResp := INIRec.ReadString(sSecao, 'nisResp', EmptyStr);
          nrOc    := INIRec.ReadString(sSecao, 'nrOc', EmptyStr);
          ufOC    := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'ufOC', 'SP'));
        end;

        Inc(I);
      end;

    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

end.
