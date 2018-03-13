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
|*  - Passado o namespace para geração no cabeçalho
******************************************************************************}
{$I ACBr.inc}

unit pcesS1020;

interface

uses
  SysUtils, Classes, ACBrUtil,
  pcnConversao, pcnGerador,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1020Collection = class;
  TS1020CollectionItem = class;
  TevtTabLotacao = class;
  TIdeLotacao = class;
  TFPasLotacao = class;
  TInfoEmprParcial = class;
  TDadosLotacao = class;
  TInfoLotacao = class;
  TProcJudTerceiroCollectionItem = class;
  TProcJudTerceiroCollection = class;
  TInfoProcJudTerceiros = class;

  TS1020Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS1020CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1020CollectionItem);
  public
    function Add: TS1020CollectionItem;
    property Items[Index: Integer]: TS1020CollectionItem read GetItem write SetItem; default;
  end;

  TS1020CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabLotacao: TevtTabLotacao;
    procedure setevtTabLotacao(const Value: TevtTabLotacao);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabLotacao: TevtTabLotacao read FEvtTabLotacao write setevtTabLotacao;
  end;

  TInfoProcJudTerceiros = class(TPersistent)
  private
    FProcJudTerceiro: TProcJudTerceiroCollection;
  public
    constructor create;
    destructor Destroy; override;

    property procJudTerceiro: TProcJudTerceiroCollection read FProcJudTerceiro write FProcJudTerceiro;
  end;

  TProcJudTerceiroCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TProcJudTerceiroCollectionItem;
    procedure SetItem(Index: Integer; Value: TProcJudTerceiroCollectionItem);
  public
    constructor create(); reintroduce;

    function Add: TProcJudTerceiroCollectionItem;
    property Items[Index: Integer]: TProcJudTerceiroCollectionItem read GetItem write SetItem;
  end;

  TProcJudTerceiroCollectionItem = class(TProcesso)
  private
    FCodTerc: string;
  public
    constructor create; reintroduce;

    property codTerc: string read FCodTerc write FCodTerc;
    property nrProcJud: string read FNrProc write FNrProc;
  end;

  TevtTabLotacao = class(TeSocialEvento)
  private
    FModoLancamento: TModoLancamento;
    fIdeEvento: TIdeEvento;
    fIdeEmpregador: TIdeEmpregador;
    fInfoLotacao: TInfoLotacao;
    FACBreSocial: TObject;

    {Geradores específicos da classe}
    procedure GerarIdeLotacao;
    procedure GerarInfoEmprParcial;
    procedure GerarInfoProcJudTerceiros;
    procedure GerarFPasLotacao;
    procedure GerarDadosLotacao;
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property IdeEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FideEmpregador write FideEmpregador;
    property infoLotacao: TInfoLotacao read fInfoLotacao write fInfoLotacao;
  end;

  TIdeLotacao = class(TPersistent)
  private
    FCodLotacao: string;
    FIniValid: string;
    FFimValid: string;
  public
    property codLotacao: string read fCodLotacao write fCodLotacao;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TFPasLotacao = class(TPersistent)
  private
    fFpas: string;
    FCodTercs: string;
    FCodTercsSusp: string;
    FInfoProcJudTerceiros: TInfoProcJudTerceiros;

    function getInfoProcJudTerceiros(): TInfoProcJudTerceiros;
  public
    constructor create;
    destructor Destroy; override;

    function infoProcJudTerceirosInst(): Boolean;

    property Fpas: string read fFpas write fFpas;
    property codTercs: string read FCodTercs write FCodTercs;
    property codTercsSusp: string read FCodTercsSusp write FCodTercsSusp;
    property infoProcJudTerceiros: TInfoProcJudTerceiros read getInfoProcJudTerceiros write FInfoProcJudTerceiros;
  end;

  TInfoEmprParcial = class(TPersistent)
  private
    FTpInscContrat: TptpInscContratante;
    FNrInscContrat: string;
    FTpInscProp: TpTpInscProp;
    FNrInscProp: string;
  public
    property tpInscContrat: TptpInscContratante read FTpInscContrat write FTpInscContrat;
    property NrInscContrat: string read FNrInscContrat write FNrInscContrat;
    property tpInscProp: TpTpInscProp read FTpInscProp write FTpInscProp;
    property nrInscProp: string read FNrInscProp write FNrInscProp;
  end;

  TDadosLotacao = class(TPersistent)
  private
    FTpLotacao: string;
    FTpInsc: tpTpInsc;
    FNrInsc: string;
    fFPasLotacao: TFPasLotacao;
    fInfoEmprParcial: TinfoEmprParcial;
  public
    constructor create;
    destructor Destroy; override;

    property tpLotacao: string read FTpLotacao write FTpLotacao;
    property tpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property fPasLotacao: TFPasLotacao read ffPasLotacao write ffPasLotacao;
    property infoEmprParcial: TInfoEmprParcial read fInfoEmprParcial write fInfoEmprParcial;
  end;

  TInfoLotacao = class(TPersistent)
  private
    fIdeLotacao: TIdeLotacao;
    fDadosLotacao: TDadosLotacao;
    fNovaValidade: TidePeriodo;

    function getDadosLotacao(): TDadosLotacao;
    function getNovaValidade(): TIdePeriodo;
  public
    constructor create;
    destructor Destroy; override;
    
    function ideLotacaoInst(): Boolean;
    function dadosLotacaoInst(): Boolean;
    function novaValidadeInst(): Boolean;

    property ideLotacao: TIdeLotacao read fIdeLotacao write fIdeLotacao;
    property dadosLotacao: TDadosLotacao read getDadosLotacao write fDadosLotacao;
    property novaValidade: TidePeriodo read getNovaValidade write fNovaValidade;
  end;

implementation

uses
  IniFiles,
  ACBreSocial, ACBrDFeUtil;

{ TS1020Collection }

function TS1020Collection.Add: TS1020CollectionItem;
begin
  Result := TS1020CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS1020Collection.GetItem(Index: Integer): TS1020CollectionItem;
begin
  Result := TS1020CollectionItem(inherited GetItem(Index));
end;

procedure TS1020Collection.SetItem(Index: Integer;
  Value: TS1020CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS1020CollectionItem }

constructor TS1020CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS1020;
  FEvtTabLotacao := TevtTabLotacao.Create(AOwner);
end;

destructor TS1020CollectionItem.Destroy;
begin
  FEvtTabLotacao.Free;
  inherited;
end;

procedure TS1020CollectionItem.setevtTabLotacao(
  const Value: TevtTabLotacao);
begin
  FEvtTabLotacao.Assign(Value);
end;

{ TevtTabLotacao }

constructor TevtTabLotacao.Create(AACBreSocial: TObject);
begin
  inherited;

  FACBreSocial := AACBreSocial;
  fIdeEvento := TIdeEvento.Create;
  fIdeEmpregador := TIdeEmpregador.Create;
  fInfoLotacao := TInfoLotacao.Create;
end;

destructor TevtTabLotacao.destroy;
begin
  fIdeEvento.Free;
  fIdeEmpregador.Free;
  fInfoLotacao.Free;

  inherited;
end;

procedure TevtTabLotacao.GerarDadosLotacao;
begin
  Gerador.wGrupo('dadosLotacao');

  Gerador.wCampo(tcStr, '', 'tpLotacao', 2, 2, 1, self.infoLotacao.DadosLotacao.tpLotacao); // Criar enumerador

  if (StrToInt(self.infoLotacao.DadosLotacao.tpLotacao) in [2, 3, 4, 5, 6, 7, 8, 9]) then
    Gerador.wCampo(tcStr, '', 'tpInsc', 1, 1, 0, ord(self.infoLotacao.DadosLotacao.tpInsc) + 1);

  Gerador.wCampo(tcStr, '', 'nrInsc', 1, 15, 0, self.infoLotacao.DadosLotacao.nrInsc);

  GerarFPasLotacao;
  GerarInfoEmprParcial;

  Gerador.wGrupo('/dadosLotacao');
end;

procedure TevtTabLotacao.GerarFPasLotacao;
begin
  Gerador.wGrupo('fpasLotacao');

  Gerador.wCampo(tcStr, '', 'fpas',         1, 3, 1, self.infoLotacao.DadosLotacao.fPasLotacao.Fpas);
  Gerador.wCampo(tcStr, '', 'codTercs',     1, 4, 1, self.infoLotacao.DadosLotacao.fPasLotacao.codTercs);
  Gerador.wCampo(tcStr, '', 'codTercsSusp', 1, 4, 0, self.infoLotacao.DadosLotacao.fPasLotacao.codTercsSusp);

  GerarInfoProcJudTerceiros;

  Gerador.wGrupo('/fpasLotacao');
end;

procedure TevtTabLotacao.GerarIdeLotacao;
begin
  Gerador.wGrupo('ideLotacao');

  Gerador.wCampo(tcStr, '', 'codLotacao', 1, 30, 1, self.infoLotacao.IdeLotacao.codLotacao);
  Gerador.wCampo(tcStr, '', 'iniValid',   7,  7, 1, self.infoLotacao.IdeLotacao.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid',   7,  7, 0, self.infoLotacao.IdeLotacao.fimValid);

  Gerador.wGrupo('/ideLotacao');
end;

procedure TevtTabLotacao.GerarInfoEmprParcial;
begin
  if infoLotacao.DadosLotacao.InfoEmprParcial.nrInscContrat <> '' then
  begin
    Gerador.wGrupo('infoEmprParcial');

    Gerador.wCampo(tcStr, '', 'tpInscContrat', 1,  1, 1, eStpInscContratanteToStr(infoLotacao.DadosLotacao.InfoEmprParcial.tpInscContrat));
    Gerador.wCampo(tcStr, '', 'nrInscContrat', 1, 14, 1, infoLotacao.DadosLotacao.InfoEmprParcial.nrInscContrat);
    Gerador.wCampo(tcStr, '', 'tpInscProp',    1,  1, 1, eSTpInscPropToStr(infoLotacao.DadosLotacao.InfoEmprParcial.tpInscProp));
    Gerador.wCampo(tcStr, '', 'nrInscProp',    1, 14, 1, infoLotacao.DadosLotacao.InfoEmprParcial.nrInscProp);

    Gerador.wGrupo('/infoEmprParcial');
  end;
end;

procedure TevtTabLotacao.GerarInfoProcJudTerceiros;
var
  i: Integer;
  objProcJudTer: TProcJudTerceiroCollectionItem;
begin
  if (infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Count > 0) then
  begin
    objProcJudTer := infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Items[0];

    if objProcJudTer.codTerc <> EmptyStr then
    begin
      Gerador.wGrupo('infoProcJudTerceiros');

      for i := 1 to infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Count - 1 do
      begin
        objProcJudTer := infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Items[i];

        Gerador.wGrupo('procJudTerceiro');

        Gerador.wCampo(tcStr, '', 'codTerc',   1,  4, 1, objProcJudTer.codTerc);
        Gerador.wCampo(tcStr, '', 'nrProcJud', 1, 20, 1, objProcJudTer.nrProcJud);

        if trim(objProcJudTer.codSusp) <> '' then
          Gerador.wCampo(tcInt, '', 'codSusp', 1, 14, 1, objProcJudTer.codSusp);

        Gerador.wGrupo('/procJudTerceiro');
      end;

      if infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Count > 99 then
        Gerador.wAlerta('', 'infoProcJudTerceiros', 'Lista de Processos Judic. Terceiros', ERR_MSG_MAIOR_MAXIMO + '99');

      Gerador.wGrupo('/infoProcJudTerceiros');
    end;
  end;
end;

function TevtTabLotacao.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTabLotacao');
    Gerador.wGrupo('evtTabLotacao Id="' + Self.Id + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.ideEmpregador);

    Gerador.wGrupo('infoLotacao');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeLotacao;

    if Self.ModoLancamento <> mlExclusao then
    begin
      GerarDadosLotacao;

      if Self.ModoLancamento = mlAlteracao then
        if (infoLotacao.novaValidadeInst()) then
          GerarIdePeriodo(self.infoLotacao.NovaValidade, 'novaValidade');
    end;

    GerarModoFechamento(ModoLancamento);

    Gerador.wGrupo('/infoLotacao');
    Gerador.wGrupo('/evtTabLotacao');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabLotacao');

    Validar(schevtTabLotacao);
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TevtTabLotacao.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtTabLotacao';
      Sequencial     := INIRec.ReadInteger(sSecao, 'Sequencial', 0);
      ModoLancamento := eSStrToModoLancamento(Ok, INIRec.ReadString(sSecao, 'ModoLancamento', 'inclusao'));

      sSecao := 'ideEvento';
      ideEvento.TpAmb   := eSStrTotpAmb(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
      ideEvento.ProcEmi := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideLotacao';
      infoLotacao.ideLotacao.codLotacao := INIRec.ReadString(sSecao, 'codLotacao', EmptyStr);
      infoLotacao.ideLotacao.IniValid   := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoLotacao.ideLotacao.FimValid   := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if (ModoLancamento <> mlExclusao) then
      begin
        sSecao := 'dadosLotacao';
        infoLotacao.dadosLotacao.tpLotacao := INIRec.ReadString(sSecao, 'tpLotacao', EmptyStr);
        infoLotacao.dadosLotacao.tpInsc    := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
        infoLotacao.dadosLotacao.nrInsc    := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

        sSecao := 'fpasLotacao';
        infoLotacao.dadosLotacao.fpasLotacao.fpas         := INIRec.ReadString(sSecao, 'fpas', EmptyStr);
        infoLotacao.dadosLotacao.fpasLotacao.codTercs     := INIRec.ReadString(sSecao, 'codTercs', EmptyStr);
        infoLotacao.dadosLotacao.fpasLotacao.codTercsSusp := INIRec.ReadString(sSecao, 'codTercsSusp', EmptyStr);

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'procJudTerceiro' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'codTerc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoLotacao.dadosLotacao.fpasLotacao.infoProcJudTerceiros.procJudTerceiro.Add do
          begin
            codTerc   := sFim;
            nrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', '');
            codSusp   := INIRec.ReadString(sSecao, 'codSusp', '');
          end;

          Inc(I);
        end;

        sSecao := 'infoEmprParcial';
        infoLotacao.dadosLotacao.infoEmprParcial.tpInscContrat := eSStrTotpInscContratante(Ok, INIRec.ReadString(sSecao, 'tpInscContrat', '1'));
        infoLotacao.dadosLotacao.infoEmprParcial.nrInscContrat := INIRec.ReadString(sSecao, 'nrInscContrat', EmptyStr);
        infoLotacao.dadosLotacao.infoEmprParcial.tpInscProp    := eSStrToTpInscProp(Ok, INIRec.ReadString(sSecao, 'tpInscProp', '1'));
        infoLotacao.dadosLotacao.infoEmprParcial.nrInscProp    := INIRec.ReadString(sSecao, 'nrInscProp', EmptyStr);

        if ModoLancamento = mlAlteracao then
        begin
          sSecao := 'novaValidade';
          infoLotacao.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoLotacao.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
        end;
      end;
    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

{ TDadosLotacao }

constructor TDadosLotacao.create;
begin
  fFPasLotacao := TFPasLotacao.Create;
  fInfoEmprParcial := TinfoEmprParcial.Create;
end;

destructor TDadosLotacao.destroy;
begin
  fFPasLotacao.Free;
  FinfoEmprParcial.Free;

  inherited;
end;

{ TInfoLotacao }

constructor TInfoLotacao.create;
begin
  fIdeLotacao := TIdeLotacao.Create;
  fDadosLotacao := nil;
  fNovaValidade := nil;
end;

function TInfoLotacao.dadosLotacaoInst: Boolean;
begin
  Result := Assigned(fDadosLotacao);
end;

destructor TInfoLotacao.destroy;
begin
  fIdeLotacao.Free;
  FreeAndNil(fDadosLotacao);
  FreeAndNil(fNovaValidade);

  inherited;
end;

function TInfoLotacao.getDadosLotacao: TDadosLotacao;
begin  
  if Not(Assigned(fDadosLotacao)) then
    fDadosLotacao := TDadosLotacao.create;
  Result := fDadosLotacao;
end;

function TInfoLotacao.getNovaValidade: TIdePeriodo;
begin                             
  if Not(Assigned(fNovaValidade)) then
    fNovaValidade := TIdePeriodo.Create;
  Result := fNovaValidade;
end;

function TInfoLotacao.ideLotacaoInst: Boolean;
begin                               
  Result := Assigned(fIdeLotacao);
end;

function TInfoLotacao.novaValidadeInst: Boolean;
begin     
  Result := Assigned(fNovaValidade);
end;

{ TProcJudTerceiroCollectionItem }

constructor TProcJudTerceiroCollectionItem.create;
begin

end;

{ TProcJudTerceiroCollection }

function TProcJudTerceiroCollection.Add: TProcJudTerceiroCollectionItem;
begin
  Result := TProcJudTerceiroCollectionItem(inherited Add());
  Result.Create;
end;

constructor TProcJudTerceiroCollection.create;
begin
  inherited create(TProcJudTerceiroCollectionItem);
end;

function TProcJudTerceiroCollection.GetItem(
  Index: Integer): TProcJudTerceiroCollectionItem;
begin
  Result := TProcJudTerceiroCollectionItem(Inherited GetItem(Index));
end;

procedure TProcJudTerceiroCollection.SetItem(Index: Integer;
  Value: TProcJudTerceiroCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

{ TInfoProcJudTerceiros }

constructor TInfoProcJudTerceiros.create;
begin
  FProcJudTerceiro := TProcJudTerceiroCollection.create;
  FProcJudTerceiro.Add();
end;

destructor TInfoProcJudTerceiros.destroy;
begin
  FProcJudTerceiro.Free;

  inherited;
end;

{ TFPasLotacao }

constructor TFPasLotacao.create;
begin
  FInfoProcJudTerceiros := nil;
end;

destructor TFPasLotacao.destroy;
begin
  FreeAndNil(FInfoProcJudTerceiros);

  inherited;
end;

function TFPasLotacao.getInfoProcJudTerceiros: TInfoProcJudTerceiros;
begin
  if Not(Assigned(FInfoProcJudTerceiros)) then
    FInfoProcJudTerceiros := TInfoProcJudTerceiros.create;
  Result := FInfoProcJudTerceiros;
end;

function TFPasLotacao.infoProcJudTerceirosInst: Boolean;
begin
  Result := Assigned(FInfoProcJudTerceiros);
end;

end.
