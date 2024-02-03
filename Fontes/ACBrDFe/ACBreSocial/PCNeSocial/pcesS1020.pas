{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Jean Carlo Cantu                                }
{                              Tiago Ravache                                   }
{                              Guilherme Costa                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit pcesS1020;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  ACBrDFeConsts,
  pcnConversao, pcnGerador,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1020Collection = class;
  TS1020CollectionItem = class;
  TevtTabLotacao = class;
  TIdeLotacao = class;
  TFPasLotacao = class;
  TInfoEmprParcial = class;
  TdadosOpPort = class;
  TDadosLotacao = class;
  TInfoLotacao = class;
  TProcJudTerceiroCollectionItem = class;
  TProcJudTerceiroCollection = class;
  TInfoProcJudTerceiros = class;

  TS1020Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1020CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1020CollectionItem);
  public
    function Add: TS1020CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1020CollectionItem;
    property Items[Index: Integer]: TS1020CollectionItem read GetItem write SetItem; default;
  end;

  TS1020CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabLotacao: TevtTabLotacao;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabLotacao: TevtTabLotacao read FEvtTabLotacao write FEvtTabLotacao;
  end;

  TInfoProcJudTerceiros = class(TObject)
  private
    FProcJudTerceiro: TProcJudTerceiroCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property procJudTerceiro: TProcJudTerceiroCollection read FProcJudTerceiro write FProcJudTerceiro;
  end;

  TProcJudTerceiroCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TProcJudTerceiroCollectionItem;
    procedure SetItem(Index: Integer; Value: TProcJudTerceiroCollectionItem);
  public
    function Add: TProcJudTerceiroCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TProcJudTerceiroCollectionItem;
    property Items[Index: Integer]: TProcJudTerceiroCollectionItem read GetItem write SetItem;
  end;

  TProcJudTerceiroCollectionItem = class(TProcesso)
  private
    FCodTerc: string;
  public
    property codTerc: string read FCodTerc write FCodTerc;
    property nrProcJud: string read FNrProc write FNrProc;
  end;

  TevtTabLotacao = class(TeSocialEvento)
  private
    FModoLancamento: TModoLancamento;
    fIdeEvento: TIdeEvento;
    fIdeEmpregador: TIdeEmpregador;
    fInfoLotacao: TInfoLotacao;

    {Geradores específicos da classe}
    procedure GerarIdeLotacao;
    procedure GerarInfoEmprParcial;
    procedure GerarDadosOpPort;
    procedure GerarInfoProcJudTerceiros;
    procedure GerarFPasLotacao;
    procedure GerarDadosLotacao;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property IdeEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FideEmpregador write FideEmpregador;
    property infoLotacao: TInfoLotacao read fInfoLotacao write fInfoLotacao;
  end;

  TIdeLotacao = class(TObject)
  private
    FCodLotacao: string;
    FIniValid: string;
    FFimValid: string;
  public
    property codLotacao: string read fCodLotacao write fCodLotacao;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TFPasLotacao = class(TObject)
  private
    fFpas: string;
    FCodTercs: string;
    FCodTercsSusp: string;
    FInfoProcJudTerceiros: TInfoProcJudTerceiros;

    function getInfoProcJudTerceiros(): TInfoProcJudTerceiros;
  public
    constructor Create;
    destructor Destroy; override;

    function infoProcJudTerceirosInst(): Boolean;

    property Fpas: string read fFpas write fFpas;
    property codTercs: string read FCodTercs write FCodTercs;
    property codTercsSusp: string read FCodTercsSusp write FCodTercsSusp;
    property infoProcJudTerceiros: TInfoProcJudTerceiros read getInfoProcJudTerceiros write FInfoProcJudTerceiros;
  end;

  TInfoEmprParcial = class(TObject)
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

  TdadosOpPort = class(TObject)
  private
    FaliqRat: tpAliqRat;
    Ffap: Double;
  public
    property aliqRat: tpAliqRat read FaliqRat write FaliqRat;
    property fap: Double read Ffap write Ffap;
  end;

  TDadosLotacao = class(TObject)
  private
    FTpLotacao: string;
    FTpInsc: tpTpInsc;
    FNrInsc: string;
    fFPasLotacao: TFPasLotacao;
    fInfoEmprParcial: TinfoEmprParcial;
    fdadosOpPort: TdadosOpPort;
  public
    constructor Create;
    destructor Destroy; override;

    property tpLotacao: string read FTpLotacao write FTpLotacao;
    property tpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property fPasLotacao: TFPasLotacao read ffPasLotacao write ffPasLotacao;
    property infoEmprParcial: TInfoEmprParcial read fInfoEmprParcial write fInfoEmprParcial;
    property dadosOpPort: TdadosOpPort read fdadosOpPort write fdadosOpPort;
  end;

  TInfoLotacao = class(TObject)
  private
    fIdeLotacao: TIdeLotacao;
    fDadosLotacao: TDadosLotacao;
    fNovaValidade: TidePeriodo;

    function getDadosLotacao(): TDadosLotacao;
    function getNovaValidade(): TIdePeriodo;
  public
    constructor Create;
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
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TS1020Collection }

function TS1020Collection.Add: TS1020CollectionItem;
begin
  Result := Self.New;
end;

function TS1020Collection.GetItem(Index: Integer): TS1020CollectionItem;
begin
  Result := TS1020CollectionItem(inherited Items[Index]);
end;

procedure TS1020Collection.SetItem(Index: Integer;
  Value: TS1020CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1020Collection.New: TS1020CollectionItem;
begin
  Result := TS1020CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS1020CollectionItem }

constructor TS1020CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS1020;
  FEvtTabLotacao := TevtTabLotacao.Create(AOwner);
end;

destructor TS1020CollectionItem.Destroy;
begin
  FEvtTabLotacao.Free;
  inherited;
end;

{ TevtTabLotacao }

constructor TevtTabLotacao.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  fIdeEvento     := TIdeEvento.Create;
  fIdeEmpregador := TIdeEmpregador.Create;
  fInfoLotacao   := TInfoLotacao.Create;
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

  if VersaoDF > ve02_05_00 then
    GerarDadosOpPort;
    
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

    if VersaoDF >= veS01_00_00 then
    begin
      if infoLotacao.DadosLotacao.InfoEmprParcial.nrInscProp <> '' then
      begin
        Gerador.wCampo(tcStr, '', 'tpInscProp', 1,  1, 1, eSTpInscPropToStr(self.infoLotacao.DadosLotacao.InfoEmprParcial.tpInscProp));
        Gerador.wCampo(tcStr, '', 'nrInscProp', 1, 14, 1, infoLotacao.DadosLotacao.InfoEmprParcial.nrInscProp);
      end;
    end
    else
    begin
      Gerador.wCampo(tcStr, '', 'tpInscProp', 1,  1, 1, eSTpInscPropToStr(self.infoLotacao.DadosLotacao.InfoEmprParcial.tpInscProp));
      Gerador.wCampo(tcStr, '', 'nrInscProp', 1, 14, 1, infoLotacao.DadosLotacao.InfoEmprParcial.nrInscProp);
    end;

    Gerador.wGrupo('/infoEmprParcial');
  end;
end;

procedure TevtTabLotacao.GerarDadosOpPort;
begin
  if infoLotacao.dadosLotacao.dadosOpPort.fap > 0 then
  begin
    Gerador.wGrupo('dadosOpPort');

    Gerador.wCampo(tcStr, '', 'aliqRat', 1, 1, 1, eSAliqRatToStr(infoLotacao.dadosLotacao.dadosOpPort.aliqRat));
    Gerador.wCampo(tcDe4, '', 'fap',     1, 5, 1, infoLotacao.dadosLotacao.dadosOpPort.fap);

    Gerador.wGrupo('/dadosOpPort');  
  end;  
end;

procedure TevtTabLotacao.GerarInfoProcJudTerceiros;
var
  i: Integer;
begin
  if (self.infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros.procJudTerceiro.Count > 0) then
  begin
    Gerador.wGrupo('infoProcJudTerceiros');
    with self.infoLotacao.dadosLotacao.fPasLotacao.infoProcJudTerceiros do
    begin
      for i := 0 to procJudTerceiro.Count-1 do
      begin
        Gerador.wGrupo('procJudTerceiro');

        Gerador.wCampo(tcStr, '', 'codTerc',   1,  4, 1, procJudTerceiro.Items[i].codTerc);
        Gerador.wCampo(tcStr, '', 'nrProcJud', 1, 20, 1, procJudTerceiro.Items[i].nrProcJud);

        if trim(procJudTerceiro.Items[i].codSusp) <> '' then
          Gerador.wCampo(tcInt, '', 'codSusp', 1, 14, 1, procJudTerceiro.Items[i].codSusp);

        Gerador.wGrupo('/procJudTerceiro');
      end;

      if procJudTerceiro.Count > 99 then
        Gerador.wAlerta('', 'infoProcJudTerceiros', 'Lista de Processos Judic. Terceiros', ERR_MSG_MAIOR_MAXIMO + '99');
    end;
    Gerador.wGrupo('/infoProcJudTerceiros');
  end;
end;

function TevtTabLotacao.GerarXML: boolean;
begin
  try
    inherited GerarXML;
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

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabLotacao');

//    Validar(schevtTabLotacao);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
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
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtTabLotacao';
      Id             := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial     := INIRec.ReadInteger(sSecao, 'Sequencial', 0);
      ModoLancamento := eSStrToModoLancamento(Ok, INIRec.ReadString(sSecao, 'ModoLancamento', 'inclusao'));

      sSecao := 'ideEvento';
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

          with infoLotacao.dadosLotacao.fpasLotacao.infoProcJudTerceiros.procJudTerceiro.New do
          begin
            codTerc   := sFim;
            nrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', '');
            codSusp   := INIRec.ReadString(sSecao, 'codSusp', '');
          end;

          Inc(I);
        end;

        sSecao := 'infoEmprParcial';
        if INIRec.ReadString(sSecao, 'tpInscContrat', '') <> ''then
        begin
          infoLotacao.dadosLotacao.infoEmprParcial.tpInscContrat := eSStrTotpInscContratante(Ok, INIRec.ReadString(sSecao, 'tpInscContrat', '1'));
          infoLotacao.dadosLotacao.infoEmprParcial.nrInscContrat := INIRec.ReadString(sSecao, 'nrInscContrat', EmptyStr);
          infoLotacao.dadosLotacao.infoEmprParcial.tpInscProp    := eSStrToTpInscProp(Ok, INIRec.ReadString(sSecao, 'tpInscProp', '1'));
          infoLotacao.dadosLotacao.infoEmprParcial.nrInscProp    := INIRec.ReadString(sSecao, 'nrInscProp', EmptyStr);
        end;

        sSecao := 'dadosOpPort';
        if INIRec.ReadString(sSecao, 'aliqRat', '') <> '' then
        begin
          infoLotacao.dadosLotacao.dadosOpPort.aliqRat := eSStrToAliqRat(Ok, INIRec.ReadString(sSecao, 'aliqRat', EmptyStr));
          infoLotacao.dadosLotacao.dadosOpPort.fap := StringToFloatDef(INIRec.ReadString(sSecao, 'fap', EmptyStr),0);
        end;

        if ModoLancamento = mlAlteracao then
        begin
          sSecao := 'novaValidade';
          infoLotacao.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoLotacao.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
        end;
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

{ TDadosLotacao }

constructor TDadosLotacao.Create;
begin
  inherited Create;
  fFPasLotacao     := TFPasLotacao.Create;
  fInfoEmprParcial := TinfoEmprParcial.Create;
  fdadosOpPort     := TdadosOpPort.Create;
end;

destructor TDadosLotacao.Destroy;
begin
  fFPasLotacao.Free;
  FinfoEmprParcial.Free;
  fdadosOpPort.Free;

  inherited;
end;

{ TInfoLotacao }

constructor TInfoLotacao.Create;
begin
  inherited Create;
  fIdeLotacao   := TIdeLotacao.Create;
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

{ TProcJudTerceiroCollection }

function TProcJudTerceiroCollection.Add: TProcJudTerceiroCollectionItem;
begin
  Result := Self.New;
end;

function TProcJudTerceiroCollection.GetItem(
  Index: Integer): TProcJudTerceiroCollectionItem;
begin
  Result := TProcJudTerceiroCollectionItem(inherited Items[Index]);
end;

procedure TProcJudTerceiroCollection.SetItem(Index: Integer;
  Value: TProcJudTerceiroCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TProcJudTerceiroCollection.New: TProcJudTerceiroCollectionItem;
begin
  Result := TProcJudTerceiroCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoProcJudTerceiros }

constructor TInfoProcJudTerceiros.Create;
begin
  inherited Create;
  FProcJudTerceiro := TProcJudTerceiroCollection.create;
end;

destructor TInfoProcJudTerceiros.destroy;
begin
  FProcJudTerceiro.Free;

  inherited;
end;

{ TFPasLotacao }

constructor TFPasLotacao.Create;
begin
  inherited Create;
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
