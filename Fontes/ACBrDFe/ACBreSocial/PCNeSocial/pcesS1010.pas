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

{$I ACBr.inc}

unit pcesS1010;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase, ACBrUtil,
  pcnConversao, pcnGerador,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1010Collection = class;
  TS1010CollectionItem = class;
  TEvtTabRubrica = class;
  TInfoRubrica = class;
  TDadosRubrica = class;
  TIdeRubrica = class;
  TIdeProcessoCPCollection = class;
  TIdeProcessoCPCollectionItem = class;
  TIdeProcessoIRRFCollection = class;
  TIdeProcessoFGTSCollection = class;
  TIdeProcessoSindCollection = class;

  TS1010Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1010CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1010CollectionItem);
  public
    function Add: TS1010CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1010CollectionItem;
    property Items[Index: Integer]: TS1010CollectionItem read GetItem write SetItem; default;
  end;

  TS1010CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabRubrica: TEvtTabRubrica;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabRubrica: TEvtTabRubrica read FEvtTabRubrica write FEvtTabRubrica;
  end;

  TProcessoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TProcesso;
    procedure SetItem(Index: Integer; Value: TProcesso);
  public
    function Add: TProcesso; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TProcesso;
    property Items[Index: Integer]: TProcesso read GetItem write SetItem; default;
  end;

  TEvtTabRubrica = class(TeSocialEvento)
  private
    FModoLancamento: TModoLancamento;
    FIdeEmpregador: TIdeEmpregador;
    FIdeEvento: TIdeEvento;
    FInfoRubrica: TInfoRubrica;

    {Geradores específicos da classe}
    procedure GerarIdeRubrica;
    procedure GerarDadosRubrica;
    procedure GerarIdeProcessoCP;
    procedure GerarProcessos(const pChave: string; pProcessoCollection: TProcessoCollection);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property IdeEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property InfoRubrica: TInfoRubrica read FInfoRubrica write FInfoRubrica;
  end;

  TInfoRubrica = class(TObject)
  private
    FDadosRubrica: TDadosRubrica;
    FideRubrica: TideRubrica;
    FnovaValidade: TidePeriodo;

    function getDadosRubrica: TDadosRubrica;
    function getNovaValidade: TidePeriodo;
  public
    constructor Create;
    destructor Destroy; override;

    function dadosRubricaInst(): Boolean;
    function novaValidadInst(): Boolean;

    property ideRubrica: TideRubrica read FideRubrica write FideRubrica;
    property DadosRubrica: TDadosRubrica read getDadosRubrica write FDadosRubrica;
    property novaValidade: TidePeriodo read getNovaValidade write FnovaValidade;
  end;

  TDadosRubrica = class(TObject)
  private
    FDscRubr: string;
    FNatRubr: integer;
    FTpRubr: tpTpRubr;
    FCodIncCP: tpCodIncCP;
    FCodIncIRRF : tpCodIncIRRF;
    FCodIncFGTS : tpCodIncFGTS;
    FCodIncSIND: tpCodIncSIND;
    FObservacao: string;
    FIdeProcessoCP: TIdeProcessoCPCollection;
    FIdeProcessoIRRF: TIdeProcessoIRRFCollection;
    FIdeProcessoFGTS: TIdeProcessoFGTSCollection;
    FIdeProcessoSIND: TIdeProcessoSindCollection;

    function getIdeProcessoCP(): TIdeProcessoCPCollection;
    function getIdeProcessoIRRF(): TIdeProcessoIRRFCollection;
    function getIdeProcessoFGTS(): TIdeProcessoFGTSCollection;
    function getIdeProcessoSIND(): TIdeProcessoSindCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function ideProcessoCPInst(): Boolean;
    function ideProcessoIRRFInst(): Boolean;
    function ideProcessoFGTSInst(): Boolean;
    function ideProcessoSINDInst(): Boolean;

    property dscRubr: string read FDscRubr write FDscRubr;
    property natRubr: integer read FNatRubr write FNatRubr;
    property tpRubr: tpTpRubr read FTpRubr write FTpRubr;
    property codIncCP: tpCodIncCP read FCodIncCP write FCodIncCP;
    property codIncIRRF: tpCodIncIRRF read FCodIncIRRF write FCodIncIRRF;
    property codIncFGTS: tpCodIncFGTS read FCodIncFGTS write FCodIncFGTS;
    property codIncSIND: tpCodIncSIND read FCodIncSIND write FCodIncSIND;
    property observacao: string read FObservacao write FObservacao;
    property IdeProcessoCP: TIdeProcessoCPCollection read getIdeProcessoCP write FIdeProcessoCP;
    property IdeProcessoIRRF: TIdeProcessoIRRFCollection read getIdeProcessoIRRF write FIdeProcessoIRRF;
    property IdeProcessoFGTS: TIdeProcessoFGTSCollection read getIdeProcessoFGTS write FIdeProcessoFGTS;
    property IdeProcessoSIND: TIdeProcessoSindCollection read getIdeProcessoSIND write FIdeProcessoSIND;
  end;

  TIdeRubrica = class(TObject)
  private
    FCodRubr: string;
    FIdeTabRubr: string;
    FIniValid: string;
    FFimValid: string;
  public
    property CodRubr: string read FCodRubr write FCodRubr;
    property ideTabRubr: string read FIdeTabRubr write FIdeTabRubr;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TIdeProcessoCPCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeProcessoCPCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeProcessoCPCollectionItem);
  public
    function Add: TIdeProcessoCPCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeProcessoCPCollectionItem;
    property Items[Index: Integer]: TIdeProcessoCPCollectionItem read GetItem write SetItem;
  end;

  TIdeProcessoCPCollectionItem = class(TProcesso)
  private
    FtpProc: tpTpProc;
    FExtDecisao: TpExtDecisao;
  public
    property tpProc: tpTpProc read FtpProc write FtpProc;
    property ExtDecisao: TpExtDecisao read FExtDecisao write FExtDecisao;
  end;

  TIdeProcessoIRRFCollection = class(TProcessoCollection)
  end;

  TIdeProcessoFGTSCollection = class(TProcessoCollection)
  end;

  TIdeProcessoSindCollection = class(TProcessoCollection)
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS1010Collection }

function TS1010Collection.Add: TS1010CollectionItem;
begin
  Result := Self.New;
end;

function TS1010Collection.GetItem(Index: Integer): TS1010CollectionItem;
begin
  Result := TS1010CollectionItem(inherited GetItem(Index));
end;

procedure TS1010Collection.SetItem(Index: Integer;
  Value: TS1010CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TS1010Collection.New: TS1010CollectionItem;
begin
  Result := TS1010CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS1010CollectionItem }

constructor TS1010CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento    := teS1010;
  FEvtTabRubrica := TEvtTabRubrica.Create(AOwner);
end;

destructor TS1010CollectionItem.Destroy;
begin
  FEvtTabRubrica.Free;

  inherited;
end;

{ TEvtTabRubrica }

constructor TEvtTabRubrica.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeEvento     := TIdeEvento.Create;
  FInfoRubrica   := TInfoRubrica.Create;
end;

destructor TEvtTabRubrica.Destroy;
begin
  FIdeEmpregador.Free;
  FIdeEvento.Free;
  FInfoRubrica.Free;

  inherited;
end;

procedure TEvtTabRubrica.GerarDadosRubrica;
begin
  Gerador.wGrupo('dadosRubrica');

  Gerador.wCampo(tcStr, '', 'dscRubr',    1, 100, 1, InfoRubrica.dadosRubrica.dscRubr);
  Gerador.wCampo(tcInt, '', 'natRubr',    1,   4, 1, InfoRubrica.dadosRubrica.natRubr);
  Gerador.wCampo(tcStr, '', 'tpRubr',     1,   1, 1, eSTpRubrToStr(InfoRubrica.dadosRubrica.tpRubr));
  Gerador.wCampo(tcStr, '', 'codIncCP',   2,   2, 1, eSCodIncCPToStr(InfoRubrica.dadosRubrica.codIncCP));
  Gerador.wCampo(tcStr, '', 'codIncIRRF', 2,   2, 1, eSCodIncIRRFToStr(InfoRubrica.dadosRubrica.codIncIRRF));
  Gerador.wCampo(tcStr, '', 'codIncFGTS', 2,   2, 1, eSCodIncFGTSToStr(InfoRubrica.dadosRubrica.codIncFGTS));
  Gerador.wCampo(tcStr, '', 'codIncSIND', 2,   2, 1, eSCodIncSINDToStr(InfoRubrica.dadosRubrica.codIncSIND));
  Gerador.wCampo(tcStr, '', 'observacao', 0, 255, 0, InfoRubrica.dadosRubrica.observacao);

  GerarideProcessoCP;

  GerarProcessos('ideProcessoIRRF', InfoRubrica.dadosRubrica.IdeProcessoIRRF);
  GerarProcessos('ideProcessoFGTS', InfoRubrica.dadosRubrica.IdeProcessoFGTS);
  GerarProcessos('ideProcessoSIND', InfoRubrica.dadosRubrica.IdeProcessoSIND);

  Gerador.wGrupo('/dadosRubrica');
end;

procedure TEvtTabRubrica.GerarProcessos(const pChave: String; pProcessoCollection: TProcessoCollection);
var
  i: Integer;
begin
  for i := 0 to pProcessoCollection.Count - 1 do
    GerarProcessoGenerico(pChave, pProcessoCollection[i]);

  if pProcessoCollection.Count > 99 then
    Gerador.wAlerta('', pChave, 'Lista de Processos: ' + pChave, ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtTabRubrica.GerarIdeProcessoCP;
var
  i: integer;
begin
  if (InfoRubrica.DadosRubrica.ideProcessoCPInst()) then
  begin
    for i := 0 to InfoRubrica.DadosRubrica.IdeProcessoCP.Count - 1 do
    begin
      Gerador.wGrupo('ideProcessoCP');

      Gerador.wCampo(tcStr, '', 'tpProc',     1,  1, 1, eSTpProcessoToStr(InfoRubrica.DadosRubrica.IdeProcessoCP.GetItem(i).tpProc));
      Gerador.wCampo(tcStr, '', 'nrProc',     1, 20, 1, InfoRubrica.DadosRubrica.IdeProcessoCP.GetItem(i).nrProc);
      Gerador.wCampo(tcStr, '', 'extDecisao', 1,  1, 1, eSExtDecisaoToStr(InfoRubrica.DadosRubrica.IdeProcessoCP.GetItem(i).extDecisao));

      if trim(InfoRubrica.DadosRubrica.IdeProcessoCP.GetItem(i).codSusp) <> '' then
        Gerador.wCampo(tcStr, '', 'codSusp', 1, 14, 1, InfoRubrica.DadosRubrica.IdeProcessoCP.GetItem(i).codSusp);

      Gerador.wGrupo('/ideProcessoCP');
    end;

    if InfoRubrica.DadosRubrica.IdeProcessoCP.Count > 99 then
      Gerador.wAlerta('', 'ideProcessoCP', 'Lista de Processos', ERR_MSG_MAIOR_MAXIMO + '99');
  end;
end;

procedure TEvtTabRubrica.GerarIdeRubrica;
begin
  Gerador.wGrupo('ideRubrica');

  Gerador.wCampo(tcStr, '', 'codRubr', 1, 30, 1, InfoRubrica.IdeRubrica.CodRubr);

  if (infoRubrica.ideRubrica.ideTabRubr <> '') then
    Gerador.wCampo(tcStr, '', 'ideTabRubr', 1, 8, 1, infoRubrica.ideRubrica.ideTabRubr);

  Gerador.wCampo(tcStr, '', 'iniValid', 7, 7, 1, infoRubrica.ideRubrica.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid', 7, 7, 0, infoRubrica.IdeRubrica.fimValid);

  Gerador.wGrupo('/ideRubrica');
end;

function TEvtTabRubrica.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTabRubrica');
    Gerador.wGrupo('evtTabRubrica Id="' + Self.Id + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoRubrica');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeRubrica;

    if Self.ModoLancamento <> mlExclusao then
    begin
      GerarDadosRubrica;

      if (ModoLancamento = mlAlteracao) then
        if (InfoRubrica.novaValidadInst()) then
          GerarIdePeriodo(InfoRubrica.novaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoRubrica');
    Gerador.wGrupo('/evtTabRubrica');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabRubrica');

//    Validar(schevtTabRubrica);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtTabRubrica.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtTabRubrica';
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

      sSecao := 'ideRubrica';
      infoRubrica.ideRubrica.codRubr    := INIRec.ReadString(sSecao, 'codRubr', EmptyStr);
      infoRubrica.ideRubrica.ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', EmptyStr);
      infoRubrica.ideRubrica.IniValid   := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoRubrica.ideRubrica.FimValid   := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if (ModoLancamento <> mlExclusao) then
      begin
        sSecao := 'dadosRubrica';
        infoRubrica.dadosRubrica.dscRubr    := INIRec.ReadString(sSecao, 'dscRubr', EmptyStr);
        infoRubrica.dadosRubrica.natRubr    := INIRec.ReadInteger(sSecao, 'natRubr', 0);
        infoRubrica.dadosRubrica.tpRubr     := eSStrToTpRubr(Ok, INIRec.ReadString(sSecao, 'tpRubr', '1'));
        infoRubrica.dadosRubrica.codIncCP   := eSStrToCodIncCP(Ok, INIRec.ReadString(sSecao, 'codIncCP', '00'));
        infoRubrica.dadosRubrica.codIncIRRF := eSStrToCodIncIRRF(Ok, INIRec.ReadString(sSecao, 'codIncIRRF', '00'));
        infoRubrica.dadosRubrica.codIncFGTS := eSStrToCodIncFGTS(Ok, INIRec.ReadString(sSecao, 'codIncFGTS', '00'));
        infoRubrica.dadosRubrica.codIncSIND := eSStrToCodIncSIND(Ok, INIRec.ReadString(sSecao, 'codIncSIND', '00'));
        infoRubrica.dadosRubrica.observacao := INIRec.ReadString(sSecao, 'observacao', EmptyStr);

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'ideProcessoCP' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'tpProc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoRubrica.dadosRubrica.ideProcessoCP.New do
          begin
            tpProc     := eSStrToTpProcesso(Ok, sFim);
            nrProc     := INIRec.ReadString(sSecao, 'nrProc', '');
            extDecisao := eSStrToExtDecisao(Ok, INIRec.ReadString(sSecao, 'extDecisao', '1'));
            codSusp    := INIRec.ReadString(sSecao, 'codSusp', '');
          end;

          Inc(I);
        end;

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'ideProcessoIRRF' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'nrProc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoRubrica.dadosRubrica.ideProcessoIRRF.New do
          begin
            nrProc  := INIRec.ReadString(sSecao, 'nrProc', '');
            codSusp := INIRec.ReadString(sSecao, 'codSusp', '');
          end;

          Inc(I);
        end;

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'ideProcessoFGTS' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'nrProc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoRubrica.dadosRubrica.ideProcessoFGTS.New do
          begin
            nrProc := INIRec.ReadString(sSecao, 'nrProc', '');
          end;

          Inc(I);
        end;

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'ideProcessoSIND' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'nrProc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoRubrica.dadosRubrica.ideProcessoSIND.New do
          begin
            nrProc := INIRec.ReadString(sSecao, 'nrProc', '');
          end;

          Inc(I);
        end;

        if ModoLancamento = mlAlteracao then
        begin
          sSecao := 'novaValidade';
          infoRubrica.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoRubrica.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
        end;
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

{ TInfoRubrica }

constructor TInfoRubrica.Create;
begin
  inherited Create;
  FideRubrica   := TideRubrica.Create;
  FDadosRubrica := nil;
  FnovaValidade := nil;
end;

function TInfoRubrica.dadosRubricaInst: Boolean;
begin
  Result := Assigned(FDadosRubrica);
end;

destructor TInfoRubrica.destroy;
begin
  FDadosRubrica.Free;
  FideRubrica.Free;
  FnovaValidade.Free;

  inherited;
end;

function TInfoRubrica.getDadosRubrica: TDadosRubrica;
begin
  if Not(Assigned(FDadosRubrica)) then
    FDadosRubrica := TDadosRubrica.create;

  Result := FDadosRubrica;
end;

function TInfoRubrica.getNovaValidade: TidePeriodo;
begin
  if Not(Assigned(FnovaValidade)) then
    FnovaValidade := TIdePeriodo.Create;

  Result := FnovaValidade;
end;

function TInfoRubrica.novaValidadInst: Boolean;
begin
  Result := Assigned(FnovaValidade);
end;

{ TIdeProcessoCPCollection }

function TIdeProcessoCPCollection.Add: TIdeProcessoCPCollectionItem;
begin
  Result := Self.New;
end;

function TIdeProcessoCPCollection.GetItem(
  Index: Integer): TIdeProcessoCPCollectionItem;
begin
  Result := TIdeProcessoCPCollectionItem(Inherited GetItem(Index));
end;

procedure TIdeProcessoCPCollection.SetItem(Index: Integer;
  Value: TIdeProcessoCPCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

function TIdeProcessoCPCollection.New: TIdeProcessoCPCollectionItem;
begin
  Result := TIdeProcessoCPCollectionItem.Create;
  Self.Add(Result);
end;

{ TDadosRubrica }

constructor TDadosRubrica.Create;
begin
  inherited Create;
  FIdeProcessoCP   := nil;
  FIdeProcessoIRRF := nil;
  FIdeProcessoFGTS := nil;
  FIdeProcessoSIND := nil;
end;

destructor TDadosRubrica.destroy;
begin
  FreeAndNil(FIdeProcessoCP);
  FreeAndNil(FIdeProcessoIRRF);
  FreeAndNil(FIdeProcessoFGTS);
  FreeAndNil(FIdeProcessoSIND);

  inherited;
end;

function TDadosRubrica.getIdeProcessoCP: TIdeProcessoCPCollection;
begin
  if Not(Assigned(FIdeProcessoCP)) then
    FIdeProcessoCP := TIdeProcessoCPCollection.Create;
  Result := FIdeProcessoCP;
end;

function TDadosRubrica.getIdeProcessoFGTS: TIdeProcessoFGTSCollection;
begin
  if Not(Assigned(FIdeProcessoFGTS)) then
    FIdeProcessoFGTS := TIdeProcessoFGTSCollection.Create;
  Result := FIdeProcessoFGTS;
end;

function TDadosRubrica.getIdeProcessoIRRF: TIdeProcessoIRRFCollection;
begin
  if Not(Assigned(FIdeProcessoIRRF)) then
    FIdeProcessoIRRF := TIdeProcessoIRRFCollection.Create;
  Result := FIdeProcessoIRRF;
end;

function TDadosRubrica.getIdeProcessoSIND: TIdeProcessoSindCollection;
begin
  if Not(Assigned(FIdeProcessoSIND)) then
    FIdeProcessoSIND := TIdeProcessoSINDCollection.Create;
  Result := FIdeProcessoSIND;
end;

function TDadosRubrica.ideProcessoCPInst: Boolean;
begin
  Result := Assigned(FIdeProcessoCP);
end;

function TDadosRubrica.ideProcessoFGTSInst: Boolean;
begin
  Result := Assigned(FIdeProcessoFGTS);
end;

function TDadosRubrica.ideProcessoIRRFInst: Boolean;
begin
  Result := Assigned(FIdeProcessoIRRF);
end;

function TDadosRubrica.ideProcessoSINDInst: Boolean;
begin
  Result := Assigned(FIdeProcessoSIND);
end;

{ TProcessoCollection }

function TProcessoCollection.Add: TProcesso;
begin
  Result := Self.New;
end;

function TProcessoCollection.GetItem(
  Index: Integer): TProcesso;
begin
  Result := TProcesso(Inherited GetItem(Index));
end;

procedure TProcessoCollection.SetItem(Index: Integer;
  Value: TProcesso);
begin
  Inherited SetItem(Index, Value);
end;

function TProcessoCollection.New: TProcesso;
begin
  Result := TProcesso.Create;
  Self.Add(Result);
end;

end.
