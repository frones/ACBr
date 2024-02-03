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

unit pcesS2399;

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
  TS2399CollectionItem = class;
  TEvtTSVTermino = class;
  TInfoTSVTermino = class;
  TVerbasRescS2399 = class;
  TDmDevCollectionItem = class;
  TDmDevCollection = class;
  TRemunAposTerm = class;

  TS2399Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2399CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2399CollectionItem);
  public
    function Add: TS2399CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2399CollectionItem;
    property Items[Index: Integer]: TS2399CollectionItem read GetItem write SetItem; default;
  end;

  TS2399CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTSVTermino : TEvtTSVTermino;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTSVTermino: TEvtTSVTermino read FEvtTSVTermino write FEvtTSVTermino;
  end;

  TEvtTSVTermino = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabSemVInc : TideTrabSemVinc;
    FInfoTSVTermino: TInfoTSVTermino;

    procedure GerarInfoTSVTermino(obj: TInfoTSVTermino);
    procedure GerarVerbasResc(obj: TVerbasRescS2399);
    procedure GerarIdeTrabSemVinc(obj: TIdeTrabSemVinc);
    procedure GerarDmDev(pDmDev: TDmDevCollection);
    procedure GerarRemunAposTerm(obj: TRemunAposTerm);
   public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTrabSemVInc: TideTrabSemVinc read FIdeTrabSemVInc write FIdeTrabSemVInc;
    property InfoTSVTermino: TInfoTSVTermino read FInfoTSVTermino write FInfoTSVTermino;
  end;

  TinfoTSVTermino = class(TObject)
  private
    FdtTerm : TDateTime;
    FmtvDesligTSV : string;
    FverbasResc : TVerbasRescS2399;
    Fquarentena : TQuarentena;
    FPensAlim: tpPensaoAlim;
    FpercAliment: Double;
    FVrAlim: Double;
    FmudancaCPF: TMudancaCPF3;
    FnrProcTrab: String;
    FRemunAposTerm: TRemunAposTerm;
  public
    constructor Create;
    destructor  Destroy; override;

    property dtTerm : TDateTime read FdtTerm write FdtTerm;
    property mtvDesligTSV : string read FmtvDesligTSV write FmtvDesligTSV;
    property verbasResc : TVerbasRescS2399 read FverbasResc write FverbasResc;
    property mudancaCPF: TMudancaCPF3 read FmudancaCPF write FmudancaCPF;
    property quarentena : TQuarentena read Fquarentena write Fquarentena;
    property percAliment : Double read FpercAliment write FpercAliment;
    property vrAlim: Double read FVrAlim write FVrAlim;
    property pensAlim: tpPensaoAlim read FPensAlim write FPensAlim;
    property nrProcTrab : String read FnrProcTrab write FnrProcTrab;
    property RemunAposTerm: TRemunAposTerm read FRemunAposTerm write FRemunAposTerm;
  end;

  TDmDevCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDMDevCollectionItem;
    procedure SetItem(Index: Integer; Value: TDMDevCollectionItem);
  public
    function Add: TDMDevCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDMDevCollectionItem;
    property Items[Index: Integer]: TDMDevCollectionItem read GetItem write SetItem; default;
  end;

  TDmDevCollectionItem = class(TObject)
  private
    FIdeDmDev: string;
    FIdeEstabLot: TideEstabLotCollection;
    FindRRA: tpSimNaoFacultativo;
    FinfoRRA: TinfoRRA;

    function getInfoRRA: TInfoRRA;
  public
    constructor Create;
    destructor Destroy; override;

    function infoRRAInst: boolean;

    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property indRRA: tpSimNaoFacultativo read FindRRA write FindRRA;
    property infoRRA: TinfoRRA read getInfoRRA write FinfoRRA;
    property ideEstabLot: TideEstabLotCollection read FIdeEstabLot write FIdeEstabLot;
  end;

  TVerbasRescS2399 = class(TVerbasResc)
  private
    FDmDev: TDmDevCollection;
  public
    constructor Create; reintroduce;

    property dmDev: TDmDevCollection read FDmDev write FDmDev;
  end;

  TRemunAposTerm = class
  private
   FindRemun: TpIndRemun;
   FdtFimRemun: TDateTime;
  public
   property indRemun: TpIndRemun read FindRemun write FindRemun;
   property dtFimRemun: TDateTime read FdtFimRemun write FdtFimRemun;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2399Collection }

function TS2399Collection.Add: TS2399CollectionItem;
begin
  Result := Self.New;
end;

function TS2399Collection.GetItem(Index: Integer): TS2399CollectionItem;
begin
  Result := TS2399CollectionItem(inherited Items[Index]);
end;

procedure TS2399Collection.SetItem(Index: Integer; Value: TS2399CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2399Collection.New: TS2399CollectionItem;
begin
  Result := TS2399CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2399CollectionItem }

constructor TS2399CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento    := teS2399;
  FEvtTSVTermino := TEvtTSVTermino.Create(AOwner);
end;

destructor TS2399CollectionItem.Destroy;
begin
  FEvtTSVTermino.Free;

  inherited;
end;

{ TinfoTSVTermino }

constructor TinfoTSVTermino.Create;
begin
  inherited;

  FverbasResc    := TVerbasRescS2399.Create;
  FmudancaCPF    := TMudancaCPF3.Create;
  Fquarentena    := TQuarentena.Create;
  FRemunAposTerm := TRemunAposTerm.Create;
end;

destructor TinfoTSVTermino.Destroy;
begin
  FverbasResc.Free;
  FmudancaCPF.Free;
  Fquarentena.Free;
  FreeAndNil(FRemunAposTerm);

  inherited;
end;

{ TDmDevCollection }

function TDmDevCollection.Add: TDMDevCollectionItem;
begin
  Result := Self.New;
end;

function TDmDevCollection.GetItem(Index: Integer): TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem(inherited Items[Index]);
end;

procedure TDmDevCollection.SetItem(Index: Integer; Value: TDMDevCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDmDevCollection.New: TDMDevCollectionItem;
begin
  Result := TDmDevCollectionItem.Create;
  Self.Add(Result);
end;

{ TDMDevCollectionItem }

constructor TDMDevCollectionItem.Create;
begin
  inherited Create;

  FIdeEstabLot := TideEstabLotCollection.Create;
  FInfoRRA     := nil;
end;

destructor TDMDevCollectionItem.Destroy;
begin
  FreeAndNil(FIdeEstabLot);

  if infoRRAInst then
    FreeAndNil(FinfoRRA);

  inherited;
end;

function TDMDevCollectionItem.getInfoRRA: TInfoRRA;
begin
  if not(Assigned(FInfoRRA)) then
    FInfoRRA := TInfoRRA.Create;
  Result := FInfoRRA;
end;

function TDMDevCollectionItem.infoRRAInst: boolean;
begin
  Result := Assigned(FInfoRRA);
end;

{ TVerbasRescS2399 }

constructor TVerbasRescS2399.Create;
begin
  inherited;

  FDmDev := TDmDevCollection.Create;
end;


{ TEvtTSVTermino }

constructor TEvtTSVTermino.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento      := TIdeEvento2.Create;
  FIdeEmpregador  := TIdeEmpregador.Create;
  FIdeTrabSemVInc := TideTrabSemVinc.Create;
  FInfoTSVTermino := TInfoTSVTermino.Create;
end;

destructor TEvtTSVTermino.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrabSemVInc.Free;
  FInfoTSVTermino.Free;

  inherited;
end;

procedure TEvtTSVTermino.GerarIdeTrabSemVinc(obj: TIdeTrabSemVinc);
begin
  Gerador.wGrupo('ideTrabSemVinculo');

  Gerador.wCampo(tcStr, '', 'cpfTrab',  11, 11, 1, obj.cpfTrab);

  if VersaoDF <= ve02_05_00 then
    Gerador.wCampo(tcStr, '', 'nisTrab',   1, 11, 0, obj.nisTrab)
  else
    Gerador.wCampo(tcStr, '', 'matricula', 1, 30, 0, obj.matricula);

  if (VersaoDF <= ve02_05_00) or (obj.matricula = '') then
    Gerador.wCampo(tcStr, '', 'codCateg',  1,  3, 1, obj.codCateg);

  Gerador.wGrupo('/ideTrabSemVinculo');
end;

procedure TEvtTSVTermino.GerarRemunAposTerm(obj: TRemunAposTerm);
begin
  if (VersaoDF >= veS01_01_00) and (obj.indRemun <> ireNaoInformado) then
  begin
    Gerador.wGrupo('remunAposTerm');

    Gerador.wCampo(tcStr, '', 'indRemun',         1,  1, 1, TpIndRemunToStr(Obj.indRemun));
    Gerador.wCampo(tcDat, '', 'dtFimRemun',      10, 10, 1, Obj.dtFimRemun);

    Gerador.wGrupo('/remunAposTerm');
  end;
end;

procedure TEvtTSVTermino.GerarInfoTSVTermino(obj: TInfoTSVTermino);
begin
  Gerador.wGrupo('infoTSVTermino');

  Gerador.wCampo(tcDat, '', 'dtTerm',       10, 10, 1, obj.dtTerm);
  Gerador.wCampo(tcStr, '', 'mtvDesligTSV',  1,  2, 0, obj.mtvDesligTSV);

  if (
       (Self.IdeTrabSemVInc.codCateg = 201) or
       (Self.IdeTrabSemVInc.codCateg = 202) or
       (Self.IdeTrabSemVInc.codCateg = 721)
     ) and (VersaoDF >= ve02_05_00) then
  begin
    if obj.pensAlim <> paNenhum then
      Gerador.wCampo(tcStr, '', 'pensAlim',    1,  1, 1, obj.pensAlim);
    Gerador.wCampo(tcDe2, '', 'percAliment', 1,  5, 0, obj.percAliment);
    Gerador.wCampo(tcDe2, '', 'vrAlim',      1, 14, 0, obj.vrAlim);
    Gerador.wCampo(tcStr, '', 'nrProcTrab', 1, 20, 0, obj.nrProcTrab);
  end;

  if obj.mtvDesligTSV <> '07' then
     GerarVerbasResc(obj.verbasResc);

  if (VersaoDF >= ve02_05_00) and (obj.mtvDesligTSV = '07') then
     GerarMudancaCPF3(obj.mudancaCPF);
	
  GerarQuarentena(obj.quarentena);
  GerarRemunAposTerm(obj.RemunAposTerm);

  Gerador.wGrupo('/infoTSVTermino');
end;

procedure TEvtTSVTermino.GerarDmDev(pDmDev: TDmDevCollection);
var
  i: integer;
begin
  for i := 0 to pDmDev.Count - 1 do
  begin
    Gerador.wGrupo('dmDev');

    Gerador.wCampo(tcStr, '', 'ideDmDev', 1, 30, 1, pDmDev[i].ideDmDev);

    if VersaoDF >= veS01_01_00 then
    begin
      if (pDmDev[i].indRRA = snfSim) and (pDmDev[i].infoRRAInst()) then
      begin
        Gerador.wCampo(tcStr, '', 'indRRA', 1,  1, 1, eSSimNaoFacultativoToStr(pDmDev[i].indRRA));

        if (pDmDev[i].infoRRAInst()) then
          GerarInfoRRA(pDmDev[i].infoRRA);
      end;
    end;

    GerarIdeEstabLot(pDmDev[i].ideEstabLot);

    Gerador.wGrupo('/dmDev');
  end;

  if pDmDev.Count > 99 then
    Gerador.wAlerta('', 'dmDev', 'Lista de Demonstrativos', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtTSVTermino.GerarVerbasResc(obj: TVerbasRescS2399);
begin
  if (obj.dmDev.Count > 0) or (obj.ProcJudTrab.Count > 0) then
  begin
    Gerador.wGrupo('verbasResc');

    GerarDmDev(obj.dmDev);
    GerarProcJudTrab(obj.ProcJudTrab);

    if obj.infoMVInst then
      GerarInfoMV(obj.infoMV);

    Gerador.wGrupo('/verbasResc');
  end;
end;

function TEvtTSVTermino.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTSVTermino');
    Gerador.wGrupo('evtTSVTermino Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento, True, True, True);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeTrabSemVinc(self.IdeTrabSemVInc);
    GerarInfoTSVTermino(Self.InfoTSVTermino);

    Gerador.wGrupo('/evtTSVTermino');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTSVTermino');

//    Validar(schevtTSVTermino);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtTSVTermino.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J, K, L, M: Integer;
begin
  Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtTSVTermino';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.indGuia     := INIRec.ReadString(sSecao, 'indGuia', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideTrabSemVinculo';
      ideTrabSemVinc.CpfTrab    := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      ideTrabSemVinc.NisTrab    := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);
      ideTrabSemVinc.codCateg   := INIRec.ReadInteger(sSecao, 'codCateg', 0);
      ideTrabSemVinc.matricula  := INIRec.ReadString(sSecao, 'matricula', EmptyStr);

      sSecao := 'infoTSVTermino';
      infoTSVTermino.dtTerm       := StringToDateTime(INIRec.ReadString(sSecao, 'dtTerm', '0'));
      infoTSVTermino.mtvDesligTSV := INIRec.ReadString(sSecao, 'mtvDesligTSV', '');

      if (
           (IdeTrabSemVInc.codCateg = 201) or
           (IdeTrabSemVInc.codCateg = 202) or
           (IdeTrabSemVInc.codCateg = 721)
         ) and (VersaoDF >= ve02_05_00) then
      begin
        infoTSVTermino.pensAlim := eSStrToTpPensaoAlimEx(INIREC.ReadString(sSecao, 'pensAlim', EmptyStr));

        if (InfoTSVTermino.pensAlim <> paNaoExistePensaoAlimenticia) then
        begin
          infoTSVTermino.percAliment := StringToFloatDef(INIRec.ReadString(sSecao, 'percAliment', ''), 0);
          infoTSVTermino.vrAlim      := StringToFloatDef(INIRec.ReadString(sSecao, 'vrAlim', ''), 0);
        end;
      end;
      if  (VersaoDF > ve02_05_00) then
        infoTSVTermino.nrProcTrab   := INIRec.ReadString(sSecao, 'nrProcTrab', EmptyStr);

      sSecao := 'mudancaCPF';
      infoTSVTermino.mudancaCPF.novoCPF := INIRec.ReadString(sSecao, 'novoCPF', EmptyStr);

      I := 1;
      while true do
      begin
        // de 01 até 50
        sSecao := 'dmDev' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'ideDmDev', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infoTSVTermino.verbasResc.dmDev.New do
        begin
          ideDmDev := sFim;
          indRRA   := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indRRA', EmptyStr));

          sSecao := 'infoRRA' + IntToStrZero(I, 2);
          infoRRA.tpProcRRA := eSStrToTpProcRRA(Ok, INIRec.ReadString(sSecao, 'tpProcRRA', EmptyStr));
          infoRRA.nrProcRRA := INIRec.ReadString(sSecao, 'nrProcRRA', EmptyStr);
          infoRRA.descRRA := INIRec.ReadString(sSecao, 'descRRA', EmptyStr);
          infoRRA.qtdMesesRRA := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdMesesRRA', EmptyStr), 0);

          sSecao := 'despProcJud' + IntToStrZero(I, 2);
          infoRRA.despProcJud.vlrDespCustas := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespCustas', EmptyStr), 0);
          infoRRA.despProcJud.vlrDespAdvogados := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrDespAdvogados', EmptyStr), 0);

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'ideAdv' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoRRA.ideAdv.New do
             begin
              tpInsc   := eSStrToTpInscricao(Ok, sFim);
              nrInsc   := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
              vlrAdv := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrAdv', ''), 0);
            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'ideEstabLot' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with ideEstabLot.New do
            begin
              tpInsc     := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
              nrInsc     := sFim;
              codLotacao := INIRec.ReadString(sSecao, 'codLotacao', '');

              K := 1;
              while true do
              begin
                // de 001 até 200
                sSecao := 'detVerbas' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                             IntToStrZero(K, 3);
                sFim   := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with detVerbas.New do
                begin
                  codRubr    := sFim;
                  ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', '');
                  qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                  fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                  vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                  vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);
                  indApurIR  := eSStrToTpindApurIR(ok,INIRec.ReadString(sSecao, 'indApurIR', ''));

                  L := 1;
                  while true do
                  begin
                    // de 01 até 99
                    sSecao := 'detOper' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                             IntToStrZero(K, 3) + IntToStrZero(L, 2);
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
                        sSecao := 'detPlano' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                             IntToStrZero(K, 3) + IntToStrZero(L, 2) + IntToStrZero(M, 2);
                        sFim   := INIRec.ReadString(sSecao, 'nmDep', 'FIM');

                        if (sFim = 'FIM') or (Length(sFim) <= 0) then
                          break;

                        with detPlano.New do
                         begin
                          tpDep    := eSStrToTpDep(Ok, INIRec.ReadString(sSecao, 'tpDep', '00'));
                          cpfDep   := INIRec.ReadString(sSecao, 'cpfDep', '');
                          nmDep    := sFim;
                          dtNascto := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
                          vlrPgDep := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrPgDep', ''), 0);
                        end;

                        Inc(M);
                      end;
                    end;

                    Inc(L);
                  end;

                  sSecao := 'infoAgNocivo' + IntToStrZero(I, 2) +
                                    IntToStrZero(J, 2) + IntToStrZero(K, 3);
                  if INIRec.ReadString(sSecao, 'grauExp', '') <> '' then
                    infoAgNocivo.grauExp := eSStrToGrauExp(Ok, INIRec.ReadString(sSecao, 'grauExp', '1'));

                end;

                Inc(K);
              end;

              sSecao := 'infoSimples' + IntToStrZero(I, 2) +
                                IntToStrZero(J, 2);
              if INIRec.ReadString(sSecao, 'indSimples', '') <> '' then
                infoSimples.indSimples := eSStrToIndSimples(Ok, INIRec.ReadString(sSecao, 'indSimples', '1'));

            end;

            Inc(J);
          end;
        end;

        inc(I);
      end;

      I := 1;
      while true do
      begin
        // de 00 até 99
        sSecao := 'procJudTrab' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'tpTrib', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infoTSVTermino.verbasResc.procJudTrab.New do
        begin
          tpTrib    := eSStrToTpTributo(Ok, sFim);
          nrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', EmptyStr);
          codSusp   := INIRec.ReadString(sSecao, 'codSusp', '');
        end;

        Inc(I);
      end;

      sSecao := 'infoMV';
      if INIRec.ReadString(sSecao, 'indMV', '') <> '' then
      begin
        infoTSVTermino.VerbasResc.infoMV.indMV := eSStrToIndMV(Ok, INIRec.ReadString(sSecao, 'indMV', '1'));

        I := 1;
        while true do
        begin
          // de 01 até 10
          sSecao := 'remunOutrEmpr' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoTSVTermino.VerbasResc.infoMV.remunOutrEmpr.New do
          begin
            TpInsc     := eSStrToTpInscricao(Ok, sFim);
            NrInsc     := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
            codCateg   := INIRec.ReadInteger(sSecao, 'codCateg', 0);
            vlrRemunOE := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRemunOE', ''), 0);
          end;

          Inc(I);
        end;
      end;

      sSecao := 'remunAposTerm';
      infoTSVTermino.RemunAposTerm.indRemun   := StrToTpIndRemun(Ok, INIRec.ReadString(sSecao, 'indRemun', '0'));
      infoTSVTermino.RemunAposTerm.dtFimRemun := StringToDateTime(INIRec.ReadString(sSecao, 'dtFimRemun', '0'));

      sSecao := 'quarentena';
      if INIRec.ReadString(sSecao, 'dtFimQuar', '') <> '' then
        infoTSVTermino.quarentena.dtFimQuar := StringToDateTime(INIRec.ReadString(sSecao, 'dtFimQuar', '0'));
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
