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

unit pcesS1207;

interface

uses
  SysUtils, 
	Classes, 
	Controls,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
		System.Generics.Collections, 
		System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
		System.Contnrs,
  {$IfEnd}
  ACBrBase,
  ACBrDFeConsts,
  pcnConversao,
	pcnGerador,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  pcesCommon, 
	pcesConversaoeSocial, 
	pcesGerador;

type
  TEvtBenPrRP = class;
  TS1207CollectionItem = class;
  TS1207Collection = class;
  TDMDevCollection = class;
  TDMDevCollectionItem = class;
  TIdeBenef = class;
  TInfoPerApur = class;
  TInfoPerAnt = class;
  TIdeEstabCollection = class;
  TIdeEstabCollectionItem = class;
  TIdePeriodoCollection = class;
  TIdePeriodoCollectionItem = class;

  TS1207Collection = class(TeSocialCollection)
  private
    function GetItem(Index: integer): TS1207CollectionItem;
    procedure SetItem(Index: integer; Value: TS1207CollectionItem);
  public
    function Add: TS1207CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1207CollectionItem;
    property Items[Index: integer]: TS1207CollectionItem read GetItem write SetItem; default;
  end;

  TS1207CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtBenPrRP: TEvtBenPrRP;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtBenPrRP: TEvtBenPrRP read FEvtBenPrRP write FEvtBenPrRP;
  end;

  TDMDevCollection = class(TACBrObjectList)
  private
    function GetItem(Index: integer): TDMDevCollectionItem;
    procedure SetItem(Index: integer; Value: TDMDevCollectionItem);
  public
    function Add: TDMDevCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDMDevCollectionItem;
    property Items[Index: integer]: TDMDevCollectionItem read GetItem write SetItem; default;
  end;

  TDMDevCollectionItem = class(TObject)
  private
    FIdeDmDev: string;
    FNrBeneficio: string;
    FInfoPerApur: TInfoPerApur;
    FInfoPerAnt: TInfoPerAnt;
    FindRRA: tpSimNaoFacultativo;
    FinfoRRA: TinfoRRA;

    function getInfoPerApur(): TInfoPerApur;
    function getInfoPerAnt(): TInfoPerAnt;
    function getInfoRRA(): TInfoRRA;
  public
    constructor Create;
    destructor Destroy; override;

    function infoPerApurInst(): boolean;
    function infoPerAntInst(): boolean;
    function infoRRAInst(): boolean;

    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property nrBeneficio: string read FNrBeneficio write FNrBeneficio;
    property indRRA: tpSimNaoFacultativo read FindRRA write FindRRA;
    property infoRRA: TinfoRRA read getInfoRRA write FinfoRRA;
    property infoPerApur: TInfoPerApur read getInfoPerApur write FInfoPerApur;
    property infoPerAnt: TInfoPerAnt read getInfoPerAnt write FInfoPerAnt;
  end;

  TEvtBenPrRP = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FIdeBenef: TIdeBenef;
    FDMDev: TDMDevCollection;

    procedure GerarIdeEstab(objIdeEstab: TIdeEstabCollection);
    procedure GerarIdePeriodo(objIdePeriodo: TIdePeriodoCollection);
    procedure GerarIdeBenef;
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
    property ideBenef: TIdeBenef read FIdeBenef write FIdeBenef;
    property dmDev: TDMDevCollection read FDMDev write FDMDev;
  end;

  TIdeBenef = class(TObject)
  private
    FCpfBenef: string;
  public
    property cpfBenef: string read FCpfBenef write FCpfBenef;
  end;

  TInfoPerApur = class(TObject)
  private
    FIdeEstab: TIdeEstabCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property ideEstab: TIdeEstabCollection read FIdeEstab write FIdeEstab;
  end;

  TInfoPerAnt = class(TObject)
  private
    FIdePeriodo: TIdePeriodoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property idePeriodo: TIdePeriodoCollection read FIdePeriodo write FIdePeriodo;
  end;

  TIdeEstabCollection = class(TACBrObjectList)
  private
    function GetItem(Index: integer): TIdeEstabCollectionItem;
    procedure SetItem(Index: integer; Value: TIdeEstabCollectionItem);
  public
    function Add: TIdeEstabCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeEstabCollectionItem;
    property Items[Index: integer]: TIdeEstabCollectionItem read GetItem write SetItem;
  end;

  TIdeEstabCollectionItem = class(TObject)
  private
    FTpInsc: TpTpInsc;
    FNrInsc: string;
    FItensRemun: TRubricaCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInsc: TpTPInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property itensRemun: TRubricaCollection read FItensRemun write FItensRemun;
  end;

  TIdePeriodoCollection = class(TACBrObjectList)
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

implementation

uses
  IniFiles,
  ACBreSocial;

{ TDMDevCollection }

function TDMDevCollection.Add: TDMDevCollectionItem;
begin
  Result := Self.New;
end;

function TDMDevCollection.GetItem(Index: integer): TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem(inherited Items[Index]);
end;

procedure TDMDevCollection.SetItem(Index: integer; Value: TDMDevCollectionItem);
begin
  inherited Items[Index] := Value;
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

  FinfoRRA        := nil;
end;

destructor TDMDevCollectionItem.Destroy;
begin
  if infoRRAInst() then
    FreeAndNil(FinfoRRA);

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

{ TEvtBenPrRP }

constructor TEvtBenPrRP.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento3.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeBenef      := TIdeBenef.Create;
  FDMDev         := TDMDevCollection.Create;
end;

destructor TEvtBenPrRP.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeBenef.Free;
  FDMDev.Free;

  inherited;
end;

procedure TEvtBenPrRP.GerarIdeEstab(objIdeEstab: TIdeEstabCollection);
var
  i: integer;
begin
  for i := 0 to objIdeEstab.Count - 1 do
  begin
    Gerador.wGrupo('ideEstab');

    Gerador.wCampo(tcInt, '', 'tpInsc', 1,  1, 1, eSTpInscricaoToStr(objIdeEstab.Items[i].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc', 1, 15, 1, objIdeEstab.Items[i].nrInsc);

    GerarItensRemun(objIdeEstab.Items[i].ItensRemun, 'itensRemun');

    Gerador.wGrupo('/ideEstab');
  end;

  if objIdeEstab.Count > 500 then
    Gerador.wAlerta('', 'ideEstab', 'Lista de itensRemun', ERR_MSG_MAIOR_MAXIMO + '500');
end;

procedure TEvtBenPrRP.GerarIdePeriodo(objIdePeriodo: TIdePeriodoCollection);
var
  i: integer;
begin
  for i := 0 to objIdePeriodo.Count - 1 do
  begin
    Gerador.wGrupo('idePeriodo');
    
    Gerador.wCampo(tcStr, '', 'perRef', 7, 7, 1, objIdePeriodo.Items[i].perRef);

    GerarIdeEstab(objIdePeriodo.Items[i].ideEstab);

    Gerador.wGrupo('/idePeriodo');
  end;

  if objIdePeriodo.Count > 180 then
    Gerador.wAlerta('', 'idePeriodo', 'Lista de Periodos', ERR_MSG_MAIOR_MAXIMO + '180');
end;

procedure TEvtBenPrRP.GerarIdeBenef;
begin
  Gerador.wGrupo('ideBenef');

  Gerador.wCampo(tcStr, '', 'cpfBenef', 11, 11, 1, ideBenef.cpfBenef);

  Gerador.wGrupo('/ideBenef');
end;

procedure TEvtBenPrRP.GerarInfoPerAnt(pInfoPerAnt: TInfoPerAnt);
begin
  Gerador.wGrupo('infoPerAnt');

  GerarIdePeriodo(pInfoPerAnt.idePeriodo);

  Gerador.wGrupo('/infoPerAnt');
end;

procedure TEvtBenPrRP.GerarInfoPerApur(pInfoPerApur: TInfoPerApur);
begin
  Gerador.wGrupo('infoPerApur');

  GerarIdeEstab(pInfoPerApur.ideEstab);

  Gerador.wGrupo('/infoPerApur');
end;

procedure TEvtBenPrRP.GerarDmDev;
var
  i: integer;
begin
  for i := 0 to dmDev.Count - 1 do
  begin
    Gerador.wGrupo('dmDev');

    Gerador.wCampo(tcStr, '', 'ideDmDev',    1, 30, 1, dmDev[i].ideDmDev);
    Gerador.wCampo(tcStr, '', 'nrBeneficio', 1, 20, 1, dmDev[i].nrBeneficio);

    if VersaoDF >= veS01_01_00 then
    begin
      if (dmDev[i].indRRA = snfSim) and (dmDev[i].infoRRAInst()) then
      begin
        Gerador.wCampo(tcStr, '', 'indRRA', 1,  1, 1, eSSimNaoFacultativoToStr(dmDev[i].indRRA));

        if (dmDev[i].infoRRAInst()) then
          GerarInfoRRA(dmDev[i].infoRRA);
      end;
    end;

    if (dmDev[i].infoPerApurInst()) then
      GerarInfoPerApur(dmDev[i].infoPerApur);

    if (dmDev[i].infoPerAntInst()) then
      GerarInfoPerAnt(dmDev[i].infoPerAnt);

    Gerador.wGrupo('/dmDev');
  end;

  if dmDev.Count > 999 then
    Gerador.wAlerta('', 'dmDev', 'Lista de Demostrativos', ERR_MSG_MAIOR_MAXIMO + '999');
end;

function TEvtBenPrRP.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtBenPrRP');
    Gerador.wGrupo('evtBenPrRP Id="' + Self.Id + '"');

    GerarIdeEvento3(Self.IdeEvento, True, True, False);
    GerarIdeEmpregador(Self.ideEmpregador);
    GerarIdeBenef;
    GerarDmDev;

    Gerador.wGrupo('/evtBenPrRP');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtBenPrRP');

//    Validar(schevtBenPrRP);
  except
    on e: Exception do
      raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TEvtBenPrRP.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J, K, L: Integer;
  dmDevI: TDMDevCollectionItem;
  ideADVI: TIdeAdvCollectionItem;
  ideEstabI: TIdeEstabCollectionItem;
  ItemRemun: TRubricaCollectionItem;
  idePeriodoI: TIdePeriodoCollectionItem;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtBenPrRP';
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

      sSecao := 'ideBenef';
      ideBenef.cpfBenef := INIRec.ReadString(sSecao, 'cpfBenef', EmptyStr);

      I := 1;
      while (true) do
      begin
        sSecao := 'dmDev' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'ideDmDev', 'FIM');

        if(Length(sFim) <= 0) or (sFim = 'FIM')then
          break;

        dmDevI := dmDev.New;
        dmDevI.ideDmDev := sFim;
        dmDevI.nrBeneficio := INIRec.ReadString(sSecao, 'nrBeneficio', EmptyStr);
        dmDevI.indRRA := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indRRA', EmptyStr));

        sSecao := 'infoRRA'+ IntToStrZero(I, 3);
        dmDevI.infoRRA.tpProcRRA   := eSStrToTpProcRRA(Ok, INIRec.ReadString(sSecao, 'tpProcRRA', EmptyStr));
        dmDevI.infoRRA.nrProcRRA   := INIRec.ReadString(sSecao, 'nrProcRRA', EmptyStr);
        dmDevI.infoRRA.descRRA     := INIRec.ReadString(sSecao, 'descRRA' , EmptyStr);
        dmDevI.infoRRA.qtdMesesRRA := StrToFloatDef(INIRec.ReadString(sSecao, 'qtdMesesRRA','0'),0);

        sSecao := 'despProcJud'+ IntToStrZero(I, 3);
        dmDevI.infoRRA.despProcJud.vlrDespCustas := StrToFloatDef(INIRec.ReadString(sSecao, 'vlrDespCustas', '0'), 0);
        dmDevI.infoRRA.despProcJud.vlrDespAdvogados := StrToFloatDef(INIRec.ReadString(sSecao, 'vlrDespAdvogados', '0'), 0);

        J := 1;
        while (true) do
        begin
          sSecao := 'ideAdv'+ IntToStrZero(I, 3) + IntToStrZero(J, 2);
          sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

          if(Length(sFim) <= 0) or (sFim = 'FIM')then
            break;

          ideADVI := dmDevI.infoRRA.ideAdv.New;
          ideADVI.tpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
          ideADVI.nrInsc := sFim;
          ideADVI.vlrADV := StrToFloatDef(INIRec.ReadString(sSecao, 'vlrAdv', '0'), 0);

          Inc(J);
        end;

        J := 1;
        while (true) do
        begin
          sSecao := 'ideEstab' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
          sFim := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

          if(Length(sFim) <= 0) or (sFim = 'FIM')then
            break;

          ideEstabI := dmDevI.infoPerApur.ideEstab.New;
          ideEstabI.tpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
          ideEstabI.nrInsc := sFim;

          K := 1;
          while (true) do
          begin
            sSecao := 'itensRemun' + IntToStrZero(I, 3) + IntToStrZero(J, 3) + IntToStrZero(K, 3);
            sFim := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

            if(Length(sFim) <= 0) or (sFim = 'FIM')then
              break;

            itemRemun := ideEstabI.itensRemun.New;
            itemRemun.codRubr := sFim;
            itemRemun.ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', EmptyStr);
            itemRemun.qtdRubr    := StrToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', '0'), 0);
            itemRemun.fatorRubr  := StrToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', '0'), 0);
            ItemRemun.vrRubr     := StrToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', '0'), 0);
            ItemRemun.indApurIR  := eSStrToTpindApurIR(Ok, INIRec.ReadString(sSecao, 'indApurIR', '0'));

            Inc(K);
          end;

          Inc(J);
        end;

        J := 1;
        while (true) do
        begin
          sSecao := 'idePeriodo' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
          sFim := INIRec.ReadString(sSecao, 'perRef', 'FIM');

          if(Length(sFim) <= 0) or (sFim = 'FIM')then
            break;

          idePeriodoI := dmDevI.infoPerAnt.idePeriodo.New;
          idePeriodoI.perRef := sFim;

          K := 1;
          while (true) do
          begin
            sSecao := 'ideEstab' + IntToStrZero(I, 3) + IntToStrZero(J, 3) + IntToStrZero(K, 3);
            sFim := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

            if(Length(sFim) <= 0) or (sFim = 'FIM')then
              break;

            ideEstabI := idePeriodoI.ideEstab.New;
            ideEstabI.tpInsc := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
            ideEstabI.nrInsc := sFim;

            L := 1;
            while (true) do
            begin
              sSecao := 'itensRemun'+ IntToStrZero(I, 3) + IntToStrZero(J, 3) + IntToStrZero(K, 3) + IntToStrZero(L, 3);
              sFim := INIRec.ReadString(sSecao, 'codRubr', 'FIM');

              if(Length(sFim) <= 0) or (sFim = 'FIM')then
                break;

              itemRemun := ideEstabI.itensRemun.New;
              itemRemun.codRubr := sFim;
              itemRemun.ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', EmptyStr);
              itemRemun.qtdRubr    := StrToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', '0'), 0);
              itemRemun.fatorRubr  := StrToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', '0'), 0);
              ItemRemun.vrRubr     := StrToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', '0'), 0);
              ItemRemun.indApurIR  := eSStrToTpindApurIR(Ok, INIRec.ReadString(sSecao, 'indApurIR', '0'));

              Inc(L);
            end;

            Inc(K);
          end;

          Inc(J);
        end;

        Inc(I);
      end;

    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

{ TS1207CollectionItem }

constructor TS1207CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento := teS1207;
  FEvtBenPrRP := TEvtBenPrRP.Create(AOwner);
end;

destructor TS1207CollectionItem.Destroy;
begin
  FEvtBenPrRP.Free;

  inherited;
end;

{ TS1207Collection }

function TS1207Collection.Add: TS1207CollectionItem;
begin
  Result := Self.New;
end;

function TS1207Collection.GetItem(Index: integer): TS1207CollectionItem;
begin
  Result := TS1207CollectionItem(inherited Items[Index]);
end;

procedure TS1207Collection.SetItem(Index: integer; Value: TS1207CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1207Collection.New: TS1207CollectionItem;
begin
  Result := TS1207CollectionItem.Create(FACBreSocial);
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
  Result := TIdePeriodoCollectionItem(inherited Items[Index]);
end;

procedure TIdePeriodoCollection.SetItem(Index: integer;
  Value: TIdePeriodoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdePeriodoCollection.New: TIdePeriodoCollectionItem;
begin
  Result := TIdePeriodoCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoPerAnt }

constructor TInfoPerAnt.Create;
begin
  inherited;

  FIdePeriodo := TIdePeriodoCollection.Create;
end;

destructor TInfoPerAnt.Destroy;
begin
  FreeAndNil(FIdePeriodo);

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

{ TIdeEstabCollectionItem }

constructor TIdeEstabCollectionItem.Create;
begin
  inherited Create;

  FItensRemun := TRubricaCollection.Create;
end;

destructor TIdeEstabCollectionItem.Destroy;
begin
  FItensRemun.Free;

  inherited;
end;

{ TIdeEstabCollection }

function TIdeEstabCollection.Add: TIdeEstabCollectionItem;
begin
  Result := Self.New;
end;

function TIdeEstabCollection.GetItem(Index: integer): TIdeEstabCollectionItem;
begin
  Result := TIdeEstabCollectionItem(inherited Items[Index]);
end;

procedure TIdeEstabCollection.SetItem(Index: integer;
  Value: TIdeEstabCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeEstabCollection.New: TIdeEstabCollectionItem;
begin
  Result := TIdeEstabCollectionItem.Create;
  Self.Add(Result);
end;

end.
