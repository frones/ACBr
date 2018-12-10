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
|* 25/04/2018: [MSS] Mário Soares Santos - mario@clinfo.com.br
|*  - Alterações para validação com o XSD (v.2.4.02)
******************************************************************************}

{$I ACBr.inc}

unit pcesS2399;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2399Collection = class;
  TS2399CollectionItem = class;
  TEvtTSVTermino = class;
  TInfoTSVTermino = class;
  TVerbasRescS2399 = class;
  TDmDevCollectionItem = class;
  TDmDevCollection = class;

  TS2399Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS2399CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2399CollectionItem);
  public
    function Add: TS2399CollectionItem;
    property Items[Index: Integer]: TS2399CollectionItem read GetItem write SetItem; default;
  end;

  TS2399CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtTSVTermino : TEvtTSVTermino;
    procedure setEvtTSVTermino(const Value: TEvtTSVTermino);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTSVTermino: TEvtTSVTermino read FEvtTSVTermino write setEvtTSVTermino;
  end;

  TEvtTSVTermino = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabSemVInc : TideTrabSemVinc;
    FInfoTSVTermino: TInfoTSVTermino;
    FACBreSocial: TObject;

    procedure GerarInfoTSVTermino(obj: TInfoTSVTermino);
    procedure GerarVerbasResc(obj: TVerbasRescS2399);
    procedure GerarIdeTrabSemVinc(obj: TIdeTrabSemVinc);
    procedure GerarDmDev(pDmDev: TDmDevCollection);
   public
    constructor Create(AACBreSocial: TObject);overload;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTrabSemVInc: TideTrabSemVinc read FIdeTrabSemVInc write FIdeTrabSemVInc;
    property InfoTSVTermino: TInfoTSVTermino read FInfoTSVTermino write FInfoTSVTermino;
  end;

  TinfoTSVTermino = class(TPersistent)
  private
    FdtTerm : TDateTime;
    FmtvDesligTSV : string;
    FverbasResc : TVerbasRescS2399;
    Fquarentena : TQuarentena;
  public
    constructor Create;
    destructor  Destroy; override;

    property dtTerm : TDateTime read FdtTerm write FdtTerm;
    property mtvDesligTSV : string read FmtvDesligTSV write FmtvDesligTSV;
    property verbasResc : TVerbasRescS2399 read FverbasResc write FverbasResc;
    property quarentena : TQuarentena read Fquarentena write Fquarentena;
  end;

  TDmDevCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDMDevCollectionItem;
    procedure SetItem(Index: Integer; Value: TDMDevCollectionItem);
  public
    constructor Create; reintroduce;

    function Add: TDMDevCollectionItem;
    property Items[Index: Integer]: TDMDevCollectionItem read GetItem write SetItem; default;
  end;

  TDmDevCollectionItem = class(TCollectionItem)
  private
    FIdeDmDev: string;
    FIdeEstabLot: TideEstabLotCollection;
  public
    constructor Create; reintroduce;

    property ideDmDev: string read FIdeDmDev write FIdeDmDev;
    property ideEstabLot: TideEstabLotCollection read FIdeEstabLot write FIdeEstabLot;
  end;

  TVerbasRescS2399 = class(TVerbasResc)
  private
    FDmDev: TDmDevCollection;
  public
    constructor Create; reintroduce;

    property dmDev: TDmDevCollection read FDmDev write FDmDev;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS2399Collection }

function TS2399Collection.Add: TS2399CollectionItem;
begin
  Result := TS2399CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS2399Collection.GetItem(Index: Integer): TS2399CollectionItem;
begin
  Result := TS2399CollectionItem(inherited GetItem(Index));
end;

procedure TS2399Collection.SetItem(Index: Integer; Value: TS2399CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS2399CollectionItem }

constructor TS2399CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS2399;
  FEvtTSVTermino := TEvtTSVTermino.Create(AOwner);
end;

destructor TS2399CollectionItem.Destroy;
begin
  FEvtTSVTermino.Free;

  inherited;
end;

procedure TS2399CollectionItem.setEvtTSVTermino(const Value: TEvtTSVTermino);
begin
  FEvtTSVTermino.Assign(Value);
end;

{ TinfoTSVTermino }

constructor TinfoTSVTermino.Create;
begin
  inherited;

  FverbasResc := TVerbasRescS2399.Create;
  Fquarentena := TQuarentena.Create;
end;

destructor TinfoTSVTermino.Destroy;
begin
  FverbasResc.Free;
  Fquarentena.Free;

  inherited;
end;

{ TDmDevCollection }

constructor TDmDevCollection.Create;
begin
  inherited Create(TDMDevCollectionItem);
end;

function TDmDevCollection.Add: TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem(inherited Add);
  Result.Create;
end;

function TDmDevCollection.GetItem(Index: Integer): TDMDevCollectionItem;
begin
  Result := TDMDevCollectionItem(inherited GetItem(Index));
end;

procedure TDmDevCollection.SetItem(Index: Integer; Value: TDMDevCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDMDevCollectionItem }

constructor TDMDevCollectionItem.Create;
begin
  FIdeEstabLot := TideEstabLotCollection.Create;
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
  inherited;

  FACBreSocial := AACBreSocial;
  FIdeEvento := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeTrabSemVInc := TideTrabSemVinc.Create;
  FInfoTSVTermino := TInfoTSVTermino.Create;
end;

destructor TEvtTSVTermino.destroy;
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
  Gerador.wCampo(tcStr, '', 'nisTrab',   1, 11, 0, obj.nisTrab);
  Gerador.wCampo(tcStr, '', 'codCateg',  1,  3, 1, obj.codCateg);

  Gerador.wGrupo('/ideTrabSemVinculo');
end;

procedure TEvtTSVTermino.GerarInfoTSVTermino(obj: TInfoTSVTermino);
begin
  Gerador.wGrupo('infoTSVTermino');

  Gerador.wCampo(tcDat, '', 'dtTerm',       10, 10, 1, obj.dtTerm);
  Gerador.wCampo(tcStr, '', 'mtvDesligTSV',  1,  2, 0, obj.mtvDesligTSV);

  GerarVerbasResc(obj.verbasResc);
//  GerarRemunOutrEmpr(obj.verbasResc.infoMV.remunOutrEmpr);
  GerarQuarentena(obj.quarentena);

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

    GerarIdeEstabLot(pDmDev[i].ideEstabLot);

    Gerador.wGrupo('/dmDev');
  end;

  if pDmDev.Count > 50 then
    Gerador.wAlerta('', 'dmDev', 'Lista de Demonstrativos', ERR_MSG_MAIOR_MAXIMO + '50');
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
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTSVTermino');
    Gerador.wGrupo('evtTSVTermino Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeTrabSemVinc(self.IdeTrabSemVInc);
    GerarInfoTSVTermino(Self.InfoTSVTermino);

    Gerador.wGrupo('/evtTSVTermino');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTSVTermino');

    Validar(schevtTSVTermino);
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
  Result := False;

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
      ideEvento.TpAmb       := eSStrTotpAmb(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
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

      sSecao := 'infoTSVTermino';
      infoTSVTermino.dtTerm       := StringToDateTime(INIRec.ReadString(sSecao, 'dtTerm', '0'));
      infoTSVTermino.mtvDesligTSV := INIRec.ReadString(sSecao, 'mtvDesligTSV', '');

      I := 1;
      while true do
      begin
        // de 01 até 50
        sSecao := 'dmDev' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'ideDmDev', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infoTSVTermino.verbasResc.dmDev.Add do
        begin
          ideDmDev := sFim;

          J := 1;
          while true do
          begin
            // de 01 até 99
            sSecao := 'ideEstabLot' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'nrInsc', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with ideEstabLot.Add do
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

                with detVerbas.Add do
                begin
                  codRubr    := sFim;
                  ideTabRubr := INIRec.ReadString(sSecao, 'ideTabRubr', '');
                  qtdRubr    := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdRubr', ''), 0);
                  fatorRubr  := StringToFloatDef(INIRec.ReadString(sSecao, 'fatorRubr', ''), 0);
                  vrUnit     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrUnit', ''), 0);
                  vrRubr     := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRubr', ''), 0);

                  L := 1;
                  while true do
                  begin
                    // de 01 até 99
                    sSecao := 'detOper' + IntToStrZero(I, 2) + IntToStrZero(J, 2) +
                             IntToStrZero(K, 3) + IntToStrZero(L, 2);
                    sFim   := INIRec.ReadString(sSecao, 'cnpjOper', 'FIM');

                    if (sFim = 'FIM') or (Length(sFim) <= 0) then
                      break;

                    with infoSaudeColet.detOper.Add do
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

                        with detPlano.Add do
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

                  sSecao := 'infoSimples' + IntToStrZero(I, 2) +
                                    IntToStrZero(J, 2) + IntToStrZero(K, 3);
                  if INIRec.ReadString(sSecao, 'indSimples', '') <> '' then
                    infoSimples.indSimples := eSStrToIndSimples(Ok, INIRec.ReadString(sSecao, 'indSimples', '1'));
                end;

                Inc(K);
              end;

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

        with infoTSVTermino.verbasResc.procJudTrab.Add do
        begin
          tpTrib    := eSStrToTpTributo(Ok, sFim);
          nrProcJud := INIRec.ReadString(sSecao, 'nrProcJud', EmptyStr);
          codSusp   := INIRec.ReadInteger(sSecao, 'codSusp', 0);
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
          sSecao := 'remunOutrEmpr' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoTSVTermino.VerbasResc.infoMV.remunOutrEmpr.Add do
          begin
            TpInsc     := eSStrToTpInscricao(Ok, sFim);
            NrInsc     := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
            codCateg   := INIRec.ReadInteger(sSecao, 'codCateg', 0);
            vlrRemunOE := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRemunOE', ''), 0);
          end;

          Inc(I);
        end;
      end;

      sSecao := 'quarentena';
      if INIRec.ReadString(sSecao, 'dtFimQuar', '') <> '' then
        infoTSVTermino.quarentena.dtFimQuar := StringToDateTime(INIRec.ReadString(sSecao, 'dtFimQuar', '0'));
    end;

    GerarXML;

    Result := True;
  finally
     INIRec.Free;
  end;
end;

end.
