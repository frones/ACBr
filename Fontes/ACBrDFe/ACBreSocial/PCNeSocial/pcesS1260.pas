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

unit pcesS1260;

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
  TS1260CollectionItem = class;
  TEvtComProd = class;
  TInfoComProd=class;
  TIdeEstabel=class;
  TTpComercItem = class;
  TTpComercColecao = class;
  TIdeAdquirItem = class;
  TIdeAdquirColecao = class;

  TS1260Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1260CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1260CollectionItem);
  public
    function Add: TS1260CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1260CollectionItem;
    property Items[Index: Integer]: TS1260CollectionItem read GetItem write SetItem; default;
  end;

  TS1260CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtComProd: TEvtComProd;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtComProd: TEvtComProd read FEvtComProd write FEvtComProd;
  end;

  TEvtComProd = class(TESocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FInfoComProd: TInfoComProd;

    {Geradores específicos da classe}
    procedure GerarInfoComProd;
    procedure GerarIdeEstabel;
    procedure GerarTpComerc(pTpComerc: TTpComercColecao);
    procedure GerarIdeAdquir(pIdeAdquir: TIdeAdquirColecao);
    procedure GerarInfoProcJud(pInfoProcJud: TInfoProcJudCollection);
    procedure GerarNfs(pNfs: TNfsColecao);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento3 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property InfoComProd: TInfoComProd read FInfoComProd write FInfoComProd;
  end;

  TInfoComProd=class(TObject)
  private
    FIdeEstabel: TIdeEstabel;
  public
    constructor Create;
    destructor Destroy; override;

    property IdeEstabel: TIdeEstabel read FIdeEstabel write FIdeEstabel;
  end;

  TIdeEstabel=class(TObject)
  private
    FnrInscEstabRural: string;
    FTpComerc: TTpComercColecao;
  public
    constructor Create;
    destructor Destroy; override;

    property nrInscEstabRural: string read FnrInscEstabRural write FnrInscEstabRural;
    property TpComerc: TTpComercColecao read FTpComerc write FTpComerc;
  end;

  TTpComercColecao = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TTpComercItem;
    procedure SetItem(Index: Integer; const Value: TTpComercItem);
  public
    function Add: TTpComercItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TTpComercItem;
    property Items[Index: Integer]: TTpComercItem read GetItem write SetItem;
  end;

  TTpComercItem = class(TObject)
  private
    FindComerc: tpIndComerc;
    FvrTotCom: Double;
    FIdeAdquir: TIdeAdquirColecao;
    FInfoProcJud: TInfoProcJudCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property indComerc: tpIndComerc read FindComerc write FindComerc;
    property vrTotCom: double read FvrTotCom write FvrTotCom;
    property IdeAdquir: TIdeAdquirColecao read FIdeAdquir write FIdeAdquir;
    property InfoProcJud: TInfoProcJudCollection read FInfoProcJud write FInfoProcJud;
  end;

  TIdeAdquirColecao = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeAdquirItem;
    procedure SetItem(Index: Integer; const Value: TIdeAdquirItem);
  public
    function Add: TIdeAdquirItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeAdquirItem;
    property Items[Index: Integer]: TIdeAdquirItem read GetItem write SetItem;
  end;

  TIdeAdquirItem = class(TObject)
  private
    FtpInsc: tpTpInsc;
    FnrInsc: string;
    FvrComerc: Double;
    FvrRetPR: Double;
    FNfs: TNfsColecao;
    function getNfs: TNfsColecao;
  public
    constructor Create;
    destructor Destroy; override;
    function nfsInst: boolean;

    property tpInsc: tpTpInsc read FtpInsc write FtpInsc;
    property nrInsc: string read FnrInsc write FnrInsc;
    property vrComerc: Double read FvrComerc write FvrComerc;
    property vrRetPR: Double read FvrRetPR write FvrRetPR;
    property nfs: TNfsColecao read getNfs write FNfs;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS1260Collection }

function TS1260Collection.Add: TS1260CollectionItem;
begin
  Result := Self.New;
end;

function TS1260Collection.GetItem(Index: Integer): TS1260CollectionItem;
begin
  Result := TS1260CollectionItem(inherited Items[Index]);
end;

procedure TS1260Collection.SetItem(Index: Integer; Value: TS1260CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1260Collection.New: TS1260CollectionItem;
begin
  Result := TS1260CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{TS1260CollectionItem}
constructor TS1260CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS1260;
  FEvtComProd := TEvtComProd.Create(AOwner);
end;

destructor TS1260CollectionItem.Destroy;
begin
  FEvtComProd.Free;

  inherited;
end;

{ TTpComercColecao }
function TTpComercColecao.Add: TTpComercItem;
begin
  Result := Self.New;
end;

function TTpComercColecao.GetItem(Index: Integer): TTpComercItem;
begin
  Result := TTpComercItem(inherited Items[Index]);
end;

procedure TTpComercColecao.SetItem(Index: Integer; const Value: TTpComercItem);
begin
  inherited Items[Index] := Value;
end;

function TTpComercColecao.New: TTpComercItem;
begin
  Result := TTpComercItem.Create;
  Self.Add(Result);
end;

{ TInfoComProd }
constructor TInfoComProd.Create;
begin
  inherited;

  FIdeEstabel := TIdeEstabel.Create;
end;

destructor TInfoComProd.Destroy;
begin
  FIdeEstabel.Free;

  inherited;
end;

{ TTpComercItem }
constructor TTpComercItem.Create;
begin
  inherited Create;
  FIdeAdquir   := TIdeAdquirColecao.Create;
  FInfoProcJud := TInfoProcJudCollection.Create;
end;

destructor TTpComercItem.Destroy;
begin
  FIdeAdquir.Free;
  FInfoProcJud.Free;

  inherited;
end;

{ TIdeAdquirColecao }
function TIdeAdquirColecao.Add: TIdeAdquirItem;
begin
  Result := Self.New;
end;

function TIdeAdquirColecao.GetItem(Index: Integer): TIdeAdquirItem;
begin
  Result := TIdeAdquirItem(inherited Items[Index]);
end;

procedure TIdeAdquirColecao.SetItem(Index: Integer;
  const Value: TIdeAdquirItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeAdquirColecao.New: TIdeAdquirItem;
begin
  Result := TIdeAdquirItem.Create;
  Self.Add(Result);
end;

{ TIdeAdquirItem }

constructor TIdeAdquirItem.Create;
begin
  inherited Create;
  FNfs := nil;
end;

destructor TIdeAdquirItem.Destroy;
begin
  FreeAndNil(FNfs);

  inherited;
end;

function TIdeAdquirItem.getNfs: TNfsColecao;
begin
  if not Assigned(FNfs) then
    FNfs := TNfsColecao.Create;
  Result := FNfs;
end;

function TIdeAdquirItem.nfsInst: boolean;
begin
  result := Assigned(FNfs);
end;

{ TIdeEstabel }
constructor TIdeEstabel.Create;
begin
  inherited;

  FTpComerc := TTpComercColecao.Create;
end;

destructor TIdeEstabel.Destroy;
begin
  FTpComerc.Free;

  inherited;
end;

{ TEvtComProd }
constructor TEvtComProd.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento3.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoComProd   := TInfoComProd.create;
end;

destructor TEvtComProd.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoComProd.Free;

  inherited;
end;

procedure TEvtComProd.GerarInfoComProd;
begin
  Gerador.wGrupo('infoComProd');

  GerarIdeEstabel;

  Gerador.wGrupo('/infoComProd');
end;

procedure TEvtComProd.GerarIdeEstabel;
begin
  Gerador.wGrupo('ideEstabel');

  Gerador.wCampo(tcStr, '', 'nrInscEstabRural', 1, 15, 1, InfoComProd.IdeEstabel.nrInscEstabRural);

  GerarTpComerc(InfoComProd.IdeEstabel.TpComerc);

  Gerador.wGrupo('/ideEstabel');
end;

procedure TEvtComProd.GerarNfs(pNfs: TNfsColecao);
var
  i: integer;
begin
  for i := 0 to pNfs.Count - 1 do
  begin
    Gerador.wGrupo('nfs');
    Gerador.wCampo(tcStr, '', 'serie',        1,  5, 0, pNfs.Items[i].serie);
    Gerador.wCampo(tcStr, '', 'nrDocto',      1, 20, 1, pNfs.Items[i].nrDocto);
    Gerador.wCampo(tcDat, '', 'dtEmisNF',    10, 10, 1, pNfs.Items[i].dtEmisNF);
    Gerador.wCampo(tcDe2, '', 'vlrBruto',     1, 14, 1, pNfs.Items[i].vlrBruto);
    Gerador.wCampo(tcDe2, '', 'vrCPDescPR',   1, 14, 1, pNfs.Items[i].vrCPDescPR);
    Gerador.wCampo(tcDe2, '', 'vrRatDescPR',  1, 14, 1, pNfs.Items[i].vrRatDescPR);
    Gerador.wCampo(tcDe2, '', 'vrSenarDesc',  1, 14, 1, pNfs.Items[i].vrSenarDesc);
    Gerador.wGrupo('/nfs');
  end;

  if pNfs.Count > 9999 then
    Gerador.wAlerta('', 'nfs', 'Lista de Notas Fiscais', ERR_MSG_MAIOR_MAXIMO + '9999');
end;

procedure TEvtComProd.GerarIdeAdquir(pIdeAdquir: TIdeAdquirColecao);
var
  i: integer;
begin
  for i := 0 to pIdeAdquir.Count - 1 do
  begin
    Gerador.wGrupo('ideAdquir');

    Gerador.wCampo(tcStr, '', 'tpInsc',   1,  1, 1, eSTpInscricaoToStr(pIdeAdquir.Items[i].tpInsc));
    Gerador.wCampo(tcStr, '', 'nrInsc',   1, 15, 1, pIdeAdquir.Items[i].nrInsc);
    Gerador.wCampo(tcDe2, '', 'vrComerc', 1, 14, 1, pIdeAdquir.Items[i].vrComerc);

    if pIdeAdquir.Items[i].nfsInst() then
      GerarNfs(pIdeAdquir.Items[i].nfs);

    Gerador.wGrupo('/ideAdquir');
  end;

  if pIdeAdquir.Count > 9999 then
    Gerador.wAlerta('', 'ideAdquir', 'Lista de Adquirentes de Produção', ERR_MSG_MAIOR_MAXIMO + '9999');
end;

procedure TEvtComProd.GerarInfoProcJud(pInfoProcJud: TInfoProcJudCollection);
var
  i : Integer;
begin
  for i := 0 to pInfoProcJud.Count - 1 do
  begin
    Gerador.wGrupo('infoProcJud');

    Gerador.wCampo(tcStr, '', 'tpProc',      1,  1, 1, pInfoProcJud.Items[i].tpProc);
    Gerador.wCampo(tcStr, '', 'nrProc',      1, 20, 1, pInfoProcJud.Items[i].nrProcJud);
    Gerador.wCampo(tcInt, '', 'codSusp',     1, 14, 1, pInfoProcJud.Items[i].codSusp);
    Gerador.wCampo(tcDe2, '', 'vrCPSusp',    1, 14, 0, pInfoProcJud.Items[i].vrCPSusp);
    Gerador.wCampo(tcDe2, '', 'vrRatSusp',   1, 14, 0, pInfoProcJud.Items[i].vrRatSusp);
    Gerador.wCampo(tcDe2, '', 'vrSenarSusp', 1, 14, 0, pInfoProcJud.Items[i].vrSenarSusp);

    Gerador.wGrupo('/infoProcJud');
  end;

  if pInfoProcJud.Count > 10 then
    Gerador.wAlerta('', 'infoProcJud', 'Lista de Informações de Processos', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TEvtComProd.GerarTpComerc(pTpComerc: TTpComercColecao);
var
  i: Integer;
begin
  for i := 0 to pTpComerc.Count - 1 do
  begin
    Gerador.wGrupo('tpComerc');

    Gerador.wCampo(tcStr, '', 'indComerc', 1,  1, 1, eSIndComercStr(pTpComerc.Items[i].indComerc));
    Gerador.wCampo(tcDe2, '', 'vrTotCom',  1, 14, 1, pTpComerc.Items[i].vrTotCom);

    GerarIdeAdquir(pTpComerc.Items[i].IdeAdquir);
    GerarInfoProcJud(pTpComerc.Items[i].InfoProcJud);

    Gerador.wGrupo('/tpComerc');
  end;

  if pTpComerc.Count > 5 then
    Gerador.wAlerta('', 'tpComerc', 'Lista de Comercialização', ERR_MSG_MAIOR_MAXIMO + '5');
end;

function TEvtComProd.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtComProd');
    Gerador.wGrupo('evtComProd Id="' + Self.Id + '"');

    if VersaoDF <= ve02_05_00 then
      GerarIdeEvento3(self.IdeEvento, True, True, False)
    else
      GerarIdeEvento3(self.IdeEvento, True, False, True);

    GerarIdeEmpregador(self.IdeEmpregador);
    GerarInfoComProd;

    Gerador.wGrupo('/evtComProd');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtComProd');

//    Validar(schevtComProd);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtComProd.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtComProd';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.IndApuracao := eSStrToIndApuracao(Ok, INIRec.ReadString(sSecao, 'indApuracao', '1'));
      ideEvento.perApur     := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      IdeEvento.indGuia     := INIRec.ReadString(sSecao, 'indGuia', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideEstabel';
      InfoComProd.IdeEstabel.nrInscEstabRural := INIRec.ReadString(sSecao, 'nrInscEstabRural', '');

      I := 1;
      while true do
      begin
        // de 1 até 4
        sSecao := 'tpComerc' + IntToStrZero(I, 1);
        sFim   := INIRec.ReadString(sSecao, 'indComerc', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with InfoComProd.IdeEstabel.tpComerc.New do
        begin
          indComerc := eSStrToIndComerc(Ok, sFim);
          vrTotCom  := StringToFloatDef(INIRec.ReadString(sSecao, 'vrTotCom', ''), 0);

          J := 1;
          while true do
          begin
            // de 0000 até 9999
            sSecao := 'ideAdquir' + IntToStrZero(I, 1) + IntToStrZero(J, 4);
            sFim   := INIRec.ReadString(sSecao, 'tpInsc', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with ideAdquir.New do
            begin
              tpInsc   := eSStrToTpInscricao(Ok, sFim);
              nrInsc   := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
              vrComerc := StringToFloatDef(INIRec.ReadString(sSecao, 'vrComerc', ''), 0);

              K := 1;
              while true do
              begin
                // de 000 até 999
                sSecao := 'nfs' + IntToStrZero(I, 1) + IntToStrZero(J, 4) +
                               IntToStrZero(K, 3);
                sFim   := INIRec.ReadString(sSecao, 'serie', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with nfs.New do
                begin
                  serie       := sFim;
                  nrDocto     := INIRec.ReadString(sSecao, 'nrDocto', EmptyStr);
                  dtEmisNF    := StringToDateTime(INIRec.ReadString(sSecao, 'dtEmisNF', '0'));
                  vlrBruto    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBruto', ''), 0);
                  vrCPDescPR  := StringToFloatDef(INIRec.ReadString(sSecao, 'vrCPDescPR', ''), 0);
                  vrRatDescPR := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRatDescPR', ''), 0);
                  vrSenarDesc := StringToFloatDef(INIRec.ReadString(sSecao, 'vrSenarDesc', ''), 0);
                end;

                Inc(K);
              end;

            end;

            Inc(J);
          end;

          J := 1;
          while true do
          begin
            // de 00 até 10
            sSecao := 'infoProcJud' + IntToStrZero(I, 1) + IntToStrZero(J, 2);
            sFim   := INIRec.ReadString(sSecao, 'nrProc', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with infoProcJud.New do
            begin
              tpProc      := eSStrToTpProcesso(Ok, INIRec.ReadString(sSecao, 'tpProc', '1'));
              nrProcJud   := sFim;
              codSusp     := INIRec.ReadInteger(sSecao, 'codSusp', 0);
              vrCPSusp    := StringToFloatDef(INIRec.ReadString(sSecao, 'vrCPSusp', ''), 0);
              vrRatSusp   := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRatSusp', ''), 0);
              vrSenarSusp := StringToFloatDef(INIRec.ReadString(sSecao, 'vrSenarSusp', ''), 0);
            end;

            Inc(J);
          end;

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

end.
