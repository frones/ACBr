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

unit pcesS1250;

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
  TS1250CollectionItem = class;
  TEvtAqProd = class;
  TInfoAquisProd=class;
  TIdeEstabAdquir=class;
  TTpAquisItem = class;
  TTpAquisColecao = class;
  TIdeProdutorItem = class;
  TIdeProdutorColecao = class;

  TS1250Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1250CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1250CollectionItem);
  public
    function Add: TS1250CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1250CollectionItem;
    property Items[Index: Integer]: TS1250CollectionItem read GetItem write SetItem; default;
  end;

  TS1250CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtAqProd: TEvtAqProd;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtAqProd: TEvtAqProd read FEvtAqProd write FEvtAqProd;
  end;

  TEvtAqProd = class(TESocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FInfoAquisProd: TInfoAquisProd;

    {Geradores específicos da classe}
    procedure GerarInfoAquisProd;
    procedure GerarIdeEstabAdquir;
    procedure GerarTpAquis(pTpAquis: TTpAquisColecao);
    procedure GerarIdeProdutor(pIdeProdutor: TIdeProdutorColecao);
    procedure GerarInfoProcJud(pInfoProcJud: TInfoProcJudCollection);
    procedure GerarInfoProcJ(pInfoProcJ: TInfoProcJCollection);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;
    property IdeEvento: TIdeEvento3 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property InfoAquisProd: TInfoAquisProd read FInfoAquisProd write FInfoAquisProd;
  end;

  TInfoAquisProd=class(TPersistent)
  private
    FIdeEstabAdquir: TIdeEstabAdquir;
  public
    constructor create;
    destructor Destroy; override;

    property IdeEstabAdquir: TIdeEstabAdquir read FIdeEstabAdquir write FIdeEstabAdquir;
  end;

  TIdeEstabAdquir=class(TPersistent)
  private
    FtpInscAdq: tpTpInsc;
    FnrInscAdq: string;
    FTpAquis: TTpAquisColecao;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscAdq: tpTpInsc read FtpInscAdq write FtpInscAdq;
    property nrInscAdq: string read FnrInscAdq write FnrInscAdq;
    property TpAquis: TTpAquisColecao read FTpAquis write FTpAquis;
  end;

  TTpAquisColecao = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TTpAquisItem;
    procedure SetItem(Index: Integer; const Value: TTpAquisItem);
  public
    function Add: TTpAquisItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TTpAquisItem;
    property Items[Index: Integer]: TTpAquisItem read GetItem write SetItem;
  end;

  TTpAquisItem = class(TObject)
  private
    FindAquis: tpIdAquis;
    FvlrTotAquis: Double;
    FIdeProdutor: TIdeProdutorColecao;
    FInfoProcJ: TInfoProcJCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property indAquis: tpIdAquis read FindAquis write FindAquis;
    property vlrTotAquis: double read FvlrTotAquis write FvlrTotAquis;
    property IdeProdutor: TIdeProdutorColecao read FIdeProdutor write FIdeProdutor;
    property InfoProcJ: TInfoProcJCollection read FInfoProcJ write FInfoProcJ;
  end;

  TIdeProdutorColecao = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeProdutorItem;
    procedure SetItem(Index: Integer; const Value: TIdeProdutorItem);
  public
    function Add: TIdeProdutorItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeProdutorItem;
    property Items[Index: Integer]: TIdeProdutorItem read GetItem write SetItem;
  end;

  TIdeProdutorItem = class(TObject)
  private
    FtpInscProd: tpTpInsc;
    FnrInscProd: string;
    FvlrBruto: Double;
    FvrCPDescPR: Double;
    FvrRatDescPR: Double;
    FvrSenarDesc: Double;
    FIndOpcCP: TpIndOpcCP;

    FNfs: TNfsColecao;
    FInfoProcJud: TInfoProcJudCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscProd: tpTpInsc read FtpInscProd write FtpInscProd;
    property nrInscProd: string read FnrInscProd write FnrInscProd;
    property vlrBruto: Double read FvlrBruto write FvlrBruto;
    property vrCPDescPR: Double read FvrCPDescPR write FvrCPDescPR;
    property vrRatDescPR: Double read FvrRatDescPR write FvrRatDescPR;
    property vrSenarDesc: Double read FvrSenarDesc write FvrSenarDesc;
    property IndOpcCP: TpIndOpcCP read FIndOpcCP write FIndOpcCP;

    property Nfs: TNfsColecao read FNfs write FNfs;
    property InfoProcJud: TInfoProcJudCollection read FInfoProcJud write FInfoProcJud;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS1250Collection }

function TS1250Collection.Add: TS1250CollectionItem;
begin
  Result := Self.New;
end;

function TS1250Collection.GetItem(Index: Integer): TS1250CollectionItem;
begin
  Result := TS1250CollectionItem(inherited Items[Index]);
end;

procedure TS1250Collection.SetItem(Index: Integer;
  Value: TS1250CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1250Collection.New: TS1250CollectionItem;
begin
  Result := TS1250CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{TS1250CollectionItem}
constructor TS1250CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS1250;
  FEvtAqProd  := TEvtAqProd.Create(AOwner);
end;

destructor TS1250CollectionItem.Destroy;
begin
  FEvtAqProd.Free;

  inherited;
end;

{ TEvtContratAvNP }
constructor TEvtAqProd.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento3.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoAquisProd := TInfoAquisProd.create;
end;

destructor TEvtAqProd.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoAquisProd.Free;

  inherited;
end;

procedure TEvtAqProd.GerarInfoAquisProd;
begin
  Gerador.wGrupo('infoAquisProd');

  GerarIdeEstabAdquir;

  Gerador.wGrupo('/infoAquisProd');
end;

procedure TEvtAqProd.GerarIdeEstabAdquir;
begin
  Gerador.wGrupo('ideEstabAdquir');

  Gerador.wCampo(tcStr, '', 'tpInscAdq', 1,  1, 1, eSTpInscricaoToStr(InfoAquisProd.IdeEstabAdquir.tpInscAdq));
  Gerador.wCampo(tcStr, '', 'nrInscAdq', 1, 15, 0, InfoAquisProd.IdeEstabAdquir.nrInscAdq);

  GerarTpAquis(InfoAquisProd.IdeEstabAdquir.TpAquis);

  Gerador.wGrupo('/ideEstabAdquir');
end;

procedure TEvtAqProd.GerarIdeProdutor(pIdeProdutor: TIdeProdutorColecao);
var
  i: integer;
begin
  for i := 0 to pIdeProdutor.Count - 1 do
  begin
    if VersaoDF < ve02_05_00 then
    begin
      Gerador.wGrupo('ideProdutor');
      Gerador.wCampo(tcStr, '', 'tpInscProd',  1,  1, 1, eSTpInscricaoToStr(pIdeProdutor.Items[i].tpInscProd));
      Gerador.wCampo(tcStr, '', 'nrInscProd',  1, 14, 1, pIdeProdutor.Items[i].nrInscProd);
      Gerador.wCampo(tcDe2, '', 'vlrBruto',    1, 14, 1, pIdeProdutor.Items[i].vlrBruto);
      Gerador.wCampo(tcDe2, '', 'vrCPDescPR',  1, 14, 1, pIdeProdutor.Items[i].vrCPDescPR);
      Gerador.wCampo(tcDe2, '', 'vrRatDescPR', 1, 14, 1, pIdeProdutor.Items[i].vrRatDescPR);
      Gerador.wCampo(tcDe2, '', 'vrSenarDesc', 1, 14, 1, pIdeProdutor.Items[i].vrSenarDesc);
    end;


    if VersaoDF >= ve02_05_00 then
    begin
      Gerador.wGrupo('ideProdutor tpInscProd="' + eSTpInscricaoToStr(pIdeProdutor.Items[i].tpInscProd) + '"' +
                                ' nrInscProd="' + pIdeProdutor.Items[i].nrInscProd + '"' +
                                ' vlrBruto="' + FloatToString(pIdeProdutor.Items[i].vlrBruto, '.', FloatMask(2, False)) + '"' +
                                ' vrCPDescPR="' + FloatToString(pIdeProdutor.Items[i].vrCPDescPR, '.', FloatMask(2, False)) + '"' +
                                ' vrRatDescPR="' + FloatToString(pIdeProdutor.Items[i].vrRatDescPR, '.', FloatMask(2, False)) + '"' +
                                ' vrSenarDesc="' + FloatToString(pIdeProdutor.Items[i].vrSenarDesc, '.', FloatMask(2, False)) + '"' +
                                ' indOpcCP="' + eSIndOpcCPToStr(pIdeProdutor.Items[i].IndOpcCp) + '"');

    end;

    GerarNfs(pIdeProdutor.Items[i].Nfs);
    GerarInfoProcJud(pIdeProdutor.Items[i].InfoProcJud);

    Gerador.wGrupo('/ideProdutor');
  end;

  if pIdeProdutor.Count > 14999 then
    Gerador.wAlerta('', 'ideProdutor', 'Lista de Produtores', ERR_MSG_MAIOR_MAXIMO + '14999');
end;

procedure TEvtAqProd.GerarInfoProcJ(pInfoProcJ: TInfoProcJCollection);
var
  i : integer;
begin
  for i := 0 to pInfoProcJ.Count - 1 do
  begin
    Gerador.wGrupo('infoProcJ nrProcJud="' + pInfoProcJ.Items[i].nrProcJud + '"' +
                                ' codSusp="' + IntToStr(pInfoProcJ.Items[i].codSusp) + '"' +
                                ' vrCPNRet="' + FloatToString(pInfoProcJ.Items[i].vrCPNRet, '.', FloatMask(2, False)) + '"' +
                                ' vrRatNRet="' + FloatToString(pInfoProcJ.Items[i].vrRatNRet, '.', FloatMask(2, False)) + '"' +
                                ' vrSenarNRet="' + FloatToString(pInfoProcJ.Items[i].vrSenarNRet, '.', FloatMask(2, False)) + '"');

    Gerador.wGrupo('/infoProcJ');
  end;

  if pInfoProcJ.Count > 10 then
    Gerador.wAlerta('', 'infoProcJ', 'Lista de Processos Judiciais', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TEvtAqProd.GerarInfoProcJud(pInfoProcJud: TInfoProcJudCollection);
var
  i : integer;
begin
  for i := 0 to pInfoProcJud.Count - 1 do
  begin
    if VersaoDF < ve02_05_00 then
    begin
      Gerador.wGrupo('infoProcJud');

      Gerador.wCampo(tcStr, '', 'nrProcJud',   1, 20, 1, pInfoProcJud.Items[i].nrProcJud);
      Gerador.wCampo(tcInt, '', 'codSusp',     1, 14, 1, pInfoProcJud.Items[i].codSusp);
      Gerador.wCampo(tcDe2, '', 'vrCPNRet',    1, 14, 1, pInfoProcJud.Items[i].vrCPNRet);
      Gerador.wCampo(tcDe2, '', 'vrRatNRet',   1, 14, 1, pInfoProcJud.Items[i].vrRatNRet);
      Gerador.wCampo(tcDe2, '', 'vrSenarNRet', 1, 14, 1, pInfoProcJud.Items[i].vrSenarNRet);
    end;

    if VersaoDF >= ve02_05_00 then
    begin
      Gerador.wGrupo('infoProcJud nrProcJud="' + pInfoProcJud.Items[i].nrProcJud + '"' +
                                ' codSusp="' + IntToStr(pInfoProcJud.Items[i].codSusp) + '"' +
                                ' vrCPNRet="' + FloatToString(pInfoProcJud.Items[i].vrCPNRet, '.', FloatMask(2, False)) + '"' +
                                ' vrRatNRet="' + FloatToString(pInfoProcJud.Items[i].vrRatNRet, '.', FloatMask(2, False)) + '"' +
                                ' vrSenarNRet="' + FloatToString(pInfoProcJud.Items[i].vrSenarNRet, '.', FloatMask(2, False)) + '"');

    end;

    Gerador.wGrupo('/infoProcJud');
  end;

  if pInfoProcJud.Count > 10 then
    Gerador.wAlerta('', 'infoProcJud', 'Lista de Processos Judiciais', ERR_MSG_MAIOR_MAXIMO + '10');
end;

procedure TEvtAqProd.GerarTpAquis(pTpAquis: TTpAquisColecao);
var
  i: Integer;
begin
  for i := 0 to pTpAquis.Count - 1 do
  begin
    if (VersaoDF < ve02_05_00) then
    begin
      Gerador.wGrupo('tpAquis');

      Gerador.wCampo(tcStr, '', 'indAquis',    1,  1, 1, eSIdAquisStr(pTpAquis.Items[i].indAquis));
      Gerador.wCampo(tcDe2, '', 'vlrTotAquis', 1, 14, 1, pTpAquis.Items[i].vlrTotAquis);

      GerarIdeProdutor(pTpAquis.Items[i].IdeProdutor);
    end;

    if (VersaoDF >= ve02_05_00) then
    begin
      Gerador.wGrupo('tpAquis indAquis="' + eSIdAquisStr(pTpAquis.Items[i].indAquis) + '"' +
                            ' vlrTotAquis="' + FloatToString(pTpAquis.Items[i].vlrTotAquis, '.', FloatMask(2, False)) + '"');

      GerarIdeProdutor(pTpAquis.Items[i].IdeProdutor);

      GerarInfoProcJ(pTpAquis.Items[i].InfoProcJ);
    end;

    Gerador.wGrupo('/tpAquis');
  end;

  if pTpAquis.Count > 3 then
    Gerador.wAlerta('', 'tpAquis', 'Lista de Aquisições', ERR_MSG_MAIOR_MAXIMO + '3');
end;

function TEvtAqProd.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtAqProd');
    Gerador.wGrupo('evtAqProd Id="' + Self.Id + '"');

    GerarIdeEvento3(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarInfoAquisProd;

    Gerador.wGrupo('/evtAqProd');
    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAqProd');

//    Validar(schevtAqProd);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

{ TTpAquisColecao }
function TTpAquisColecao.Add: TTpAquisItem;
begin
  Result := Self.New;
end;

function TTpAquisColecao.GetItem(Index: Integer): TTpAquisItem;
begin
  Result := TTpAquisItem(inherited Items[Index]);
end;

procedure TTpAquisColecao.SetItem(Index: Integer; const Value: TTpAquisItem);
begin
  inherited Items[Index] := Value;
end;

function TTpAquisColecao.New: TTpAquisItem;
begin
  Result := TTpAquisItem.Create;
  Self.Add(Result);
end;

{ TInfoAquisProd }
constructor TInfoAquisProd.create;
begin
  inherited;

  FIdeEstabAdquir := TIdeEstabAdquir.Create;
end;

destructor TInfoAquisProd.destroy;
begin
  FIdeEstabAdquir.Free;

  inherited;
end;

{ TTpAquisItem }
constructor TTpAquisItem.Create;
begin
  inherited Create;
  FIdeProdutor := TIdeProdutorColecao.Create;
  FInfoProcJ   := TInfoProcJCollection.Create;
end;

destructor TTpAquisItem.destroy;
begin
  FIdeProdutor.Free;
  FInfoProcJ.Free;

  inherited;
end;

{ TIdeProdutorColecao }
function TIdeProdutorColecao.Add: TIdeProdutorItem;
begin
  Result := Self.New;
end;

function TIdeProdutorColecao.GetItem(Index: Integer): TIdeProdutorItem;
begin
  Result := TIdeProdutorItem(inherited Items[Index]);
end;

procedure TIdeProdutorColecao.SetItem(Index: Integer;
  const Value: TIdeProdutorItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeProdutorColecao.New: TIdeProdutorItem;
begin
  Result := TIdeProdutorItem.Create;
  Self.Add(Result);
end;

{ TIdeEstabAdquir }
constructor TIdeEstabAdquir.Create;
begin
  inherited;

  FTpAquis := TTpAquisColecao.Create;
end;

destructor TIdeEstabAdquir.Destroy;
begin
  FTpAquis.Free;

  inherited;
end;

{ TIdeProdutorItem }
constructor TIdeProdutorItem.Create;
begin
  inherited Create;

  FNfs         := TNfsColecao.Create;
  FInfoProcJud := TInfoProcJudCollection.Create;
end;

destructor TIdeProdutorItem.Destroy;
begin
  FNfs.Free;
  FInfoProcJud.Free;

  inherited;
end;

function TEvtAqProd.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtAqProd';
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

      sSecao := 'ideEstabAdquir';
      InfoAquisProd.IdeEstabAdquir.tpInscAdq := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscAdq', '1'));
      InfoAquisProd.IdeEstabAdquir.nrInscAdq := INIRec.ReadString(sSecao, 'nrInscAdq', '');

      I := 1;
      while true do
      begin
        // de 1 até 3
        sSecao := 'tpAquis' + IntToStrZero(I, 1);
        sFim   := INIRec.ReadString(sSecao, 'indAquis', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with InfoAquisProd.IdeEstabAdquir.tpAquis.New do
        begin
          indAquis    := eSStrToIdAquis(Ok, sFim);
          vlrTotAquis := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotAquis', ''), 0);

          J := 1;
          while true do
          begin
            // de 00001 até 14999
            sSecao := 'ideProdutor' + IntToStrZero(I, 1) + IntToStrZero(J, 5);
            sFim   := INIRec.ReadString(sSecao, 'tpInscProd', 'FIM');

            if (sFim = 'FIM') or (Length(sFim) <= 0) then
              break;

            with ideProdutor.New do
            begin
              tpInscProd  := eSStrToTpInscricao(Ok, sFim);
              nrInscProd  := INIRec.ReadString(sSecao, 'nrInscProd', EmptyStr);
              vlrBruto    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBruto', ''), 0);
              vrCPDescPR  := StringToFloatDef(INIRec.ReadString(sSecao, 'vrCPDescPR', ''), 0);
              vrRatDescPR := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRatDescPR', ''), 0);
              vrSenarDesc := StringToFloatDef(INIRec.ReadString(sSecao, 'vrSenarDesc', ''), 0);
              IndOpcCP    := eSStrToIndOpcCP(Ok, INIRec.ReadString(sSecao, 'indOpcCP', '0'));

              K := 1;
              while true do
              begin
                // de 0000 até 9999
                sSecao := 'nfs' + IntToStrZero(I, 1) + IntToStrZero(J, 5) +
                               IntToStrZero(K, 4);
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

              K := 1;
              while true do
              begin
                // de 00 até 10
                sSecao := 'infoProcJud' + IntToStrZero(I, 1) + IntToStrZero(J, 5) +
                               IntToStrZero(K, 2);
                sFim   := INIRec.ReadString(sSecao, 'nrProcJud', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                with infoProcJud.New do
                begin
                  nrProcJud   := sFim;
                  codSusp     := INIRec.ReadInteger(sSecao, 'codSusp', 0);
                  vrCPNRet    := StringToFloatDef(INIRec.ReadString(sSecao, 'vrCPNRet', ''), 0);
                  vrRatNRet   := StringToFloatDef(INIRec.ReadString(sSecao, 'vrRatNRet', ''), 0);
                  vrSenarNRet := StringToFloatDef(INIRec.ReadString(sSecao, 'vrSenarNRet', ''), 0);
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
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
