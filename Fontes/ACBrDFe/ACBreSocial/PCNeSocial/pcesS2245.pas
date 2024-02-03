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

unit pcesS2245;

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
  TS2245CollectionItem = class;
  TEvtTreiCap = class;
  TTreiCap = class;
  TInfoComplem = class;
  TIdeProfRespCollection = class;
  TIdeProfRespCollectionItem = class;

  TS2245Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2245CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2245CollectionItem);
  public
    function Add: TS2245CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2245CollectionItem;
    property Items[Index: Integer]: TS2245CollectionItem read GetItem write SetItem; default;
  end;

  TS2245CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTreiCap: TEvtTreiCap;
  public
    constructor Create(AOwner: TComponent);
    destructor  Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTreiCap: TEvtTreiCap read FEvtTreiCap write FEvtTreiCap;
  end;

  TEvtTreiCap = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FTreiCap: TTreiCap;

    { Geradores da classe }
    procedure GerarTreiCap(objTreiCap: TTreiCap);
    procedure GerarInfoComplem(objInfoComplem: TInfoComplem);
    procedure GerarIdeProfResp(objIdeProfResp: TIdeProfRespCollection);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property treiCap: TTreiCap read FTreiCap write FTreiCap;
  end;

  TTreiCap = class(TObject)
  private
    FcodTreiCap: String;
    FobsTreiCap: String;
    FInfoComplem: TInfoComplem;
  public
    constructor Create;
    destructor Destroy; override;

    property codTreiCap: String read FcodTreiCap write FcodTreiCap;
    property obsTreiCap: String read FobsTreiCap write FobsTreiCap;
    property infoComplem: TInfoComplem read FInfoComplem write FInfoComplem;
  end;

  TInfoComplem = class(TObject)
  private
    FdtTreiCap: TDateTime;
    FdurTreiCap: Double;
    FModTreiCap: tpModTreiCap;
    FTpTreiCap: tpTpTreiCap;
    FindTreinAnt: tpSimNao;
    FIdeProfResp: TIdeProfRespCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property dtTreiCap: TDateTime read FdtTreiCap write FdtTreiCap;
    property durTreiCap: Double read FdurTreiCap write FdurTreiCap;
    property modTreiCap: tpModTreiCap read FModTreiCap write FModTreiCap;
    property tpTreiCap: tpTpTreiCap read FTpTreiCap write FTpTreiCap;
    property indTreinAnt: tpSimNao read FindTreinAnt write FindTreinAnt;
    property ideProfResp: TIdeProfRespCollection read FIdeProfResp write FIdeProfResp;
  end;

  TIdeProfRespCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeProfRespCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeProfRespCollectionItem);
  public
    function Add: TIdeProfRespCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeProfRespCollectionItem;
    property Items[Index: Integer]: TIdeProfRespCollectionItem read GetItem write SetItem; default;
  end;

  TIdeProfRespCollectionItem = class(TObject)
  private
    FcpfProf: String;
    FNmProf: String;
    FTpProf: tpTpProf;
    FFormProf: String;
    FcodCBO: String;
    FnacProf: tpNacProf;
  public
    property cpfProf: string read FcpfProf write FcpfProf;
    property nmProf: string read FNmProf write FNmProf;
    property tpProf: tpTpProf read FTpProf write FTpProf;
    property formProf: string read FFormProf write FFormProf;
    property codCBO: string read FcodCBO write FcodCBO;
    property nacProf: tpNacProf read FnacProf write FnacProf;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2245CollectionItem }

constructor TS2245CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS2245;
  FEvtTreiCap := TEvtTreiCap.Create(AOwner);
end;

destructor TS2245CollectionItem.Destroy;
begin
  FEvtTreiCap.Free;

  inherited;
end;

{ TS2245Collection }

function TS2245Collection.Add: TS2245CollectionItem;
begin
  Result := Self.New;
end;

function TS2245Collection.GetItem(Index: Integer): TS2245CollectionItem;
begin
  Result := TS2245CollectionItem(inherited Items[Index]);
end;

procedure TS2245Collection.SetItem(Index: Integer; Value: TS2245CollectionItem);
begin
  inherited Items[Index] := Value;
end;

constructor TEvtTreiCap.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo    := TIdeVinculo.Create;
  FTreiCap       := TTreiCap.Create;
end;

destructor TEvtTreiCap.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FTreiCap.Free;

  inherited;
end;

procedure TEvtTreiCap.GerarIdeProfResp(objIdeProfResp: TIdeProfRespCollection);
var
  i: Integer;
begin
  for i := 0 to objIdeProfResp.Count - 1 do
  begin
    Gerador.wGrupo('ideProfResp');

    Gerador.wCampo(tcStr, '', 'cpfProf',  1,  11, 0, objIdeProfResp.Items[i].cpfProf);
    Gerador.wCampo(tcStr, '', 'nmProf',   1,   2, 1, objIdeProfResp.Items[i].nmProf);
    Gerador.wCampo(tcStr, '', 'tpProf',   1,   1, 1, tpTpProfToStr(objIdeProfResp.Items[i].tpProf));
    Gerador.wCampo(tcStr, '', 'formProf', 1, 255, 1, objIdeProfResp.Items[i].formProf);
    Gerador.wCampo(tcStr, '', 'codCBO',   1,   6, 1, objIdeProfResp.Items[i].codCBO);
    Gerador.wCampo(tcStr, '', 'nacProf',  1,   1, 1, tpNacProfToStr(objIdeProfResp.Items[i].nacProf));

    Gerador.wGrupo('/ideProfResp');
  end;

  if objIdeProfResp.Count > 99 then
    Gerador.wAlerta('', 'ideProfResp', 'Lista de Informações relativas ao profissional responsável pelo treinamento/capacitação/exercício simulado', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtTreiCap.GerarInfoComplem(objInfoComplem: TInfoComplem);
begin
  if (objInfoComplem.dtTreiCap > 0) then
  begin
    Gerador.wGrupo('infoComplem');

    Gerador.wCampo(tcDat, '', 'dtTreiCap',  10, 10, 1, objInfoComplem.dtTreiCap);
    Gerador.wCampo(tcDe2, '', 'durTreiCap',  1,  6, 1, objInfoComplem.durTreiCap);
    Gerador.wCampo(tcStr, '', 'modTreiCap',  1,  1, 1, tpModTreiCapToStr(objInfoComplem.modTreiCap));
    Gerador.wCampo(tcStr, '', 'tpTreiCap',   1,  1, 1, tpTpTreiCapToStr(objInfoComplem.tpTreiCap));
    Gerador.wCampo(tcStr, '', 'indTreinAnt', 1,  1, 1, eSSimNaoToStr(objInfoComplem.indTreinAnt));

    GerarIdeProfResp(objInfoComplem.ideProfResp);

    Gerador.wGrupo('/infoComplem');
  end;  
end;

procedure TEvtTreiCap.GerarTreiCap(objTreiCap: TTreiCap);
begin
  Gerador.wGrupo('treiCap');

  Gerador.wCampo(tcStr, '', 'codTreiCap', 1,   4, 1, objTreiCap.codTreiCap);
  Gerador.wCampo(tcStr, '', 'obsTreiCap', 1, 999, 0, objTreiCap.obsTreiCap);

  GerarInfoComplem(objTreiCap.infoComplem);

  Gerador.wGrupo('/treiCap');
end;

function TEvtTreiCap.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, Self.ideEmpregador.NrInsc, Self.Sequencial);

    GerarCabecalho('evtTreiCap');
    Gerador.wGrupo('evtTreiCap Id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(self.IdeVinculo);
    GerarTreiCap(Self.treiCap);

    Gerador.wGrupo('/evtTreiCap');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTreiCap');

//    Validar(schevtTreiCap);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtTreiCap.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtTreiCap';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
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
      IdeVinculo.codCateg  := INIRec.ReadInteger(sSecao, 'codCateg', 0);

      sSecao := 'treiCap';
      treiCap.codTreiCap := INIRec.ReadString(sSecao, 'codTreiCap', EmptyStr);
      treiCap.obsTreiCap := INIRec.ReadString(sSecao, 'obsTreiCap', EmptyStr);

      sSecao := 'infoComplem';
      if INIRec.ReadString(sSecao, 'dtTreiCap', '') <> '' then
      begin
        treiCap.infoComplem.dtTreiCap   := StringToDateTime(INIRec.ReadString(sSecao, 'dtTreiCap', '0'));
        treiCap.infoComplem.durTreiCap  := StringToFloatDef(INIRec.ReadString(sSecao, 'durTreiCap', EmptyStr), 0);
        treiCap.infoComplem.modTreiCap  := StrTotpModTreiCap(Ok, INIRec.ReadString(sSecao, 'modTreiCap', '1'));
        treiCap.infoComplem.tpTreiCap   := StrTotpTpTreiCap(Ok, INIRec.ReadString(sSecao, 'tpTreiCap', '1'));
        treiCap.infoComplem.indTreinAnt := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indTreinAnt', 'N'));
      end;

      I := 1;
      while true do
      begin
        // de 01 até 99
        sSecao := 'ideProfResp' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'nmProf', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with treiCap.infoComplem.ideProfResp.New do
        begin
          cpfProf  := INIRec.ReadString(sSecao, 'cpfProf', '');
          nmProf   := sFim;
          tpProf   := StrTotpTpProf(Ok, INIRec.ReadString(sSecao, 'tpProf', '1'));
          formProf := INIRec.ReadString(sSecao, 'formProf', '');
          codCBO   := INIRec.ReadString(sSecao, 'codCBO', '');
          nacProf  := StrTotpNacProf(Ok, INIRec.ReadString(sSecao, 'nacProf', '1'));
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

function TS2245Collection.New: TS2245CollectionItem;
begin
  Result := TS2245CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TIdeProfRespCollection }

function TIdeProfRespCollection.Add: TIdeProfRespCollectionItem;
begin
  Result := Self.New;
end;

function TIdeProfRespCollection.GetItem(Index: Integer): TIdeProfRespCollectionItem;
begin
  Result := TIdeProfRespCollectionItem(inherited Items[Index]);
end;

procedure TIdeProfRespCollection.SetItem(Index: Integer; Value: TIdeProfRespCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeProfRespCollection.New: TIdeProfRespCollectionItem;
begin
  Result := TIdeProfRespCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoComplem }

constructor TInfoComplem.Create;
begin
  inherited;

  FIdeProfResp := TIdeProfRespCollection.Create;
end;

destructor TInfoComplem.Destroy;
begin
  FIdeProfResp.Free;

  inherited;
end;

{ TTreiCap }

constructor TTreiCap.Create;
begin
  inherited;

  FInfoComplem := TInfoComplem.Create;
end;

destructor TTreiCap.Destroy;
begin
  FInfoComplem.Free;
  
  inherited;
end;

end.
