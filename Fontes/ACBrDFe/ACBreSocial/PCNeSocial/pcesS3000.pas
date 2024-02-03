{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pcesS3000;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$ELSE}
   Contnrs,
  {$IFEND}
  ACBrBase, pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS3000Collection = class;
  TS3000CollectionItem = class;
  TEvtExclusao = class;
  TInfoExclusao = class;

  TS3000Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS3000CollectionItem;
    procedure SetItem(Index: Integer; Value: TS3000CollectionItem);
  public
    function Add: TS3000CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS3000CollectionItem;
    property Items[Index: Integer]: TS3000CollectionItem read GetItem write SetItem; default;
  end;

  TS3000CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtExclusao: TEvtExclusao;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtExclusao: TEvtExclusao read FEvtExclusao write FEvtExclusao;
  end;

  TEvtExclusao = class(TESocialEvento)
  private
    FIdeEvento: TIdeEvento;
    FIdeEmpregador: TIdeEmpregador;
    FInfoExclusao: TInfoExclusao;
    {Geradores específicos da classe}
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property InfoExclusao: TInfoExclusao read FInfoExclusao write FInfoExclusao;
  end;

  TInfoExclusao = class(TObject)
  private
    FtpEvento: TTipoEvento;
    FnrRecEvt: string;
    FIdeTrabalhador: TideTrabalhador2;
    FIdeFolhaPagto: TIdeFolhaPagto;
  public
    constructor Create;
    destructor Destroy; override;

    property tpEvento: TTipoEvento read FtpEvento write FtpEvento;
    property nrRecEvt: string read FnrRecEvt write FnrRecEvt;
    property IdeTrabalhador: TideTrabalhador2 read FIdeTrabalhador write FIdeTrabalhador;
    property IdeFolhaPagto: TIdeFolhaPagto read FIdeFolhaPagto write FIdeFolhaPagto;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TS3000Collection }

function TS3000Collection.Add: TS3000CollectionItem;
begin
  Result := Self.New;
end;

function TS3000Collection.GetItem(Index: Integer): TS3000CollectionItem;
begin
  Result := TS3000CollectionItem(inherited Items[Index]);
end;

procedure TS3000Collection.SetItem(Index: Integer;
  Value: TS3000CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS3000Collection.New: TS3000CollectionItem;
begin
  Result := TS3000CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS3000CollectionItem }

constructor TS3000CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento  := teS3000;
  FEvtExclusao := TEvtExclusao.Create(AOwner);
end;

destructor TS3000CollectionItem.Destroy;
begin
  FEvtExclusao.Free;

  inherited;
end;

{ TInfoExclusao }

constructor TInfoExclusao.Create;
begin
  inherited;

  FIdeTrabalhador := TideTrabalhador2.Create;
  FIdeFolhaPagto  := TIdeFolhaPagto.Create;
end;

destructor TInfoExclusao.Destroy;
begin
  FIdeTrabalhador.Free;
  FIdeFolhaPagto.Free;

  inherited;
end;

{ TEvtExclusao }

constructor TEvtExclusao.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoExclusao  := TInfoExclusao.Create;
end;

destructor TEvtExclusao.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoExclusao.Free;

  inherited;
end;

function TEvtExclusao.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtExclusao');
    Gerador.wGrupo('evtExclusao Id="' + Self.Id + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoExclusao');

    Gerador.wCampo(tcStr, '', 'tpEvento', 1,  6, 1, TipoEventoToStr(self.InfoExclusao.tpEvento));
    Gerador.wCampo(tcStr, '', 'nrRecEvt', 1, 40, 1, self.InfoExclusao.nrRecEvt);

    if(self.InfoExclusao.tpEvento In [teS1200..teS1210, teS2190..teS2420, teS8299])then
      GerarIdeTrabalhador(self.InfoExclusao.IdeTrabalhador, True);

    if(self.InfoExclusao.tpEvento In [teS1200, teS1202, teS1207, teS1280, teS1300])then
      GerarIdeFolhaPagto(self.InfoExclusao.IdeFolhaPagto)
    else if(self.InfoExclusao.tpEvento In [teS1200..teS1280,teS1300])then
      GerarIdeFolhaPagto2(self.InfoExclusao.IdeFolhaPagto);

    Gerador.wGrupo('/infoExclusao');
    Gerador.wGrupo('/evtExclusao');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtExclusao');

//    Validar(schevtExclusao);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TEvtExclusao.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao: String;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtExclusao';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'infoExclusao';
      infoExclusao.tpEvento := StrToTipoEvento(Ok, INIRec.ReadString(sSecao, 'tpEvento', EmptyStr));
      infoExclusao.nrRecEvt := INIRec.ReadString(sSecao, 'nrRecEvt', EmptyStr);

      sSecao := 'ideTrabalhador';
      if INIRec.ReadString(sSecao, 'cpfTrab', '') <> '' then
      begin
        infoExclusao.ideTrabalhador.cpfTrab := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
        infoExclusao.ideTrabalhador.nisTrab := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);
      end;

      sSecao := 'ideFolhaPagto';
      if INIRec.ReadString(sSecao, 'indApuracao', '') <> '' then
      begin
        infoExclusao.ideFolhaPagto.indApuracao := eSStrToIndApuracao(Ok, INIRec.ReadString(sSecao, 'indApuracao', '1'));
        infoExclusao.ideFolhaPagto.perApur     := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      end;
      if (TipoEventoToStr(infoExclusao.tpEvento) = 'S-1210') then infoExclusao.ideFolhaPagto.perApur := INIRec.ReadString(sSecao, 'perApur', EmptyStr);  //27/05/2021
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
