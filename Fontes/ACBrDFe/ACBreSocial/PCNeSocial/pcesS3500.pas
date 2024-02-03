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

unit pcesS3500;

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
  TS3500Collection = class;
  TS3500CollectionItem = class;
  TEvtExcProcTrab = class;
  TInfoExclusao = class;
  TIdeProcTrab = class;

  TS3500Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS3500CollectionItem;
    procedure SetItem(Index: Integer; Value: TS3500CollectionItem);
  public
    function New: TS3500CollectionItem;
    property Items[Index: Integer]: TS3500CollectionItem read GetItem write SetItem; default;
  end;

  TS3500CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtExcProcTrab: TEvtExcProcTrab;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtExcProcTrab: TEvtExcProcTrab read FEvtExcProcTrab write FEvtExcProcTrab;
  end;

  TEvtExcProcTrab = class(TESocialEvento)
  private
    FIdeEvento: TIdeEvento;
    FIdeEmpregador: TIdeEmpregador;
    FInfoExclusao: TInfoExclusao;
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
    FideProcTrab: TIdeProcTrab;
  public
    constructor Create;
    destructor Destroy; override;

    property tpEvento: TTipoEvento read FtpEvento write FtpEvento;
    property nrRecEvt: string read FnrRecEvt write FnrRecEvt;
    property ideProcTrab: TIdeProcTrab read FideProcTrab write FideProcTrab;
  end;

  TIdeProcTrab = class(TObject)
  private
    FcpfTrab: string;
    FnrProcTrab: string;
    FperApurPgto: string;
  public
    property cpfTrab: string read FcpfTrab write FcpfTrab;
    property nrProcTrab: string read FnrProcTrab write FnrProcTrab;
    property perApurPgto: string read FperApurPgto write FperApurPgto;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TS3500Collection }

function TS3500Collection.GetItem(Index: Integer): TS3500CollectionItem;
begin
  Result := TS3500CollectionItem(inherited Items[Index]);
end;

procedure TS3500Collection.SetItem(Index: Integer;
  Value: TS3500CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS3500Collection.New: TS3500CollectionItem;
begin
  Result := TS3500CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS3500CollectionItem }

constructor TS3500CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento     := teS3500;
  FEvtExcProcTrab := TEvtExcProcTrab.Create(AOwner);
end;

destructor TS3500CollectionItem.Destroy;
begin
  FEvtExcProcTrab.Free;

  inherited;
end;

{ TInfoExclusao }

constructor TInfoExclusao.Create;
begin
  inherited Create;

  FideProcTrab := TIdeProcTrab.Create;
end;

destructor TInfoExclusao.Destroy;
begin
  FideProcTrab.Free;

  inherited;
end;

{ TEvtExcProcTrab }

constructor TEvtExcProcTrab.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoExclusao  := TInfoExclusao.Create;
end;

destructor TEvtExcProcTrab.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoExclusao.Free;

  inherited;
end;

function TEvtExcProcTrab.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtExcProcTrab');
    Gerador.wGrupo('evtExcProcTrab Id="' + Self.Id + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoExclusao');

    Gerador.wCampo(tcStr, '', 'tpEvento'  , 1,  6, 1, TipoEventoToStr(self.InfoExclusao.tpEvento));
    Gerador.wCampo(tcStr, '', 'nrRecEvt'  , 1, 40, 1, self.InfoExclusao.nrRecEvt);

    Gerador.wGrupo('ideProcTrab');

    Gerador.wCampo(tcStr, '', 'nrProcTrab' ,  20, 20, 1, self.InfoExclusao.ideProcTrab.nrProcTrab);

    if self.InfoExclusao.tpEvento = teS2500 then
      Gerador.wCampo(tcStr, '', 'cpfTrab'    ,  11, 11, 1, self.InfoExclusao.ideProcTrab.cpfTrab)
    else if self.InfoExclusao.tpEvento = teS2501 then
      Gerador.wCampo(tcStr, '', 'perApurPgto',   7,  7, 1, self.InfoExclusao.ideProcTrab.perApurPgto);

    Gerador.wGrupo('/ideProcTrab');
    Gerador.wGrupo('/infoExclusao');
    Gerador.wGrupo('/evtExcProcTrab');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtExcProcTrab');

//    Validar(schevtExcProcTrab);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtExcProcTrab.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtExcProcTrab';
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

      sSecao := 'ideProcTrab';
      infoExclusao.ideProcTrab.cpfTrab     := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      infoExclusao.ideProcTrab.nrProcTrab  := INIRec.ReadString(sSecao, 'nrProcTrab', EmptyStr);
      infoExclusao.ideProcTrab.perApurPgto := INIRec.ReadString(sSecao, 'perApurPgto', EmptyStr);
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
